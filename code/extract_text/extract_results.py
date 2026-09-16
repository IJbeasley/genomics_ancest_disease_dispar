#!/usr/bin/env python3
"""
Extract the results section from JATS, TEI (GROBID), and BioC XML files.

Companion to extract_methods.py — same CLI, same output conventions, same
text-cleaning pipeline (the cleaning helpers are imported from
extract_methods so methods and results text stay byte-for-byte comparable
downstream).

Supported formats:
  - JATS XML: PubMed Central / publisher format (<sec sec-type="results">)
  - TEI XML:  GROBID output from PDFs (.pdf.tei.xml), <div> with <head>Results</head>
  - BioC XML: Auto-CORPus/BioC format (_bioc.xml), <infon key="section_title_1">Results</infon>

Usage:
    python3 extract_results.py input.xml -o output_results.txt
    python3 extract_results.py input.xml            # prints to stdout
"""

import xml.etree.ElementTree as ET
import argparse
import sys
import re
from pathlib import Path

# Shared helpers live in extract_methods.py (same directory).  Python puts
# the script's own directory on sys.path, so a plain import works whenever
# this file is run as a script or imported from this folder.
from extract_methods import (
    detect_xml_format,
    clean_extracted_text,
    extract_text_from_element,
    flatten_without_citations,
    extract_bioc_main,
    _is_main_journal,
    _is_letter_to_editor,
)


# ---------------------------------------------------------------------------
# Section-title vocabulary
# ---------------------------------------------------------------------------

# Section titles that mark the END of the results section.
NON_RESULTS_SECTIONS = {
    'discussion', 'discussions', 'conclusion', 'conclusions',
    'method', 'methods', 'materials and methods', 'methods and materials',
    'subjects and methods', 'patients and methods', 'participants and methods',
    'experimental procedures', 'experimental section', 'online methods',
    'research design and methods', 'study design',
    'acknowledgment', 'acknowledgments', 'acknowledgement', 'acknowledgements',
    'references', 'bibliography', 'funding', 'competing interests',
    'conflict of interest', 'conflicts of interest', 'disclosure', 'disclosures',
    'data availability', 'code availability', 'author contributions',
    'authors contributions', 'supplementary', 'supplemental', 'appendix',
    'abbreviations', 'ethics approval', 'ethics declarations',
    'web resources', 'urls', 'limitations',
}

# Titles that are never a results section, used to veto fuzzy matches.
NON_RESULTS_PREFIXES = (
    'abstract', 'introduction', 'background', 'discussion', 'conclusion',
    'method', 'material', 'subject', 'patient', 'participant',
    'acknowledg', 'reference', 'bibliograph', 'funding', 'disclosure',
    'conflict', 'competing', 'data availability', 'code availability',
    'supplement', 'appendix', 'author', 'ethic', 'limitation',
    'web resource', 'url',
)

# Titles that count as a results head even without the word "results".
RESULTS_SYNONYM_TITLES = {
    'findings', 'main findings', 'principal findings', 'key findings',
    'primary findings', 'study findings', 'observations',
}

# Fallback keywords for GROBID output where the section hierarchy has been
# flattened and the top-level "Results" head was dropped, leaving only the
# result subheadings (e.g. "Single variant association analysis").  Kept
# deliberately narrow — anything methods-flavoured is vetoed above.
RESULTS_SUBSECTION_KEYWORDS = (
    'result', 'finding',
    'association analysis', 'association analyses', 'association testing',
    'single variant association', 'gene-based association',
    'genome-wide association results', 'replication in',
    'replication cohort results', 'replication analysis',
    'enrichment analysis', 'phewas', 'phenome-wide association',
    'heritability estimate', 'meta-analysis result',
    'characteristics of participants', 'clinical characterization',
    'descriptive characteristics', 'baseline characteristics',
    'novel loci', 'identification of',
)


def _strip_numbering(title):
    """Drop leading section numbering such as '3.' or '2.1 ' from a title."""
    return re.sub(r'^\s*(?:\d+\.)*\d+\.?\s+', '', title.strip()).strip()


def _is_results_title(title, max_words=6):
    """
    True when a section title names a results section.

    Accepts 'Results', 'RESULTS', '3. Results', 'Results and Discussion',
    'Genome-wide association results', 'Findings', etc.  Rejects headings
    that clearly belong to another section, and rejects long descriptive
    headings (which are usually subsections, not the section head).
    """
    if not title:
        return False

    t = _strip_numbering(title).lower().rstrip(':.')
    if not t:
        return False

    if t.startswith(NON_RESULTS_PREFIXES):
        return False

    if t in RESULTS_SYNONYM_TITLES:
        return True

    if 'result' in t:
        return len(t.split()) <= max_words

    return False


def _is_non_results_title(title):
    """True when a title marks the start of a section after the results."""
    if not title:
        return False
    t = _strip_numbering(title).lower().rstrip(':.')
    if t in NON_RESULTS_SECTIONS:
        return True
    return any(t.startswith(stop + ' ') for stop in NON_RESULTS_SECTIONS)


# Heads that name a methods (sub)section — used to anchor the fallbacks that
# recover results from documents where the results head itself is missing.
METHODS_HEAD_PREFIXES = (
    'method', 'material', 'patients and', 'subjects and', 'participants and',
    'study population', 'study subjects', 'study participants', 'study design',
    'study cohort', 'cohort description', 'genotyping', 'quality control',
    'imputation', 'statistical analys', 'statistical method',
    'sample collection', 'data collection', 'dna extraction',
    'phenotype definition', 'ethic', 'experimental procedure',
)

_METHODS_HEAD_EXACT = {'patients', 'subjects', 'participants', 'study populations'}


def _is_methods_title(title):
    """True when a section title names a methods section or subsection."""
    if not title:
        return False
    t = _strip_numbering(title).lower().rstrip(':.')
    if not t or _is_results_title(t):
        return False
    return t in _METHODS_HEAD_EXACT or t.startswith(METHODS_HEAD_PREFIXES)


# ---------------------------------------------------------------------------
# Back-matter truncation (whole-body / "Main" fallbacks only)
# ---------------------------------------------------------------------------

# Headings that mark the start of back matter in a whole-body extraction.
# Matched case-sensitively in title case: the lowercase words appear in
# ordinary prose ("see references therein"), the headings do not.
_BODY_TAIL_HEADINGS = (
    r'References',
    r'Bibliography',
    r'Literature\s+Cited',
    r'Footnotes?',
    r'Acknowledge?ments?',
    r'Competing\s+(?:financial\s+)?interests',
    r'Conflicts?\s+of\s+interest',
    r'Author\s+[Cc]ontributions?',
    r'Disclosures?',
    r'Data\s+[Aa]vailability',
    r'Code\s+[Aa]vailability',
)

# A heading counts only at a sentence boundary and only when followed by
# punctuation and then more text — "... interests. References. Nathan DM, ..."
_BODY_TAIL_RE = re.compile(
    r'(?:(?<=\.)|(?<=\?)|(?<=!))\s+(?:' + '|'.join(_BODY_TAIL_HEADINGS) + r')\s*[.:]\s+'
)

# Back matter lives at the end of a document.  Refusing to cut in the first
# part of the text keeps an in-text mention from truncating real results.
_BODY_TAIL_MIN_FRACTION = 0.4


def _truncate_body_tail(text, source=''):
    """
    Drop back matter from a whole-body extraction.

    The `_main` fallbacks take the entire <body> (or every "Main" passage),
    which in some publisher XML runs past the end of the narrative and into
    footnotes, competing-interest statements and the full reference list.
    Explicitly tagged results sections end at their own section boundary and
    never need this, so callers apply it only to `is_main` text.

    Returns the text cut at the earliest back-matter heading that falls in the
    last 60% of the document, or the text unchanged when there is none.
    """
    if not text:
        return text

    cut = None
    threshold = len(text) * _BODY_TAIL_MIN_FRACTION
    for match in _BODY_TAIL_RE.finditer(text):
        if match.start() >= threshold:
            cut = match
            break

    if cut is None:
        return text

    truncated = text[:cut.start()].rstrip()
    dropped_words = len(text[cut.start():].split())
    label = f" in {source}" if source else ''
    print(
        f"  dropped {dropped_words} words of back matter{label} "
        f"(cut at '{cut.group().strip()}')",
        file=sys.stderr,
    )
    return truncated


# ---------------------------------------------------------------------------
# JATS XML extraction
# ---------------------------------------------------------------------------

# Sections that are tagged as results but are really front/back matter.
_JATS_SKIP_TITLE_KEYWORDS = (
    'analysis team', 'author', 'contributor', 'writing group',
    'study group', 'consortium', 'working group', 'steering committee',
    'acknowledgment', 'funding', 'competing interest', 'conflict of interest',
)


def _abstract_element_ids(root):
    """Ids of every element that sits inside an <abstract>."""
    ids = set()
    for elem in root.iter():
        if elem.tag.endswith('abstract'):
            for descendant in elem.iter():
                ids.add(id(descendant))
    return ids


def _section_title(sec):
    """First <title> text of a section, or '' when it has none."""
    for child in sec:
        if child.tag.endswith('title'):
            return ''.join(child.itertext()).strip()
    return ''


def _is_front_or_back_matter(sec):
    title = _section_title(sec).lower()
    return any(kw in title for kw in _JATS_SKIP_TITLE_KEYWORDS)


def find_all_results_sections(root):
    """
    Find all top-level results sections in a JATS document, in document order.

    Three passes, most reliable signal first:
      1. sec-type="results"
      2. sec-type containing 'result' (e.g. "methods|results", "results|discussion")
      3. a <title> that reads as a results heading

    Sections inside an <abstract>, sections nested inside an already-selected
    section, and author/funding sections mis-tagged as results are excluded.
    """
    in_abstract = _abstract_element_ids(root)
    doc_order = {id(elem): i for i, elem in enumerate(root.iter())}

    candidates = []          # list of (element, priority)
    selected_ids = set()     # ids of selected sections and all their descendants

    def accept(sec, priority):
        if id(sec) in in_abstract or id(sec) in selected_ids:
            return
        if _is_front_or_back_matter(sec):
            return
        candidates.append((sec, priority))
        for descendant in sec.iter():
            selected_ids.add(id(descendant))

    all_secs = [e for e in root.iter() if e.tag.endswith('sec')]

    # Pass 1 — sec-type="results"
    for sec in all_secs:
        sec_type = (sec.get('sec-type') or '').strip().lower()
        if sec_type == 'results':
            accept(sec, 1)

    # Pass 2 — sec-type mentioning results in a compound value
    for sec in all_secs:
        sec_type = (sec.get('sec-type') or '').strip().lower()
        if sec_type and sec_type != 'results' and 'result' in sec_type:
            accept(sec, 2)

    # Pass 3 — title-based detection
    for sec in all_secs:
        if _is_results_title(_section_title(sec)):
            accept(sec, 3)

    candidates.sort(key=lambda pair: doc_order.get(id(pair[0]), 0))
    return [sec for sec, _ in candidates]


def _jats_untitled_results(root):
    """
    Recover results from Nature-style articles whose results sections carry
    finding-specific titles ("Discovery and fine-mapping of T1D loci") instead
    of a "Results" heading, with METHODS placed after them.

    Strategy: take the top-level body sections that come before the first
    methods section, dropping the introduction and stopping at any
    discussion/conclusion section.  Returns [] when the layout does not match.
    """
    body = root.find('.//{*}body')
    if body is None:
        return []

    top_secs = [s for s in list(body) if s.tag.endswith('sec')]
    if not top_secs:
        return []

    methods_idx = None
    for i, sec in enumerate(top_secs):
        sec_type = (sec.get('sec-type') or '').lower()
        if 'method' in sec_type or _is_methods_title(_section_title(sec)):
            methods_idx = i
            break

    # Nothing to anchor on, or the conventional Intro/Methods/Results order
    # (in which case anything before methods is introduction, not results).
    if not methods_idx:
        return []

    out = []
    for sec in top_secs[:methods_idx]:
        sec_type = (sec.get('sec-type') or '').lower()
        title = _section_title(sec)
        low = _strip_numbering(title).lower()

        if sec_type in ('intro', 'introduction', 'abstract') or \
                low.startswith(('introduction', 'background', 'abstract')):
            continue
        if _is_non_results_title(title):
            break
        if sec_type in ('supplementary-material', 'extended-data', 'ack',
                        'COI-statement'.lower(), 'data-availability'):
            continue
        if _is_front_or_back_matter(sec):
            continue
        out.append(sec)

    return out


def find_results_sections(root):
    """
    Return (sections, is_main) for a JATS document.

    `sections` is a list of results sections in document order — results are
    more often split across several sibling sections than methods are, so all
    top-level matches are kept and concatenated.

    `is_main` is True when no results section exists and the whole <body> is
    used instead: Nature Genetics / AJHG-style articles where the narrative
    runs under a single "Main" heading, and letters to the editor.
    """
    sections = find_all_results_sections(root)
    if sections:
        return sections, False

    # Untitled Nature-style results sections placed before METHODS.
    sections = _jats_untitled_results(root)
    if sections:
        return sections, False

    if _is_main_journal(root) or _is_letter_to_editor(root):
        body = root.find('.//{*}body')
        if body is not None:
            return [body], True

    return [], False


# ---------------------------------------------------------------------------
# TEI XML extraction (GROBID .pdf.tei.xml files)
# ---------------------------------------------------------------------------

TEI_NS = {'tei': 'http://www.tei-c.org/ns/1.0'}


def _tei_find(parent, name):
    """Find a child by TEI-namespaced name, falling back to no namespace."""
    found = parent.find('tei:' + name, TEI_NS)
    if found is None:
        found = parent.find(name)
    return found


def _tei_findall(parent, name):
    found = parent.findall('tei:' + name, TEI_NS)
    if not found:
        found = parent.findall(name)
    return found


def extract_tei_results(root):
    """
    Extract the results section from a TEI (GROBID) document.

    GROBID emits a flat list of sibling <div> elements under <body>.  The
    Results <div> often holds only a <head>; the prose lives in the divs that
    follow, up to the next major section (Discussion, Methods, References...).
    """
    body = _tei_find(root, 'body')
    if body is None:
        body = root.find('.//tei:body', TEI_NS)
    if body is None:
        body = root.find('.//body')
    if body is None:
        return None

    divs = _tei_findall(body, 'div')
    if not divs:
        return None

    def head_text(div):
        head = _tei_find(div, 'head')
        if head is not None and head.text:
            return head.text.strip()
        return ''

    # Primary pass: an explicit results head.
    results_start = None
    for i, div in enumerate(divs):
        if _is_results_title(head_text(div)):
            results_start = i
            break

    # Fallback: hierarchy flattened by GROBID, only subheadings survive.
    if results_start is None:
        for i, div in enumerate(divs):
            title = _strip_numbering(head_text(div)).lower()
            if not title or title.startswith(NON_RESULTS_PREFIXES):
                continue
            if any(kw in title for kw in RESULTS_SUBSECTION_KEYWORDS):
                results_start = i
                break

    if results_start is None:
        return None

    text_parts = []
    for i, div in enumerate(divs[results_start:], start=results_start):
        title = head_text(div)

        if i > results_start and _is_non_results_title(title):
            break

        clean_title = _strip_numbering(title).rstrip(' .;,:')
        if clean_title:
            text_parts.append(clean_title + '. ')

        for p in _tei_findall(div, 'p'):
            # Not itertext(): GROBID keeps <ref type="bibr">[1]</ref> inline,
            # and itertext() would pull those bracketed numbers into the prose.
            para_text = flatten_without_citations(p).strip()
            if para_text:
                text_parts.append(clean_extracted_text(para_text) + ' ')

    result = clean_extracted_text(' '.join(text_parts))
    return result if result else None


# ---------------------------------------------------------------------------
# BioC XML extraction (Auto-CORPus _bioc.xml files)
# ---------------------------------------------------------------------------

_RESULTS_HEADER_RE = re.compile(
    r'^(Results?(?:\s+and\s+Discussion)?'
    r'|Findings'
    r'|Principal\s+Findings)'
    r'\s*[:.]?\s',
    re.IGNORECASE,
)

_NON_RESULTS_HEADER_RE = re.compile(
    r'^(Discussion'
    r'|Conclusions?'
    r'|(?:Materials?\s+and\s+)?Methods?'
    r'|Subjects?\s+and\s+Methods?'
    r'|Patients?\s+and\s+Methods?'
    r'|Acknowledg(?:e)?ments?'
    r'|References'
    r'|Funding'
    r'|Data\s+Availability'
    r'|Author\s+Contributions?'
    r'|Supplementary)'
    r'\s*[:.]?\s',
    re.IGNORECASE,
)


def _passage_section_title(passage, level=1):
    key = f'section_title_{level}'
    for infon in passage.findall('infon'):
        if infon.get('key') == key:
            return (infon.text or '').strip()
    return None


def _passage_text(passage):
    text_elem = passage.find('text')
    if text_elem is not None and text_elem.text:
        return text_elem.text.strip()
    return ''


def _bioc_subsection_results(root):
    """
    Recover results from BioC files whose section_title_1 is uninformative
    ("document part", "Author notes") but whose section_title_2 carries the
    real headings.

    Two passes: an explicit results heading in section_title_2, then — failing
    that — everything between the methods block and the discussion, which is
    where the results sit in an Intro/Methods/Results/Discussion body.
    """
    passages = list(root.iter('passage'))
    heads = [_passage_section_title(p, 2) or '' for p in passages]
    if not any(heads):
        return None

    # Only step down to section_title_2 when section_title_1 is genuinely
    # uninformative.  If the file has real top-level labels (Main, Methods,
    # Discussion, ...) the structure is intact and the other strategies —
    # which respect it — must win.
    top_labels = {
        _strip_numbering(_passage_section_title(p, 1) or '').lower()
        for p in passages
    }
    STRUCTURAL = ('main', 'method', 'result', 'introduction', 'discussion',
                  'conclusion', 'findings')
    if any(lbl.startswith(STRUCTURAL) for lbl in top_labels if lbl):
        return None

    # Pass 1 — an explicit "Results" subheading.
    parts = [
        clean_extracted_text(_passage_text(p)) + ' '
        for p, h in zip(passages, heads)
        if _is_results_title(h) and _passage_text(p)
    ]
    if parts:
        return clean_extracted_text(' '.join(parts))

    # Pass 2 — anchor on the methods block, collect until the discussion.
    methods_seen = False
    parts = []
    for passage, head in zip(passages, heads):
        if not head:
            continue
        if _is_methods_title(head):
            methods_seen = True
            continue
        if not methods_seen:
            continue
        if _is_non_results_title(head):
            break
        text = _passage_text(passage)
        if text:
            parts.append(clean_extracted_text(text) + ' ')

    result = clean_extracted_text(' '.join(parts))
    return result if result else None


def extract_bioc_results(root):
    """
    Extract the results section from a BioC document.

    Returns (text, is_main).  `is_main` is True when the file has no results
    passages but does have Nature-style "Main" passages, which carry the
    results narrative.
    """
    # Primary strategy: section_title_1 labels.
    text_parts = []
    for passage in root.iter('passage'):
        title = _passage_section_title(passage)
        if title is None:
            continue
        if _is_results_title(title):
            para_text = _passage_text(passage)
            if para_text:
                text_parts.append(clean_extracted_text(para_text) + ' ')

    if text_parts:
        return clean_extracted_text(' '.join(text_parts)), False

    # Some publishers (Taylor & Francis, parts of Oxford Academic) file the
    # whole body under a single meaningless section_title_1 such as
    # "document part" and put the real headings in section_title_2.
    text_parts = _bioc_subsection_results(root)
    if text_parts:
        return text_parts, False

    # Nature-style files: the whole narrative sits under "Main".
    for passage in root.iter('passage'):
        title = _passage_section_title(passage)
        if title and title.strip().lower() == 'main':
            main_text = extract_bioc_main(root)
            if main_text:
                return main_text, True
            break

    # Fallback: broken/missing labels — scan passage text for inline headers.
    return _bioc_fallback_inline_headers(root), False


def _bioc_fallback_inline_headers(root):
    """
    Fallback for BioC files whose section labels are wrong or missing (common
    in Oxford Academic output, where the whole body lands under "Author notes"
    or "document part").  Finds a passage that opens with an inline "Results"
    header and collects until the next major section header.
    """
    passages = list(root.iter('passage'))

    results_start = None
    for i, passage in enumerate(passages):
        text = _passage_text(passage)
        if text and _RESULTS_HEADER_RE.match(text):
            results_start = i
            break

    if results_start is None:
        return None

    text_parts = []
    for i, passage in enumerate(passages[results_start:], start=results_start):
        text = _passage_text(passage)
        if not text:
            continue

        if i > results_start and _NON_RESULTS_HEADER_RE.match(text):
            break

        if i == results_start:
            text = _RESULTS_HEADER_RE.sub('', text, count=1).strip()

        if text:
            text_parts.append(clean_extracted_text(text) + ' ')

    result = clean_extracted_text(' '.join(text_parts))
    return result if result else None


# ---------------------------------------------------------------------------
# XML parsing with recovery for malformed publisher XML
# ---------------------------------------------------------------------------

_DEFAULT_NS_URIS = {
    'xlink': 'http://www.w3.org/1999/xlink',
    'mml':   'http://www.w3.org/1998/Math/MathML',
    'oasis': 'http://docs.oasis-open.org/ns/oasis-exchange/table',
    'ali':   'http://www.niso.org/schemas/ali/1.0/',
}


def parse_xml_with_recovery(xml_file):
    """
    Parse an XML file, repairing the malformed markup some publishers ship.

    SAGE/Atypon JATS, for instance, uses namespace prefixes (<oasis:table>,
    xlink:href) that are never declared on the root element, which strict
    ElementTree refuses.  Recovery: re-declare every used prefix on the root,
    re-parse, and if that still fails fall back to lxml's recovering parser
    with namespaces stripped.
    """
    try:
        return ET.parse(xml_file).getroot()
    except ET.ParseError as parse_err:
        print(
            f"Warning: ElementTree could not parse {xml_file} ({parse_err}); "
            f"attempting to repair undeclared namespace prefixes",
            file=sys.stderr,
        )

    raw = Path(xml_file).read_text(encoding='utf-8')

    elem_prefixes = set(re.findall(r'<\s*([A-Za-z][\w.-]*):', raw))
    attr_prefixes = set(re.findall(r'\s([A-Za-z][\w.-]*):[A-Za-z][\w.-]*\s*=', raw))
    used_prefixes = (elem_prefixes | attr_prefixes) - {'xml', 'xmlns'}

    declared_uris = dict(
        re.findall(r'xmlns:([A-Za-z][\w.-]*)\s*=\s*"([^"]*)"', raw)
    )

    patched = raw
    if used_prefixes:
        injection = ''.join(
            ' xmlns:{}="{}"'.format(
                p,
                declared_uris.get(p) or _DEFAULT_NS_URIS.get(p) or f'urn:local:undeclared:{p}',
            )
            for p in sorted(used_prefixes)
        )

        def _patch_root(m):
            head, tail = m.group(1), m.group(2)
            for p in used_prefixes:
                head = re.sub(rf'\s+xmlns:{re.escape(p)}\s*=\s*"[^"]*"', '', head)
            return head + injection + tail

        patched, n = re.subn(
            r'(<[A-Za-z][\w.-]*\b[^>]*?)(\s*/?>)', _patch_root, patched, count=1
        )
        if n:
            print(f"  injected xmlns declarations for: {sorted(used_prefixes)}",
                  file=sys.stderr)

    try:
        return ET.fromstring(patched)
    except ET.ParseError as second_err:
        try:
            from lxml import etree as LET
        except ImportError:
            raise second_err
        print(
            f"  namespace injection insufficient ({second_err}); "
            f"falling back to lxml recover=True with namespace strip",
            file=sys.stderr,
        )
        lxml_root = LET.fromstring(
            raw.encode('utf-8'), LET.XMLParser(recover=True, huge_tree=True)
        )
        for elem in lxml_root.iter():
            if isinstance(elem.tag, str) and '}' in elem.tag:
                elem.tag = elem.tag.split('}', 1)[1]
        LET.cleanup_namespaces(lxml_root)
        return ET.fromstring(LET.tostring(lxml_root))


# ---------------------------------------------------------------------------
# Main extraction entry point
# ---------------------------------------------------------------------------

def extract_results_section(xml_file):
    """
    Extract the results section text from a JATS, TEI, or BioC XML file.

    Returns a dict with:
      'text'    — the results text, or None when no results section was found
      'is_main' — True when the text is a whole-body / "Main" fallback rather
                  than an explicitly tagged results section
    """
    try:
        root = parse_xml_with_recovery(xml_file)
    except ET.ParseError as e:
        print(f"Error parsing XML file: {e}", file=sys.stderr)
        return {'text': None, 'is_main': False}
    except Exception as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        return {'text': None, 'is_main': False}

    try:
        fmt = detect_xml_format(root)

        if fmt == 'tei':
            return {'text': extract_tei_results(root), 'is_main': False}

        if fmt == 'bioc':
            text, is_main = extract_bioc_results(root)
            if is_main:
                text = _truncate_body_tail(text, Path(xml_file).name)
            return {'text': text, 'is_main': is_main}

        if fmt == 'unknown':
            print(f"Warning: could not detect XML format for {xml_file}, trying JATS",
                  file=sys.stderr)

        # JATS
        sections, is_main = find_results_sections(root)
        if not sections:
            return {'text': None, 'is_main': False}

        parts = []
        for sec in sections:
            sec_text = extract_text_from_element(sec).strip()
            if sec_text:
                parts.append(sec_text)

        results_text = clean_extracted_text(' '.join(parts))

        if not results_text:
            return {'text': None, 'is_main': False}

        # Guard against pointer stubs ("Results are available online at ...").
        if len(results_text.split()) < 50:
            lower = results_text.lower()
            stub_indicators = (
                'available in the online version',
                'available at http',
                'available at https',
                'available at 10.',
                'available at doi',
                'online content',
            )
            if any(ind in lower for ind in stub_indicators):
                if _is_main_journal(root):
                    body = root.find('.//{*}body')
                    if body is not None:
                        body_text = extract_text_from_element(body).strip()
                        if body_text and len(body_text.split()) >= 50:
                            body_text = _truncate_body_tail(
                                body_text, Path(xml_file).name)
                            return {'text': body_text, 'is_main': True}
                print("Results are only available online (not extracted).", file=sys.stderr)
                return {'text': None, 'is_main': False}

        if is_main:
            results_text = _truncate_body_tail(results_text, Path(xml_file).name)

        return {'text': results_text, 'is_main': is_main}

    except Exception as e:
        print(f"Error processing file: {e}", file=sys.stderr)
        return {'text': None, 'is_main': False}


def main():
    parser = argparse.ArgumentParser(
        description='Extract the results section from JATS, TEI, or BioC XML files'
    )
    parser.add_argument('input_file', help='Path to the input XML file')
    parser.add_argument(
        '-o', '--output',
        help='Path to the output text file (prints to stdout if omitted)'
    )
    args = parser.parse_args()

    result = extract_results_section(args.input_file)
    results_text = result['text']

    if results_text is None:
        print("No results section found in the XML file.", file=sys.stderr)
        sys.exit(1)

    if args.output:
        output_path = Path(args.output)
        # Whole-body fallback gets a '_main' marker, matching extract_methods.py
        if result['is_main']:
            output_path = output_path.parent / f"{output_path.stem}_main{output_path.suffix}"
        output_path.write_text(results_text, encoding='utf-8')
        print(f"Results section extracted to: {output_path}")
    else:
        print(results_text)


if __name__ == '__main__':
    main()
