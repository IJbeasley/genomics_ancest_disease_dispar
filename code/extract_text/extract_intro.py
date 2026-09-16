#!/usr/bin/env python3
"""
Extract the introduction section from JATS, TEI (GROBID), and BioC XML files.

Companion to extract_methods.py and extract_results.py — same CLI, same output
conventions, same text-cleaning pipeline (the cleaning helpers are imported
from extract_methods so methods, results and introduction text stay
byte-for-byte comparable downstream).

Supported formats:
  - JATS XML: PubMed Central / publisher format (<sec sec-type="intro">)
  - TEI XML:  GROBID output from PDFs (.pdf.tei.xml), <div> with <head>Introduction</head>
  - BioC XML: Auto-CORPus/BioC format (_bioc.xml), <infon key="section_title_1">Introduction</infon>

Two things make the introduction harder to scope than methods or results, and
both are handled explicitly below:

  1. It is frequently UNTITLED.  Many publishers open <body> with bare <p>
     children and give the first heading to the methods, so there is no
     "Introduction" head to find — the introduction is simply everything
     before the first titled section.
  2. The ABSTRACT looks almost exactly like it.  A structured abstract often
     contains <sec><title>Background</title>, which is the single most
     dangerous false positive here, so every strategy excludes anything
     living inside <abstract>.

Unlike extract_results.py there is no whole-body fallback: for a Nature-style
"Main" article the introduction is the opening paragraphs, never the whole
body, so the fallback takes the LEAD paragraphs and marks them `is_lead`.
Files written from that path get a '_lead' suffix rather than '_main'.

Usage:
    python3 extract_intro.py input.xml -o output_intro.txt
    python3 extract_intro.py input.xml            # prints to stdout
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
    _is_main_journal,
    _is_letter_to_editor,
)

# Back-matter truncation is defined next to the results whole-body fallback
# that needed it first.  Reused here as cheap insurance for the lead-paragraph
# fallback rather than duplicated.
from extract_results import _truncate_body_tail

# XML parsing with namespace repair is identical across all three extractors.
from extract_results import parse_xml_with_recovery


# ---------------------------------------------------------------------------
# Section-title vocabulary
# ---------------------------------------------------------------------------

# Titles that name an introduction section.
INTRO_TITLES = {
    'introduction', 'introductions', 'background', 'backgrounds',
    'background and aims', 'background and objectives',
    'background and significance', 'introduction and background',
    'general introduction', 'overview',
}

# Prefixes that make a title an introduction even with trailing words,
# e.g. "Background to the study".
INTRO_TITLE_PREFIXES = ('introduction', 'background')

# Section titles that mark the END of the introduction — in practice every
# major section that can follow it.
NON_INTRO_SECTIONS = {
    'method', 'methods', 'materials and methods', 'methods and materials',
    'material and methods', 'subjects and methods', 'patients and methods',
    'participants and methods', 'experimental procedures',
    'experimental section', 'online methods', 'methodology',
    'research design and methods', 'study design', 'study population',
    'result', 'results', 'results and discussion', 'findings',
    'discussion', 'discussions', 'conclusion', 'conclusions',
    'acknowledgment', 'acknowledgments', 'acknowledgement', 'acknowledgements',
    'references', 'bibliography', 'funding', 'competing interests',
    'conflict of interest', 'conflicts of interest', 'disclosure', 'disclosures',
    'data availability', 'code availability', 'author contributions',
    'authors contributions', 'supplementary', 'supplemental', 'appendix',
    'abbreviations', 'ethics approval', 'ethics declarations',
    'web resources', 'urls', 'limitations', 'abstract',
}

# Titles that are never an introduction, used to veto fuzzy matches.  Note
# that 'background' is deliberately absent — it is an intro synonym here.
NON_INTRO_PREFIXES = (
    'abstract', 'method', 'material', 'result', 'finding', 'discussion',
    'conclusion', 'subject', 'patient', 'participant', 'acknowledg',
    'reference', 'bibliograph', 'funding', 'disclosure', 'conflict',
    'competing', 'data availability', 'code availability', 'supplement',
    'appendix', 'author', 'ethic', 'limitation', 'web resource', 'url',
    'statistical analys', 'study design', 'study population',
)

# A lead-paragraph extraction from a body with no section structure at all
# cannot see where the introduction ends, so it is capped.  Generous enough
# for a long introduction, small enough not to swallow an entire article.
_LEAD_MAX_PARAGRAPHS = 6
_LEAD_MAX_WORDS = 900

# Below this, an "introduction" is almost certainly a standfirst, a teaser or
# a stray caption rather than a section.
_MIN_INTRO_WORDS = 30


# A leading "Introduction." / "BACKGROUND:" heading sentence, as prepended by
# extract_text_from_element for a titled JATS section.
_LEADING_HEADING_RE = re.compile(
    r'^\s*(?:\d+\.?\s*)*'
    r'(?:Introductions?|Backgrounds?|Background\s+and\s+(?:aims?|objectives?|significance)|Overview)'
    r'\s*[.:]\s*',
    re.IGNORECASE,
)


def _strip_numbering(title):
    """Drop leading section numbering such as '1.' or '1.1 ' from a title."""
    return re.sub(r'^\s*(?:\d+\.)*\d+\.?\s+', '', title.strip()).strip()


def _is_intro_title(title, max_words=6):
    """
    True when a section title names an introduction section.

    Accepts 'Introduction', 'INTRODUCTION', '1. Introduction', 'Background',
    'Background and aims'.  Rejects headings belonging to another section and
    long descriptive headings, which are subsections rather than section heads.
    """
    if not title:
        return False

    t = _strip_numbering(title).lower().rstrip(':.')
    if not t:
        return False

    if t.startswith(NON_INTRO_PREFIXES):
        return False

    if t in INTRO_TITLES:
        return True

    if t.startswith(INTRO_TITLE_PREFIXES):
        return len(t.split()) <= max_words

    return False


def _is_non_intro_title(title):
    """True when a title marks the start of a section after the introduction."""
    if not title:
        return False
    t = _strip_numbering(title).lower().rstrip(':.')
    if t in NON_INTRO_SECTIONS:
        return True
    if any(t.startswith(stop + ' ') for stop in NON_INTRO_SECTIONS):
        return True
    # Any heading that is not itself an intro heading ends the introduction
    # once we are inside one — introductions do not have sibling subsections
    # in the formats seen here.
    return t.startswith(NON_INTRO_PREFIXES)


# ---------------------------------------------------------------------------
# JATS XML extraction
# ---------------------------------------------------------------------------

# Sections that may be tagged as intro but are really front/back matter.
_JATS_SKIP_TITLE_KEYWORDS = (
    'analysis team', 'author', 'contributor', 'writing group',
    'study group', 'consortium', 'working group', 'steering committee',
    'acknowledgment', 'funding', 'competing interest', 'conflict of interest',
)


def _abstract_element_ids(root):
    """
    Ids of every element inside an <abstract>, plus <front> as a whole.

    This is the central guard for introduction extraction: a structured
    abstract's <title>Background</title> is otherwise indistinguishable from
    a real introduction heading.
    """
    ids = set()
    for elem in root.iter():
        tag = elem.tag.split('}')[-1] if '}' in elem.tag else elem.tag
        if tag in ('abstract', 'front', 'front-stub'):
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


# sec-types that are back matter rather than article content.
_BACK_MATTER_SEC_TYPES = (
    'supplementary-material', 'supplementary', 'extended-data', 'ack',
    'acknowledgments', 'acknowledgements', 'coi-statement', 'conflict',
    'data-availability', 'funding', 'appendix', 'abbreviations', 'ethics',
)


def _is_back_matter_sec(sec):
    """True when a <sec> is back matter (supplement, acknowledgments, ...)."""
    sec_type = (sec.get('sec-type') or '').strip().lower()
    if sec_type and sec_type.startswith(_BACK_MATTER_SEC_TYPES):
        return True
    title = _strip_numbering(_section_title(sec)).lower().rstrip(':.')
    if not title:
        return False
    return title in NON_INTRO_SECTIONS and not title.startswith(
        ('method', 'result', 'discussion', 'conclusion')
    )


def find_all_intro_sections(root):
    """
    Find all top-level introduction sections in a JATS document, in order.

    Three passes, most reliable signal first:
      1. sec-type containing 'intro'
      2. sec-type containing 'background'
      3. a <title> that reads as an introduction heading

    Pass 3 carries the corpus: the largest group of introduction sections
    have an empty sec-type and are identified by their title alone.

    Sections inside an <abstract> or <front>, sections nested inside an
    already-selected section, and author/funding sections mis-tagged as
    introduction are excluded.
    """
    in_abstract = _abstract_element_ids(root)
    doc_order = {id(elem): i for i, elem in enumerate(root.iter())}

    candidates = []
    selected_ids = set()

    def accept(sec, priority):
        if id(sec) in in_abstract or id(sec) in selected_ids:
            return
        if _is_front_or_back_matter(sec):
            return
        candidates.append((sec, priority))
        for descendant in sec.iter():
            selected_ids.add(id(descendant))

    all_secs = [e for e in root.iter() if e.tag.endswith('sec')]

    # Pass 1 — sec-type mentioning intro ("intro", "introduction").
    for sec in all_secs:
        sec_type = (sec.get('sec-type') or '').strip().lower()
        if 'intro' in sec_type:
            accept(sec, 1)

    # Pass 2 — sec-type mentioning background.
    for sec in all_secs:
        sec_type = (sec.get('sec-type') or '').strip().lower()
        if sec_type and 'background' in sec_type:
            accept(sec, 2)

    # Pass 3 — title-based detection (the common case).
    for sec in all_secs:
        if _is_intro_title(_section_title(sec)):
            accept(sec, 3)

    candidates.sort(key=lambda pair: doc_order.get(id(pair[0]), 0))
    return [sec for sec, _ in candidates]


def _jats_untitled_intro(root):
    """
    Recover the introduction from articles that open <body> with bare <p>
    children and give the first heading to another section.

    In that layout the introduction is exactly the run of elements before the
    first <sec>.  Returns (elements, is_lead); `is_lead` is True when the body
    has no <sec> at all, in which case the run is capped because there is no
    structural signal for where the introduction ends.
    """
    body = root.find('.//{*}body')
    if body is None:
        return [], False

    children = list(body)
    if not children:
        return [], False

    lead = []
    for child in children:
        tag = child.tag.split('}')[-1] if '}' in child.tag else child.tag
        if tag == 'sec':
            break
        if tag in ('p', 'disp-quote', 'list'):
            lead.append(child)

    if not lead:
        return [], False

    # Only a CONTENT section counts as structure.  Many articles are a flat
    # run of <p> plus a single trailing <sec sec-type="supplementary-material">
    # or acknowledgments block; treating that as structure made the paragraph
    # run above extend to the end of the article instead of the end of the
    # introduction.
    has_sections = any(
        (c.tag.split('}')[-1] if '}' in c.tag else c.tag) == 'sec'
        and not _is_back_matter_sec(c)
        for c in children
    )

    # No sections anywhere: the body is an undifferentiated run of paragraphs
    # and the introduction boundary is unknowable, so cap and flag it.
    if not has_sections:
        return lead[:_LEAD_MAX_PARAGRAPHS], True

    return lead, False


def _jats_main_lead(root):
    """
    Lead paragraphs of a Nature-style "Main" article or a letter to the editor.

    These have no introduction section: the narrative runs under a single
    heading and opens with one to three paragraphs of background that serve as
    the introduction.  Takes the leading paragraphs of the first body section,
    capped, and always reports is_lead=True.
    """
    body = root.find('.//{*}body')
    if body is None:
        return []

    # Prefer the first top-level section's paragraphs; fall back to the body's.
    containers = [body]
    for child in body:
        if (child.tag.split('}')[-1] if '}' in child.tag else child.tag) == 'sec':
            containers.insert(0, child)
            break

    for container in containers:
        paras = [
            c for c in container
            if (c.tag.split('}')[-1] if '}' in c.tag else c.tag) == 'p'
        ]
        if paras:
            return paras[:_LEAD_MAX_PARAGRAPHS]

    return []


def find_intro_sections(root):
    """
    Return (elements, is_lead) for a JATS document.

    `elements` is a list of sections (or paragraphs, for the untitled layouts)
    in document order.  `is_lead` is True when the text is a capped run of
    lead paragraphs rather than a delimited introduction section.
    """
    sections = find_all_intro_sections(root)
    if sections:
        return sections, False

    # Untitled introduction: bare paragraphs before the first <sec>.
    lead, is_lead = _jats_untitled_intro(root)
    if lead:
        return lead, is_lead

    # Nature-style "Main" articles and letters to the editor.
    if _is_main_journal(root) or _is_letter_to_editor(root):
        paras = _jats_main_lead(root)
        if paras:
            return paras, True

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


def extract_tei_intro(root):
    """
    Extract the introduction from a TEI (GROBID) document.

    Returns (text, is_lead).  GROBID emits a flat list of sibling <div>
    elements under <body>; the introduction <div> may hold only a <head>, with
    the prose in the divs that follow, up to the next major section.

    GROBID also routinely drops the introduction head altogether, in which
    case the leading head-less divs are the introduction — taken as lead
    paragraphs.
    """
    body = _tei_find(root, 'body')
    if body is None:
        body = root.find('.//tei:body', TEI_NS)
    if body is None:
        body = root.find('.//body')
    if body is None:
        return None, False

    divs = _tei_findall(body, 'div')
    if not divs:
        return None, False

    def head_text(div):
        head = _tei_find(div, 'head')
        if head is not None and head.text:
            return head.text.strip()
        return ''

    def div_text(div):
        parts = []
        for p in _tei_findall(div, 'p'):
            # Not itertext(): GROBID keeps <ref type="bibr">[1]</ref> inline,
            # and itertext() would pull those bracketed numbers into the prose.
            para = flatten_without_citations(p).strip()
            if para:
                parts.append(clean_extracted_text(para) + ' ')
        return parts

    # Primary pass: an explicit introduction head.
    intro_start = None
    for i, div in enumerate(divs):
        if _is_intro_title(head_text(div)):
            intro_start = i
            break

    if intro_start is not None:
        text_parts = []
        for i, div in enumerate(divs[intro_start:], start=intro_start):
            title = head_text(div)
            if i > intro_start and _is_non_intro_title(title):
                break
            # The heading itself is dropped for the introduction: unlike a
            # results subheading it carries no information, and keeping it
            # puts a bare "Introduction." sentence in front of every file.
            if i > intro_start:
                clean_title = _strip_numbering(title).rstrip(' .;,:')
                if clean_title:
                    text_parts.append(clean_title + '. ')
            text_parts.extend(div_text(div))

        result = clean_extracted_text(' '.join(text_parts))
        return (result, False) if result else (None, False)

    # Fallback: no introduction head — take the leading head-less divs, which
    # is where GROBID puts the opening prose when it loses the heading.
    text_parts = []
    for div in divs:
        title = head_text(div)
        if title and _is_non_intro_title(title):
            break
        if title and not _is_intro_title(title):
            break
        text_parts.extend(div_text(div))
        if len(' '.join(text_parts).split()) >= _LEAD_MAX_WORDS:
            break

    result = clean_extracted_text(' '.join(text_parts))
    if not result:
        return None, False
    return ' '.join(result.split()[:_LEAD_MAX_WORDS]), True


# ---------------------------------------------------------------------------
# BioC XML extraction (Auto-CORPus _bioc.xml files)
# ---------------------------------------------------------------------------

_INTRO_HEADER_RE = re.compile(
    r'^(Introductions?'
    r'|Backgrounds?'
    r'|Background\s+and\s+(?:aims?|objectives?|significance))'
    r'\s*[:.]?\s',
    re.IGNORECASE,
)

_NON_INTRO_HEADER_RE = re.compile(
    r'^(Results?'
    r'|Discussion'
    r'|Conclusions?'
    r'|(?:Materials?\s+and\s+)?Methods?'
    r'|Subjects?\s+and\s+Methods?'
    r'|Patients?\s+and\s+Methods?'
    r'|Methodology'
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


def _passage_type(passage):
    for infon in passage.findall('infon'):
        if infon.get('key') == 'type':
            return (infon.text or '').strip().lower()
    return ''


def _passage_text(passage):
    text_elem = passage.find('text')
    if text_elem is not None and text_elem.text:
        return text_elem.text.strip()
    return ''


def _is_abstract_passage(passage):
    """True when a BioC passage belongs to the abstract."""
    for level in (1, 2):
        title = (_passage_section_title(passage, level) or '').lower()
        if title.startswith('abstract'):
            return True
    return 'abstract' in _passage_type(passage)


def _bioc_subsection_intro(root):
    """
    Recover the introduction from BioC files whose section_title_1 is
    uninformative ("document part", "Author notes") but whose section_title_2
    carries the real headings.
    """
    passages = list(root.iter('passage'))
    heads = [_passage_section_title(p, 2) or '' for p in passages]
    if not any(heads):
        return None

    # Only step down to section_title_2 when section_title_1 is genuinely
    # uninformative.  If the file has real top-level labels the structure is
    # intact and the other strategies — which respect it — must win.
    top_labels = {
        _strip_numbering(_passage_section_title(p, 1) or '').lower()
        for p in passages
    }
    STRUCTURAL = ('main', 'method', 'result', 'introduction', 'background',
                  'discussion', 'conclusion', 'findings')
    if any(lbl.startswith(STRUCTURAL) for lbl in top_labels if lbl):
        return None

    parts = [
        clean_extracted_text(_passage_text(p)) + ' '
        for p, h in zip(passages, heads)
        if _is_intro_title(h) and _passage_text(p) and not _is_abstract_passage(p)
    ]
    if parts:
        return clean_extracted_text(' '.join(parts))

    return None


def _bioc_main_lead(root):
    """
    Lead passages of a BioC "Main" article.

    The introduction is the opening passages of the Main narrative, so this
    takes them up to the paragraph cap rather than the whole section.
    """
    parts = []
    for passage in root.iter('passage'):
        title = (_passage_section_title(passage) or '').strip().lower()
        if title != 'main':
            continue
        if _is_abstract_passage(passage):
            continue
        text = _passage_text(passage)
        if not text:
            continue
        parts.append(clean_extracted_text(text) + ' ')
        if len(parts) >= _LEAD_MAX_PARAGRAPHS:
            break

    result = clean_extracted_text(' '.join(parts))
    if not result:
        return None
    return ' '.join(result.split()[:_LEAD_MAX_WORDS])


def extract_bioc_intro(root):
    """
    Extract the introduction from a BioC document.

    Returns (text, is_lead).  `is_lead` is True when the text came from the
    lead passages of a "Main" article rather than a labelled introduction.
    """
    # Primary strategy: section_title_1 labels.
    #
    # Auto-CORPus sometimes stamps section_title_1="Introduction" on EVERY
    # passage of the body and leaves the real structure in section_title_2
    # (Methods, Discussion, Conclusions).  Collecting on level 1 alone then
    # returns the whole article, so a passage whose level-2 heading names
    # another section is skipped: level 2 is the more specific signal and wins.
    text_parts = []
    for passage in root.iter('passage'):
        title = _passage_section_title(passage)
        if title is None or _is_abstract_passage(passage):
            continue
        if not _is_intro_title(title):
            continue
        subtitle = _passage_section_title(passage, 2)
        if subtitle and not _is_intro_title(subtitle):
            continue
        para_text = _passage_text(passage)
        if para_text:
            text_parts.append(clean_extracted_text(para_text) + ' ')

    if text_parts:
        return clean_extracted_text(' '.join(text_parts)), False

    # Publishers that file the whole body under one meaningless
    # section_title_1 and put the real headings in section_title_2.
    sub = _bioc_subsection_intro(root)
    if sub:
        return sub, False

    # Nature-style files: the narrative sits under "Main" and opens with the
    # background that serves as the introduction.
    for passage in root.iter('passage'):
        title = _passage_section_title(passage)
        if title and title.strip().lower() == 'main':
            lead = _bioc_main_lead(root)
            if lead:
                return lead, True
            break

    # Fallback: broken or missing labels — scan passage text for an inline
    # "Introduction" header.
    return _bioc_fallback_inline_headers(root), False


def _bioc_fallback_inline_headers(root):
    """
    Fallback for BioC files whose section labels are wrong or missing (common
    in Oxford Academic output, where the whole body lands under "Author notes"
    or "document part").  Finds a passage opening with an inline
    "Introduction" header and collects until the next major section header.
    """
    passages = [p for p in root.iter('passage') if not _is_abstract_passage(p)]

    intro_start = None
    for i, passage in enumerate(passages):
        text = _passage_text(passage)
        if text and _INTRO_HEADER_RE.match(text):
            intro_start = i
            break

    if intro_start is None:
        return None

    text_parts = []
    for i, passage in enumerate(passages[intro_start:], start=intro_start):
        text = _passage_text(passage)
        if not text:
            continue

        if i > intro_start and _NON_INTRO_HEADER_RE.match(text):
            break

        if i == intro_start:
            text = _INTRO_HEADER_RE.sub('', text, count=1).strip()

        if text:
            text_parts.append(clean_extracted_text(text) + ' ')

    result = clean_extracted_text(' '.join(text_parts))
    return result if result else None


# ---------------------------------------------------------------------------
# Main extraction entry point
# ---------------------------------------------------------------------------

def extract_intro_section(xml_file):
    """
    Extract the introduction text from a JATS, TEI, or BioC XML file.

    Returns a dict with:
      'text'    — the introduction text, or None when none was found
      'is_lead' — True when the text is a capped run of lead paragraphs rather
                  than a delimited introduction section
    """
    try:
        root = parse_xml_with_recovery(xml_file)
    except ET.ParseError as e:
        print(f"Error parsing XML file: {e}", file=sys.stderr)
        return {'text': None, 'is_lead': False}
    except Exception as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        return {'text': None, 'is_lead': False}

    try:
        fmt = detect_xml_format(root)

        if fmt == 'tei':
            text, is_lead = extract_tei_intro(root)
            if text and is_lead:
                text = _truncate_body_tail(text, Path(xml_file).name)
            return {'text': text, 'is_lead': is_lead}

        if fmt == 'bioc':
            text, is_lead = extract_bioc_intro(root)
            if text and is_lead:
                text = _truncate_body_tail(text, Path(xml_file).name)
            return {'text': text, 'is_lead': is_lead}

        if fmt == 'unknown':
            print(f"Warning: could not detect XML format for {xml_file}, trying JATS",
                  file=sys.stderr)

        # JATS
        elements, is_lead = find_intro_sections(root)
        if not elements:
            return {'text': None, 'is_lead': False}

        parts = []
        for elem in elements:
            elem_text = extract_text_from_element(elem).strip()
            if elem_text:
                parts.append(elem_text)

        intro_text = clean_extracted_text(' '.join(parts))

        # extract_text_from_element prepends each section's title, which for
        # the introduction means a bare "Introduction." sentence in front of
        # every file.  It carries no information and would become a spurious
        # sentence for the tokenizer, so it is dropped (the TEI and BioC
        # paths never add it in the first place).
        intro_text = _LEADING_HEADING_RE.sub('', intro_text, count=1).strip()

        if not intro_text:
            return {'text': None, 'is_lead': False}

        if is_lead:
            intro_text = _truncate_body_tail(intro_text, Path(xml_file).name)
            intro_text = ' '.join(intro_text.split()[:_LEAD_MAX_WORDS])

        # An introduction this short is a standfirst, a teaser or a stray
        # caption — not a section worth passing downstream.
        if len(intro_text.split()) < _MIN_INTRO_WORDS:
            print(
                f"Introduction too short to be a section "
                f"({len(intro_text.split())} words, not extracted).",
                file=sys.stderr,
            )
            return {'text': None, 'is_lead': False}

        return {'text': intro_text, 'is_lead': is_lead}

    except Exception as e:
        print(f"Error processing file: {e}", file=sys.stderr)
        return {'text': None, 'is_lead': False}


def main():
    parser = argparse.ArgumentParser(
        description='Extract the introduction from JATS, TEI, or BioC XML files'
    )
    parser.add_argument('input_file', help='Path to the input XML file')
    parser.add_argument(
        '-o', '--output',
        help='Path to the output text file (prints to stdout if omitted)'
    )
    args = parser.parse_args()

    result = extract_intro_section(args.input_file)
    intro_text = result['text']

    if intro_text is None:
        print("No introduction section found in the XML file.", file=sys.stderr)
        sys.exit(1)

    if args.output:
        output_path = Path(args.output)
        # Lead-paragraph fallback gets a '_lead' marker, the introduction
        # analogue of the '_main' marker used by the methods and results
        # extractors.
        if result['is_lead']:
            output_path = output_path.parent / f"{output_path.stem}_lead{output_path.suffix}"
        output_path.write_text(intro_text, encoding='utf-8')
        print(f"Introduction section extracted to: {output_path}")
    else:
        print(intro_text)


if __name__ == '__main__':
    main()
