#!/usr/bin/env python3
"""
Extract methods section from JATS, TEI (GROBID), and BioC XML files.
This script extracts all text content from methods sections across formats.

Supported formats:
  - JATS XML: Standard PubMed Central format (<sec sec-type="methods">)
  - TEI XML:  GROBID output from PDFs (.pdf.tei.xml), <div> with <head>Methods</head>
  - BioC XML: Auto-CORPus/BioC format (_bioc.xml), <passage> with <infon>Methods</infon>
"""

import xml.etree.ElementTree as ET
import argparse
import sys
import re
import unicodedata
import html as htmlmod
from pathlib import Path


# ---------------------------------------------------------------------------
# Format detection
# ---------------------------------------------------------------------------

def detect_xml_format(root):
    """
    Detect whether an XML root element is JATS, TEI, or BioC format.

    Returns one of: 'jats', 'tei', 'bioc', or 'unknown'.
    """
    tag = root.tag

    # TEI: root tag is <TEI> (possibly namespaced)
    if tag.endswith('}TEI') or tag == 'TEI':
        return 'tei'

    # BioC: root tag is <collection>
    if tag == 'collection':
        return 'bioc'

    # JATS: root tag is <article> (possibly namespaced)
    if tag.endswith('}article') or tag == 'article':
        return 'jats'

    # Fallback heuristics
    # TEI namespace present anywhere
    for elem in root.iter():
        if 'http://www.tei-c.org/ns/1.0' in elem.tag:
            return 'tei'

    # BioC has <document> > <passage> structure
    if root.find('.//passage') is not None:
        return 'bioc'

    # JATS has <body> > <sec> structure
    if root.find('.//{*}sec') is not None:
        return 'jats'

    return 'unknown'
  
    return False
  
# -----------------------------------------------------------------------------
# Detect if this is a journal format without methods sections
# -----------------------------------------------------------------------------
def _is_main_journal(root):
    journal = root.find('.//{*}journal-title')

    main_journals = {
        'nature genetics',
        'human genetics and genomics advances',
        'the american journal of human genetics',
        'american journal of human genetics',
        'the american journal of human genetics',
        'journal of human genetics'
    }

    if journal is not None and journal.text:
        journal_name = journal.text.strip().lower()
        return journal_name in main_journals

    return False
  
# -----------------------------------------------------------------------------
# Detect if this article is a letter to the editor (which often has no methods section)
# -----------------------------------------------------------------------------
import re

def _is_letter_to_editor(root):
    pattern = re.compile(r'^\s*(to the editor|dear editor)\b', re.IGNORECASE)

    for p in root.findall('.//{*}p'):
        text = flatten_without_citations(p)
        if not text:
            continue

        if pattern.match(text):
            return True

    return False


# -------------------------------------------------------------------------------
# Main extraction function - for Bioc Nature genetics
# -------------------------------------------------------------------------------
def extract_bioc_main(root):
    text_parts = []

    for passage in root.iter('passage'):
        section_title = None

        for infon in passage.findall('infon'):
            if infon.get('key') == 'section_title_1':
                section_title = infon.text
                break

        if section_title and section_title.strip().lower() == 'main':
            text_elem = passage.find('text')
            if text_elem is not None and text_elem.text:
                para_text = text_elem.text.strip()
                if para_text:
                    text_parts.append(clean_extracted_text(para_text) + ' ')

    # Clean the JOIN, not only each part: every part already ends in ' ', so a
    # plain join left a double space at each paragraph boundary -- the reason
    # BioC files differed from every other path on whitespace.
    result = clean_extracted_text(' '.join(text_parts))
    return result if result else None


# ---------------------------------------------------------------------------
# Citation elements (shared across formats)
# ---------------------------------------------------------------------------

# Element names that carry a bibliographic citation.  Publishers disagree on
# both the name and the attribute:
#   PMC / JATS  <xref ref-type="bibr" rid="CR1">1</xref>
#   GROBID TEI  <ref type="bibr" target="#b0">[1]</ref>
#   Elsevier    <ce:cross-ref refid="bib15"><ce:sup>15</ce:sup></ce:cross-ref>
# The Elsevier form has no ref-type at all, so an attribute-only test misses
# it and its <ce:sup> digits end up glued to the preceding word
# ("hazards models15,16").  Hence a test on name AND attribute.
_CITATION_ELEMENT_NAMES = ('cross-ref', 'cross-refs')


def _is_citation_element(elem):
    """True when an element is a bibliographic citation reference."""
    if elem.get('ref-type') == 'bibr' or elem.get('type') == 'bibr':
        return True
    tag = elem.tag.split('}')[-1] if '}' in elem.tag else elem.tag
    if tag in _CITATION_ELEMENT_NAMES and elem.get('refid'):
        return True
    # Elsevier also emits <ce:cross-refs> whose refid lives on the wrapper;
    # a bare cross-ref with no refid is an internal figure/table link, which
    # the narrative needs, so it is deliberately kept.
    return False


def flatten_without_citations(elem):
    """
    Flatten an element to text, dropping bibliographic citations.

    The drop-in replacement for ''.join(elem.itertext()) wherever a paragraph
    is flattened: itertext() has no way to skip a subtree, so it pulls the
    citation's own digits into the prose.  Tail text after a dropped citation
    is preserved, because that is ordinary sentence text.
    """
    parts = []

    if not _is_citation_element(elem):
        if elem.text:
            parts.append(elem.text)
        for child in elem:
            parts.append(flatten_without_citations(child))
            if child.tail:
                parts.append(child.tail)

    return ''.join(parts)


# ---------------------------------------------------------------------------
# Text cleaning (shared across formats)
# ---------------------------------------------------------------------------

# Byte-order marks and zero-width spaces -> replaced by a space (see the
# comment at the call site).  U+200B is literally ZERO WIDTH SPACE.
_ZERO_WIDTH_RE = re.compile('[\u200b\u2060\ufeff]')

# True joiners, which carry no width and no space: simply dropped.
_JOINER_RE = re.compile('[\u200c\u200d]')

# Typographic quote marks -> ASCII.  Includes the low-9 forms (\u201a and
# \u201e, used as opening quotes in German and Polish sources) and guillemets.
# PRIMES ARE DELIBERATELY ABSENT: \u2032/\u2033 carry meaning in nucleotide
# notation (5' and 3' ends) and stay exactly as the publisher set them.
_QUOTE_TRANSLATION = {
    0x2018: "'", 0x2019: "'", 0x201a: "'", 0x201b: "'",   # single
    0x201c: '"', 0x201d: '"', 0x201e: '"', 0x201f: '"',   # double
    0x00ab: '"', 0x00bb: '"',                             # guillemets
}

# Trademark, registered and copyright signs, with any preceding space.
# Applied BEFORE NFKC, which would otherwise turn U+2122 into "TM".
_TRADEMARK_RE = re.compile(r'\s*[\u00ae\u2122\u00a9]')

# An exponent detached from its base by superscript flattening:
# "10 -8", "10 \u22128", "10 \u2013 8".  Anchored on a literal 10, which is how
# every occurrence in this corpus appears, so ordinary arithmetic spacing
# elsewhere is untouched.
_SPLIT_EXPONENT_RE = re.compile(r'10\s+([-\u2212\u2013])\s*(\d)')


# A citation number stranded after a closing parenthesis: "(LDSC)41",
# "SNPTEST (v2.5)50".  Deliberately narrow.  The general ")<digits>" rule is
# unsafe in this corpus: it destroys HLA alleles (HLA-DQA1(*)0601), gene
# symbols formed from an expanded abbreviation (interleukin (IL)10 -> "(IL)"),
# chromosome numbers (chromosome (chr)1), amino-acid positions (methionine
# (M)67), RT primers (oligo(dT)18), chemistry (1,25(OH)2D3) and the BMI
# formula (weight (kg)/height (m)2).  So only two unambiguous shapes are
# accepted, and only when the digits TERMINATE -- nothing alphanumeric, no
# decimal, no range dash, no nested close and no unit sign may follow, which
# is what keeps "(H1N1)2009", "(SLC)17A" and "(1.12-1.25)0.590" intact.
_TRAILING_REF_RE = re.compile(
    r'(?<!\\)\(([A-Za-z0-9][A-Za-z0-9.\-]{1,24})\)(\d{1,3})'
    r'(?![\dA-Za-z]|\.\d|[-\u2013\u2014]\d|\)|%|/|\u00b0|\u00d7)'
)

# Shape 1 -- a version string: "v1.0.1", "v2.5", "v.1.0", "3.0", "0.5.5".
# A dot or a leading "v" is required, so a bare "(2)15" (an equation or
# footnote number) is never touched.
_VERSION_INNER_RE = re.compile(r'^(?:[vV]\.?\d+(?:\.\d+)*[a-z]?|\d+(?:\.\d+)+[a-z]?)$')

# Shape 2 -- a NAMED TOOL, matched against an explicit allowlist rather than
# by shape.  Shape cannot do this job: "(PSCA)5" and "(MUC1)6" are gene
# symbols followed by a reference number (strip the number), while "(ERK)1"
# and "(PCDH)9" are gene symbols SPLIT from their digit (ERK1, PCDH9 -- keep
# it), and the two are indistinguishable by pattern.  Cohort acronyms
# ((FHS)78, (MESA)82) are likewise the same shape as a split gene symbol, so
# they are not accepted either.  An allowlist gives up some true positives and
# in exchange can never invent a wrong gene name.
_KNOWN_TOOLS = {
    # imputation / phasing
    'beagle', 'impute', 'impute2', 'impute4', 'mach', 'minimac', 'minimac2',
    'minimac3', 'minimac4', 'shapeit', 'shapeit2', 'shapeit4', 'eagle',
    'eagle2', 'hapi-ur', 'fastphase',
    # association / QC / meta-analysis
    'plink', 'snptest', 'bolt-lmm', 'bolt', 'saige', 'regenie', 'gemma',
    'emmax', 'fastgwa', 'gcta', 'metal', 'metasoft', 'gwama', 'quicktest',
    'probabel', 'rvtests', 'epacts', 'smmat', 'seqmeta', 'skat', 'acat-v',
    # heritability / genetic correlation / pathway
    'ldsc', 's-ldsc', 'gnova', 'hess', 'popcorn', 'magma', 'depict', 'fuma',
    'pascal', 'vegas', 'vegas2', 'gsea', 'ai-reml',
    # annotation / functional
    'annovar', 'vep', 'cadd', 'gwava', 'dann', 'sift', 'polyphen',
    'polyphen-2', 'snpeff', 'regulomedb', 'haploreg',
    # PRS
    'prsice', 'prsice-2', 'ldpred', 'ldpred2', 'prs-cs', 'prscs', 'lassosum',
    # ancestry / structure
    'eigenstrat', 'eigensoft', 'admixture', 'structure', 'rfmix', 'flare',
    'terastructure', 'peddy', 'king',
    # alignment / variant calling / general
    'bwa', 'bowtie', 'bowtie2', 'samtools', 'bcftools', 'vcftools', 'gatk',
    'picard', 'fastqc', 'trimmomatic', 'star', 'salmon', 'kallisto',
    'htseq', 'deseq2', 'edger', 'limma', 'liftover', 'crossmap',
    # stats environments
    'stata', 'sas', 'spss', 'matlab', 'ggplot2', 'metafor', 'mendelianrandomization',
}


def _is_known_tool(inner):
    """True when the parenthetical names a tool on the allowlist."""
    name = inner.strip().lower()
    # Tolerate a trailing version glued to the name ("PLINK1.9", "GCTA-1.94").
    name = re.sub(r'[-\s]?v?\d+(?:\.\d+)*$', '', name) or name
    return name in _KNOWN_TOOLS


def _strip_trailing_refs(text):
    """
    Drop a citation number stranded after a closing parenthesis, for the two
    safe cases only: a version string ("(v1.0.1)36") and a tool named on the
    allowlist ("(LDSC)41").  Anything else -- a cohort acronym, a gene symbol
    -- is left alone, because "(PCDH)9" (gene PCDH9) and "(PSCA)5" (gene plus
    reference 5) are the same shape.
    """
    def repl(match):
        inner = match.group(1)
        if _VERSION_INNER_RE.match(inner) or _is_known_tool(inner):
            return '(' + inner + ')'
        return match.group(0)

    return _TRAILING_REF_RE.sub(repl, text)


# A reference marker flattened onto the end of a word: "populations10,11,12".
# Host word must be 4+ LOWERCASE letters, which excludes gene symbols
# (TCF7L2, PRDM15, MPPED2) and rsIDs (rs10795076).  Requires two or more
# numbers: the single-digit form ("association9") is indistinguishable from
# gene and software names (zranb3, plink2, minimac3, ggplot2, impute2) and is
# deliberately NOT matched -- see _PROSE_REF_HOSTS below for that case.
# The host may be capitalised ("Asians7,18", "Europeans6,12", "Biobank18,19")
# but its remaining letters must be lowercase, which excludes all-caps gene
# symbols (TCF7L2, ZRANB3).  Capitalised mouse gene symbols (Notch3, Tcf7l2)
# are safe here because this rule requires TWO OR MORE numbers, and a gene
# symbol never carries a comma-separated digit list.
_GLUED_REFS_RE = re.compile(
    r'(?<![A-Za-z0-9])([A-Za-z][a-z]{3,})(\d{1,3}(?:[,\u2013-]\d{1,3}){1,})(?![\d,]*\d{3}\b)'
)

# A thousands-separated number, not a reference list: "the13,026 subjects"
# (a lost space before a sample size) must survive untouched.
_THOUSANDS_RE = re.compile(r'^\d{1,3}(?:,\d{3})+$')

# Ordinary prose nouns that precede a citation.  Used for the single-number
# form, where shape alone cannot separate a reference marker from a gene or
# software name, so only these known-safe hosts are stripped.
_PROSE_REF_HOSTS = (
    'studies', 'study', 'analysis', 'analyses', 'population', 'populations',
    'cohort', 'cohorts', 'patients', 'participants', 'individuals', 'carriers',
    'controls', 'cases', 'association', 'associations', 'susceptibility',
    'heritability', 'meta-analysis', 'report', 'reports', 'reported',
    'described', 'previously', 'elsewhere', 'respectively', 'others',
    'work', 'works', 'literature', 'review', 'reviews', 'guidelines',
    'method', 'methods', 'protocol', 'criteria', 'disease', 'diseases',
    'cancer', 'carcinoma', 'risk', 'variants', 'loci', 'locus', 'gene',
    'genes', 'sequence', 'kinetics', 'program', 'software',
)

_PROSE_GLUED_REF_RE = re.compile(
    r'(?<![A-Za-z0-9])(' + '|'.join(sorted(_PROSE_REF_HOSTS, key=len, reverse=True))
    + r')(\d{1,3})(?![\dA-Za-z.,\-/])',
    re.IGNORECASE,
)


def _strip_glued_refs(text):
    """
    Strip superscript reference markers flattened onto the preceding word.

    Two rules, both conservative:
      * two or more numbers after any 4+ letter lowercase word, rejecting
        thousands separators, descending runs and numbers above 300
      * a single number, but only after a known prose noun
    """
    def _multi(match):
        word, digits = match.group(1), match.group(2)
        if _THOUSANDS_RE.match(digits):
            return match.group(0)
        numbers = [int(n) for n in re.split(r'[,\u2013-]', digits) if n]
        if numbers != sorted(numbers):
            return match.group(0)
        if any(n > 300 for n in numbers):
            return match.group(0)
        return word

    def _single(match):
        if int(match.group(2)) > 300:
            return match.group(0)
        return match.group(1)

    text = _GLUED_REFS_RE.sub(_multi, text)
    return _PROSE_GLUED_REF_RE.sub(_single, text)


def clean_extracted_text(text):
    """
    Apply standard text cleaning to extracted methods text.
    Shared across all XML formats.
    """
    # Trademark signs come FIRST, before NFKC: normalisation rewrites
    # U+2122 to the literal letters "TM" ("ThermoFisher\u2122" ->
    # "ThermoFisherTM"), after which no symbol remains to strip.
    text = _TRADEMARK_RE.sub('', text)

    text = unicodedata.normalize("NFKC", text)
    text = htmlmod.unescape(text)

    # Zero-width characters and byte-order marks survive NFKC and are
    # invisible in an editor.  A true joiner (ZWNJ/ZWJ) is dropped; the rest
    # become a space, because mid-word they stand in for a lost one
    # ("individuals\ufeffwith diabetes") and deleting them would weld the two
    # words into one token.  The whitespace collapse below tidies the result.
    text = _JOINER_RE.sub('', text)
    text = _ZERO_WIDTH_RE.sub(' ', text)

    # Normalise curly/typographic quotes to ASCII.  Present in 67% of files,
    # and a curly apostrophe silently breaks exact-match lookups of cohort
    # and assay names against the plain-ASCII reference lists.
    text = text.translate(_QUOTE_TRANSLATION)

    text = text.replace('\xa0', ' ')
    text = re.sub(r'\s+', ' ', text).strip()

    # Rejoin exponents that superscript flattening split with a space:
    # "p < 2.5 x 10 -8" -> "p < 2.5 x 10-8".  Affects ~65% of methods files;
    # left alone the detached exponent reads as its own numeric token and no
    # p-value parser can recover the magnitude.
    text = _SPLIT_EXPONENT_RE.sub(r'10\1\2', text)

    # Remove section numbering at start of paragraphs
    text = re.sub(r'^(\d+\.)+\d*\s*', '', text)
    text = re.sub(r'\.\s+(\d+\.)+\d*\s+', '. ', text)

    # Clean up punctuation artifacts from removed citations
    text = re.sub(r'([,;.])\s*([,;.])', r'\2', text)
    text = re.sub(r'\s+([,;.])', r'\1', text)

    # Remove author citations, with or without a trailing year:
    #   "(Smith et al.)"  "(Mahajan et al. 2011)"  "(Woo et al, 2014)"
    # The year alternative is kept separate from the name character class so a
    # digit can never be absorbed into the author list.
    text = re.sub(
        r'\(\s*[A-Z][A-Za-z\s&,;.\'\-]*?et\s+al\.?'
        r'(?:\s*[,;]?\s*(?:19|20)\d{2}[a-z]?)?'
        r'\s*[,;.]?\s*\)',
        '', text,
    )

    # Remove superscript reference markers that a converter already flattened
    # onto the preceding word ("populations10,11,12").  Only reachable for
    # BioC/Auto-CORPus input, where the markup is gone by the time we see the
    # text; every format that still has markup is handled structurally by
    # flatten_without_citations() instead.
    text = _strip_glued_refs(text)

    # Citation numbers stranded after "(toolname)" or "(version)".
    text = _strip_trailing_refs(text)

    # Remove numbered bracket citations: "[20]", "[1,2]", "[3-5]", "[7, 9, 11]".
    # Integers only, so decimal intervals such as "[1.2, 3.4]" and notation
    # like "[Ca2+]" are left alone.  A preceding space is consumed so
    # "risk [20], we" closes up to "risk, we".
    text = re.sub(
        r'\s*\[\s*\d+\s*(?:[,;–—\-]\s*\d+\s*)*\]',
        '', text,
    )

    # Remove hanging Nature-style reference parentheticals left behind when the
    # citation inside them was stripped: "(ref.)", "(refs)", "(refs,)",
    # "(refs. -)".  Only punctuation and whitespace may follow "ref"/"refs",
    # so an intact citation ("(ref. 98)", "(refs. 6,18,19)") and unrelated
    # parentheticals ("(reference number 11/NW/0382)") are left alone.  The
    # leading space is consumed so "IGF2BP2 (refs,)." closes to "IGF2BP2.".
    text = re.sub(r'\s*\(\s*refs?\s*[-–—,;.\s]*\)', '', text,
                  flags=re.IGNORECASE)

    # Remove empty brackets
    text = re.sub(r'\[\s*[,;–—\-\s]*\s*\]', '', text)
    text = re.sub(r'\(\s*[,;–—\-\s]*\s*\)', '', text)
    text = re.sub(r'\s+', ' ', text).strip()

    # Repair parentheses left holding stray punctuation by the removals above,
    # e.g. "regions (, www.t1dbase.org)" or "plot (; Figure 4)".
    text = re.sub(r'\(\s*[,;]\s*', '(', text)
    text = re.sub(r'\s*[,;]\s*\)', ')', text)
    text = re.sub(r'\(\s*\)', '', text)

    # Clean trailing punctuation artifacts
    text = re.sub(r'[,;]\s*[–—\-]\s*[,;.]', '.', text)
    text = re.sub(r'[,;]\s*[–—\-]\s*$', '.', text)
    text = re.sub(r'[–—\-]\s*[,;.]', '.', text)

    # Fix bracket spacing
    text = re.sub(r'\(\s+', '(', text)
    text = re.sub(r'\s+\)', ')', text)
    text = re.sub(r'\[\s+', '[', text)
    text = re.sub(r'\s+\]', ']', text)

    # Final cleanup
    text = re.sub(r'\s+([,;.:!?])', r'\1', text)
    text = re.sub(r'\s+', ' ', text).strip()
    text = re.sub(r'\.{2,}', '.', text)
    text = re.sub(r',{2,}', ',', text)

    return text


# ---------------------------------------------------------------------------
# TEI XML extraction (GROBID .pdf.tei.xml files)
# ---------------------------------------------------------------------------

TEI_NS = {'tei': 'http://www.tei-c.org/ns/1.0'}

# Section titles that indicate the end of the methods section
NON_METHODS_SECTIONS = {
    'result', 'results', 'discussion', 'conclusion', 'conclusions',
    'acknowledgment', 'acknowledgments', 'acknowledgement', 'acknowledgements',
    'references', 'bibliography', 'funding', 'competing interests',
    'conflict of interest', 'disclosure', 'data availability',
    'supplementary', 'supplemental', 'appendix',
    'resource availability', 
    'lead contact',
    'materials availability', 'data and materials availability',
    'data and code availability',
    'data and code availability statement',
    'code availability',
    'key resources table'
}

# Fallback keywords: substrings that, when they appear in a section head,
# strongly suggest the head is a methods subsection. Used only when GROBID
# fails to produce a top-level "Methods" head — which happens on PDFs where
# the section hierarchy gets flattened during parsing.
METHODS_SUBSECTION_KEYWORDS = (
    'participants', 'subjects', 'study population', 'study subjects',
    'cohort', 'discovery cohort', 'replication cohort',
    'genotyping', 'genotype calling', 'quality control',
    'imputation', 'gwas', 'genome-wide association',
    'statistical analysis', 'statistical method',
    'phenotype', 'phenotyping',
    'dna extraction', 'sample collection', 'data collection',
    'study design', 'replication study', 'meta-analysis',
)

# Heads whose lowercased title starts with any of these are clearly NOT
# methods content — used to prevent the fallback from latching onto a
# "Discussion of GWAS findings"-style head.
FALLBACK_EXCLUDED_PREFIXES = (
    'abstract', 'introduction', 'background',
    'result', 'results', 'discussion', 'conclusion', 'conclusions',
    'acknowledg', 'reference', 'references', 'bibliography',
    'funding', 'disclosure', 'conflict', 'data availability',
    'supplement', 'supplemental', 'appendix',
)


def extract_tei_methods(root):
    """
    Extract methods section from a TEI (GROBID) XML document.

    TEI structure: <body> contains flat sibling <div> elements.
    The Methods <div> typically has just a <head> with no paragraphs;
    the actual content is in subsequent sibling <div> elements until
    the next major section (Results, Discussion, etc.).

    Returns extracted text string, or None if no methods section found.
    """
    body = root.find('.//tei:body', TEI_NS)
    if body is None:
        # Try without namespace
        body = root.find('.//body')
    if body is None:
        return None

    divs = list(body.findall('tei:div', TEI_NS))
    if not divs:
        divs = list(body.findall('div'))
    if not divs:
        return None

    # Find the index of the Methods div
    methods_start = None
    for i, div in enumerate(divs):
        head = div.find('tei:head', TEI_NS)
        if head is None:
            head = div.find('head')
        if head is not None and head.text:
            title = head.text.strip().lower()
            # Match "methods", "materials and methods", "methods and materials", etc.
            if ('method' in title and
                not title.startswith('result') and
                not title.startswith('discussion')):
                methods_start = i
                break

    # Fallback: GROBID sometimes flattens the section hierarchy and drops the
    # top-level "Methods" head, leaving only its subheadings (e.g. "Study
    # participants", "Genotyping", "GWAS"). If the first pass found nothing,
    # walk the divs again looking for heads that contain one of the
    # methods-subsection keywords.
    if methods_start is None:
        for i, div in enumerate(divs):
            head = div.find('tei:head', TEI_NS)
            if head is None:
                head = div.find('head')
            if head is None or not head.text:
                continue
            title = head.text.strip().lower()
            # Strip leading numeric prefixes like "2.1 " or "3. ".
            title_clean = re.sub(r'^(\d+\.)*\d+\s+', '', title)
            if any(title_clean.startswith(p) for p in FALLBACK_EXCLUDED_PREFIXES):
                continue
            if any(kw in title_clean for kw in METHODS_SUBSECTION_KEYWORDS):
                methods_start = i
                break

    if methods_start is None:
        return None

    # Collect this div and all subsequent sibling divs until a non-methods section
    text_parts = []
    for i, div in enumerate(divs[methods_start:], start=methods_start):
        head = div.find('tei:head', TEI_NS)
        if head is None:
            head = div.find('head')

        clean_title = ''
        if head is not None and head.text:
            # Normalise the head before doing anything with it: strip leading
            # numbering ("2.1 ") and trailing punctuation.  GROBID routinely
            # emits heads with a trailing period ("GWAS.", "VISP participants."),
            # and without the rstrip those heads (a) never match the stop words
            # below, so collection runs past the end of the methods section, and
            # (b) produce ".." once the ". " separator is appended.
            clean_title = re.sub(r'^(\d+\.)*\d+\s+', '', head.text.strip()).rstrip(' .;,:')
            title = clean_title.lower()

            # Stop if we've hit a non-methods section (but not on the first div)
            if i > methods_start and title:
                if title in NON_METHODS_SECTIONS or any(
                        title.startswith(stop_word + ' ')
                        for stop_word in NON_METHODS_SECTIONS):
                    break

        if clean_title:
            text_parts.append(clean_title + '. ')

        # Extract paragraphs from this div
        for p in div.findall('tei:p', TEI_NS) or div.findall('p'):
            para_text = flatten_without_citations(p).strip()
            if para_text:
                text_parts.append(clean_extracted_text(para_text) + ' ')

    # Clean the joined string, not just the individual parts: artifacts that
    # straddle a join (a citation stripped across a paragraph boundary leaving
    # " ;", doubled spaces) are invisible to the per-part cleaning above.
    result = clean_extracted_text(' '.join(text_parts))
    return result if result else None


# ---------------------------------------------------------------------------
# BioC XML extraction
# ---------------------------------------------------------------------------

def extract_bioc_methods(root):
    """
    Extract methods section from a BioC XML document.

    BioC structure: flat <passage> elements, each with <infon> metadata
    indicating the section. We collect all passages where
    <infon key="section_title_1"> is "Methods" (case-insensitive).

    Fallback: some BioC files (e.g. Oxford Academic via Auto-CORPus) have
    broken section labels where the entire article body is mislabelled as
    "Author notes", "document part", "ACKNOWLEDGEMENTS", etc.  For these,
    we scan the passage text for inline section headers like
    "Materials and Methods" and collect text until the next major section.

    Returns extracted text string, or None if no methods section found.
    """
    # --- Primary strategy: use section_title_1 infon labels ---------------
    text_parts = []

    for passage in root.iter('passage'):
        section_title = None
        for infon in passage.findall('infon'):
            if infon.get('key') == 'section_title_1':
                section_title = infon.text
                break

        if section_title is None:
            continue

        title_lower = section_title.strip().lower()

        # Match methods-related section titles
        if ('method' in title_lower and
            not title_lower.startswith('result') and
            not title_lower.startswith('discussion')):

            text_elem = passage.find('text')
            if text_elem is not None and text_elem.text:
                para_text = text_elem.text.strip()
                if para_text:
                    text_parts.append(clean_extracted_text(para_text) + ' ')

    if text_parts:
        result = clean_extracted_text(' '.join(text_parts))
        return result if result else None

    # --- Fallback: scan passage text for inline section headers -----------
    return _bioc_fallback_inline_headers(root)


# Regex for detecting an inline methods header at the start of passage text
_METHODS_HEADER_RE = re.compile(
    r'^((?:Materials?\s+and\s+)?Methods?'
    r'|Subjects?\s+and\s+Methods?'
    r'|Patients?\s+and\s+Methods?'
    r'|Study\s+Design(?:\s+and\s+Methods?)?'
    r'|Experimental\s+(?:Procedures?|Section))'
    r'\s*[:.]?\s',
    re.IGNORECASE,
)

# Regex for detecting the START of a non-methods section (stops collection)
_NON_METHODS_HEADER_RE = re.compile(
    r'^(Results?'
    r'|Discussion'
    r'|Conclusions?'
    r'|Acknowledg(?:e)?ments?'
    r'|References'
    r'|Funding'
    r'|Data\s+Availability'
    r'|Author\s+Contributions?'
    r'|Supplementary)'
    r'\s*[:.]?\s',
    re.IGNORECASE,
)


def _bioc_fallback_inline_headers(root):
    """
    Fallback for BioC files with broken/missing section labels.

    Scans all passage text for an inline "Methods" header, then collects
    that passage and all subsequent passages until a non-methods header
    (Results, Discussion, etc.) is encountered.
    """
    passages = list(root.iter('passage'))
    methods_start = None

    # Find the first passage whose text begins with a methods header
    for i, passage in enumerate(passages):
        text_elem = passage.find('text')
        if text_elem is None or not text_elem.text:
            continue
        t = text_elem.text.strip()
        if _METHODS_HEADER_RE.match(t):
            methods_start = i
            break

    if methods_start is None:
        return None

    text_parts = []
    for passage in passages[methods_start:]:
        text_elem = passage.find('text')
        if text_elem is None or not text_elem.text:
            continue
        t = text_elem.text.strip()

        # Stop if we hit a non-methods section header (but not on the first passage)
        if passage != passages[methods_start] and _NON_METHODS_HEADER_RE.match(t):
            break

        # Strip the inline header from the first passage
        if passage == passages[methods_start]:
            t = _METHODS_HEADER_RE.sub('', t, count=1).strip()

        if t:
            text_parts.append(clean_extracted_text(t) + ' ')

    # Clean the JOIN, not only each part: every part already ends in ' ', so a
    # plain join left a double space at each paragraph boundary -- the reason
    # BioC files differed from every other path on whitespace.
    result = clean_extracted_text(' '.join(text_parts))
    return result if result else None


def extract_text_from_element(element, parent_tag=None):
    """
    Recursively extract all text content from an XML element,
    including nested elements, while preserving structure with line breaks.
    """
    text_parts = []
    current_tag = element.tag.split('}')[-1] if '}' in element.tag else element.tag
    
    # Check if this is a section with a title
    if current_tag == 'sec':
        # Look for label and title elements
        label_elem = None
        title_elem = None
        
        for child in element:
            child_tag = child.tag.split('}')[-1] if '}' in child.tag else child.tag
            if child_tag == 'label':
                label_elem = child
            elif child_tag == 'title':
                title_elem = child
        
        # Add section header with period at the end
        if label_elem is not None or title_elem is not None:
            header_parts = []
            # Skip label (section numbers) for cleaner sentence tokenization
            # if label_elem is not None:
            #     # Recursively get all text from label
            #     label_text = extract_text_from_element(label_elem, current_tag)
            #     if label_text:
            #         header_parts.append(label_text.strip())
            if title_elem is not None:
                # Recursively get all text from title (including nested elements)
                title_text = extract_text_from_element(title_elem, current_tag)
                if title_text:
                    # Remove leading section numbers from title (e.g., "2.1 GWAS" → "GWAS")
                    import re
                    title_text = re.sub(r'^(\d+\.)*\d+\s+', '', title_text.strip())
                    if title_text:  # Only add if there's text left after removing numbers
                        header_parts.append(title_text)
            
            if header_parts:
                text_parts.append(' '.join(header_parts) + '. ')
        
        # Process other children (except label and title which we already handled)
        for child in element:
            child_tag = child.tag.split('}')[-1] if '}' in child.tag else child.tag
            if child_tag not in ['label', 'title']:
                child_text = extract_text_from_element(child, current_tag)
                if child_text:
                    text_parts.append(child_text)
            
            # Get the tail text (text after the child element)
            if child.tail:
                tail_text = child.tail.strip()
                if tail_text:
                    text_parts.append(tail_text)
    
    # Check if this is a paragraph - add spacing between paragraphs
    elif current_tag == 'p':
        # Get all text from the paragraph
        para_text_parts = []
        
        if element.text:
            para_text_parts.append(element.text.strip())
        
        for child in element:
            # Skip citation references (xref with ref-type="bibr")
            # <ref type="bibr" target="#b1">2</ref> 
            child_tag = child.tag.split('}')[-1] if '}' in child.tag else child.tag
            
            #print(child.get('target'))
            if _is_citation_element(child):
                # Skip this citation, but keep any tail text
                if child.tail:
                    tail_text = child.tail.strip()
                    if tail_text:
                        para_text_parts.append(tail_text)
                continue
          # if child.get('target') and child.get('target')="#b1" and hild.get('ref-type') == 'bibr':
          #       # Skip this citation, but keep any tail text
          #       if child.tail:
          #           tail_text = child.tail.strip()
          #           if tail_text:
          #               para_text_parts.append(tail_text)
          #       continue
            
            child_text = extract_text_from_element(child, current_tag)
            if child_text:
                para_text_parts.append(child_text)
            
            if child.tail:
                tail_text = child.tail.strip()
                if tail_text:
                    para_text_parts.append(tail_text)
        
        # Join paragraph parts and normalize internal whitespace
        import re
        import unicodedata
        
        para_text = ' '.join(filter(None, para_text_parts))
        
        # Normalize unicode
        para_text = unicodedata.normalize("NFKC", para_text)
        import html
        para_text = html.unescape(para_text)
        
        para_text = para_text.replace('\xa0', ' ')  # Replace non-breaking spaces with regular spaces
        para_text = re.sub(r'\s+', ' ', para_text).strip()
        
        # Remove section numbering at start of sentences (e.g., "2.1", "2.3.1", "2.5.6.7")
        para_text = re.sub(r'^(\d+\.)+\d*\s*', '', para_text)  # at start of paragraph
        para_text = re.sub(r'\.\s+(\d+\.)+\d*\s+', '. ', para_text)  # after period
        
        # removing et al. citations 
        #para_text = re.sub(r'\(\s*(?:[^()]*?et al\.[^()]*)\)'), '', para_text)
        
        # Clean up punctuation artifacts from removed citations
        # Remove standalone punctuation like ", ," or ". ,"
        para_text = re.sub(r'([,;.])\s*([,;.])', r'\2', para_text)  # collapse repeated punctuation
        para_text = re.sub(r'\s+([,;.])', r'\1', para_text)  # remove space before punctuation
        
        # Remove incomplete author citations like (Author et al, ) or (Author et al,)
        # Also handles multiple citations: (Author et al,; Author2 et al, )
        para_text = re.sub(r'\([A-Z][a-zA-Z\s&,;.]+et al[,;\s.]*\)', '', para_text)
        
        # Remove empty brackets left by citations: [ ] or [, ] or [ , ] or [, – ]
        # This catches brackets with any combination of spaces, commas, semicolons, dashes
        para_text = re.sub(r'\[\s*[,;–—\-\s]*\s*\]', '', para_text)  # remove empty square brackets
        para_text = re.sub(r'\(\s*[,;–—\-\s]*\s*\)', '', para_text)  # remove empty round brackets
        para_text = re.sub(r'\s+', ' ', para_text).strip()  # normalize whitespace again
        
        # Clean up trailing punctuation artifacts like ",," or ", –," or "–," at end of sentences
        para_text = re.sub(r'[,;]\s*[–—\-]\s*[,;.]', '.', para_text)  # ", –." or "; –," → "."
        para_text = re.sub(r'[,;]\s*[–—\-]\s*$', '.', para_text)  # ", –" at end → "."
        para_text = re.sub(r'[–—\-]\s*[,;.]', '.', para_text)  # "–." or "–," → "."
        
        # Remove extra spaces inside brackets: "( text )" → "(text)"
        para_text = re.sub(r'\(\s+', '(', para_text)  # "( " → "("
        para_text = re.sub(r'\s+\)', ')', para_text)  # " )" → ")"
        para_text = re.sub(r'\[\s+', '[', para_text)  # "[ " → "["
        para_text = re.sub(r'\s+\]', ']', para_text)  # " ]" → "]"
        
        # Final cleanup: normalize any remaining whitespace and trim
        para_text = re.sub(r'\s+([,;.:!?])', r'\1', para_text)  # Remove space before punctuation
        para_text = re.sub(r'\s+', ' ', para_text).strip()
        
        # Replace double (or more) periods with single period
        para_text = re.sub(r'\.{2,}', '.', para_text)  # ".." or "..." → "."
        # Replace double (or more) commas with single comma
        para_text = re.sub(r',{2,}', ',', para_text)  # ",," or ",,," → ","
        
        
        # Add paragraph with space after it
        if para_text:
            text_parts.append(para_text + ' ')
    
    else:
        # For non-section, non-paragraph elements, process normally
        # Skip citation references entirely
        if _is_citation_element(element):
            return ''  # Return empty string for citations
        
        # Skip LaTeX source in tex-math tags (but keep MathML)
        if element.tag.endswith('tex-math'):
            return ''  # Return empty string for LaTeX source
        
        # Skip graphics/images (we already have text from MathML)
        tag = element.tag.split('}')[-1] if '}' in element.tag else element.tag
        if tag in ['graphic', 'inline-graphic']:
            return ''  # Return empty string for images

        # Skip tables and figures entirely (data tables, not narrative text)
        if tag in ['table-wrap', 'table', 'fig', 'disp-formula']:
            return ''  # Return empty string for tables and figures
        
        # Get the element's own text
        if element.text:
            text_parts.append(element.text.strip())
        
        # Process all child elements recursively
        for child in element:
            child_text = extract_text_from_element(child, current_tag)
            if child_text:
                text_parts.append(child_text)
            
            # Get the tail text (text after the child element)
            if child.tail:
                tail_text = child.tail.strip()
                if tail_text:
                    text_parts.append(tail_text)
    
    # Join all parts with spaces
    #result = ' '.join(filter(None, text_parts))
    result = ' '.join(text_parts)
    
    # Final global normalization
    import re
    import unicodedata
    
    result = unicodedata.normalize("NFKC", result)
    result = result.replace('\xa0', ' ')
    result = re.sub(r'\s+', ' ', result).strip()
    
    # remove et al. citations globally (in case any remain)
    result = re.sub(r'\(\s*(?:[^()]*?et al[.,;]\s*[^()]*)\)', '', result)
    result = re.sub(r'\(\s*(?:[^()]*?n.d.\s*[^()]*)\)', '', result)
    result = re.sub(r'\(\s*[A-Z][A-Za-z-]+(?:\s*&\s*[A-Z][A-Za-z-]+)+\s*,?\s*\)','', result)
    result = re.sub(r'\(\s*(?:[A-Z][A-Za-z-]+(?:\s*&\s*[A-Z][A-Za-z-]+)?,\s*(?:n\.d\.|\d{4})\s*;?\s*)+\)', '', result)
    #result = re.sub(r'\(\s*(?:[A-Z][A-Za-z-]+(?:\s*&\s*[A-Z][A-Za-z-]+)?(?:,\s*(?:n\.d\.|\d{4})?)?\s*;?\s*)+\)', '', result)
    #result = re.sub(r'\(\s*(?:[^()]*?et al\.[^()]*)\)', '', result)
    
    # Fix bracket spacing LAST
    result = re.sub(r'\(\s+', '(', result)
    result = re.sub(r'\s+\)', ')', result)
    
    # Replace double (or more) periods with single period
    result = re.sub(r'\.{2,}', '.', result)  # ".." or "..." → "."
    # Replace double (or more) commas with single comma
    result = re.sub(r',{2,}', ',', result)  # ",," or ",,," → ","
    
    # Remove empty brackets left by citations: [ ] or (, ) or [ , ] or ( , )
    result = re.sub(r'\[\s*[,;–—\-\s]*\s*\]', '', result)  # remove empty square brackets
    result = re.sub(r'\(\s*[,;–—\-\s]*\s*\)', '', result)  # remove empty round brackets
    result = re.sub(r'\s+', ' ', result).strip()  # normalize whitespace again
    
    # remove spaces before punctuation
    result = re.sub(r'\s+([,;.:!?])', r'\1', result)  # remove space before punctuation

    return result


def find_methods_section(root):
    """
    Find the methods section in a JATS XML document.
    Returns a tuple of (methods_element, is_main_fallback) or (None, False) if not found.
    
    Searches for methods section in multiple ways:
    1. sec-type="materials|methods" or "materials and methods" attribute
    2. sec-type="methods" attribute (not in abstract, prefer top-level)
    3. title text containing "methods" (case-insensitive)
    
    Skips methods sections inside abstracts (these are summaries, not full methods).
    """
    all_sections = find_all_methods_sections(root)
    
    if _is_main_journal(root) and not all_sections:
        body = root.find('.//{*}body')
        if body is not None:
            return (body, True)
    
    if _is_letter_to_editor(root) and not all_sections:
        body = root.find('.//{*}body')
        if body is not None:
            return (body, True)
          
    return (all_sections[0], False) if len(all_sections) > 0 else (None, False)


def find_all_methods_sections(root):
    """
    Find ALL top-level methods sections in a JATS XML document.
    Returns a list of methods section elements (may be empty).
    
    This is useful for files that have both a stub and a full methods section.
    Only returns top-level sections, not their subsections.
    Filters out author/contributor lists that are incorrectly tagged as methods.
    """
    candidates = []
    
    # Author/team section keywords to skip (these are not real methods)
    author_keywords = [
        'analysis team', 'author', 'contributor', 'writing group',
        'study group', 'consortium', 'working group', 'steering committee',
        'acknowledgment', 'funding', 'competing interest', 'conflict of interest'
    ]
    
    # Helper function to check if a section is inside another section in candidates
    def is_subsection_of_found(sec):
        """Check if sec is a descendant of any section already in candidates"""
        for found_sec in candidates:
            for descendant in found_sec.iter():
                if descendant == sec:
                    return True
        return False
    
    # Helper function to check if section is an author/contributor list
    def is_author_section(sec):
        """Check if this section is an author or contributor list"""
        for child in sec:
            if child.tag.endswith('title'):
                title_text = ''.join(child.itertext()).strip().lower()
                # Check if title matches author/team keywords
                for keyword in author_keywords:
                    if keyword in title_text:
                        return True
                break
        return False
    
    # First try: Look for sections with sec-type="materials|methods" or "materials and methods"
    for sec in root.iter():
        if sec.tag.endswith('sec'):
            sec_type = sec.get('sec-type')
            if sec_type and ('material' in sec_type.lower() and 'method' in sec_type.lower()):
                # Check if in abstract
                for parent in root.iter():
                    if parent.tag.endswith('abstract'):
                        for child in parent.iter():
                            if child == sec:
                                break
                        else:
                            continue
                        break
                else:
                    # Not in abstract, check if it's an author section
                    if not is_subsection_of_found(sec) and not is_author_section(sec):
                        candidates.append(sec)
    
    # Second try: Look for sections with sec-type="methods" (not in abstract)
    methods_candidates = []
    for sec in root.iter():
        if sec.tag.endswith('sec'):
            sec_type = sec.get('sec-type')
            if sec_type and sec_type.lower() == 'methods':
                # Check if in abstract
                is_in_abstract = False
                for parent in root.iter():
                    if parent.tag.endswith('abstract'):
                        for child in parent.iter():
                            if child == sec:
                                is_in_abstract = True
                                break
                        if is_in_abstract:
                            break
                
                if not is_in_abstract:
                    # Count depth (how many sec ancestors)
                    depth = 0
                    for parent in root.iter():
                        if parent.tag.endswith('sec'):
                            for child in parent.iter():
                                if child == sec and child != parent:
                                    depth += 1
                                    break
                    methods_candidates.append((sec, depth))
    
    # Sort by depth and add to candidates
    if methods_candidates:
        methods_candidates.sort(key=lambda x: x[1])
        for sec, depth in methods_candidates:
            if sec not in candidates and not is_subsection_of_found(sec) and not is_author_section(sec):
                candidates.append(sec)
    
    # Third try: Look for sections with title containing "methods" (not in abstract)
    # Common patterns: "Methods", "Materials and Methods", "Methods and Materials"
    for sec in root.iter():
        if sec.tag.endswith('sec'):
            # Check if inside abstract first
            is_in_abstract = False
            for parent in root.iter():
                if parent.tag.endswith('abstract'):
                    for child in parent.iter():
                        if child == sec:
                            is_in_abstract = True
                            break
                    if is_in_abstract:
                        break
            
            if is_in_abstract or sec in candidates or is_subsection_of_found(sec):
                continue
            
            # Look for a title child element
            for child in sec:
                if child.tag.endswith('title'):
                    title_text = ''.join(child.itertext()).strip().lower()
                    # Check if title contains methods-related keywords
                    if ('method' in title_text and 
                        not title_text.startswith('result') and
                        not title_text.startswith('discussion') and
                        # Avoid subsections like "Statistical methods"
                        len(title_text.split()) <= 6):
                        if not is_author_section(sec):
                            candidates.append(sec)
                    break  # Only check first title
    
    # Prioritize sections: prefer explicit "Methods Summary" / "Materials and Methods" titles
    # over generic sec-type="methods" sections that might be author lists
    def section_priority(sec):
        """Return priority score (lower is better)"""
        sec_type = sec.get('sec-type')
        for child in sec:
            if child.tag.endswith('title'):
                title = ''.join(child.itertext()).strip().lower()
                # Demote Nature-style "Online content" / online-methods
                # stubs below every real methods candidate. These are
                # short boilerplate pointers like "Any methods,
                # additional references, ... are available at 10.1038/...".
                if (title == 'online content' or
                    title == 'online methods note' or
                    title.startswith('online content') or
                    title.startswith('online methods note')):
                    return 6
                # Priority 1: Clear methods titles
                if ('methods summary' in title or
                    'materials and methods' in title or
                    'methods and materials' in title):
                    return 1
                # Priority 2: Has sec-type with material+method
                if sec_type and 'material' in sec_type.lower() and 'method' in sec_type.lower():
                    return 2
                # Priority 3: Has sec-type="methods"
                if sec_type and sec_type.lower() == 'methods':
                    return 3
                # Priority 4: Title contains "method"
                if 'method' in title:
                    return 4
                break
        return 5
      
    # Sort by priority
    candidates.sort(key=section_priority)
    
    return candidates


def extract_methods_section(xml_file):
    """
    Extract the methods section text from a JATS, TEI, or BioC XML file.

    Automatically detects the XML format and dispatches to the appropriate
    extraction logic.

    Args:
        xml_file: Path to the XML file

    Returns:
        Dictionary with keys:
        - 'text': String containing the methods section text, or None if not found or if online-only
        - 'is_main': Boolean indicating if this is Nature Genetics fallback body text
    """
    try:
        try:
            tree = ET.parse(xml_file)
            root = tree.getroot()
        except ET.ParseError as parse_err:
            # Some publisher-produced JATS (e.g. SAGE/Atypon) ships malformed
            # XML — namespace prefixes like <oasis:table> are used without
            # being declared on the root element.  ElementTree's strict
            # parser refuses these.  Recovery strategy:
            #   1. Read the file as text.
            #   2. Find every `<prefix:` used in the body but not declared
            #      via `xmlns:prefix=` on the root.
            #   3. Inject placeholder namespace declarations on the root
            #      <article> element, then re-parse with ET.
            #   4. If that still fails, fall back to lxml's recovering
            #      parser and round-trip through serialization.
            print(
                f"Warning: ElementTree could not parse {xml_file} ({parse_err}); "
                f"attempting to repair undeclared namespace prefixes",
                file=sys.stderr,
            )
            raw = Path(xml_file).read_text(encoding='utf-8')

            # Collect every prefix that's actually USED, both as element
            # prefix (`<oasis:table>`) and as attribute prefix
            # (`xlink:href="..."`).  `xml` and `xmlns` are XML's special
            # built-in prefixes — never declare those.
            elem_prefixes = set(re.findall(r'<\s*([A-Za-z][\w.-]*):', raw))
            attr_prefixes = set(re.findall(r'\s([A-Za-z][\w.-]*):[A-Za-z][\w.-]*\s*=', raw))
            used_prefixes = (elem_prefixes | attr_prefixes) - {'xml', 'xmlns'}

            # Map each declared prefix to a URI seen anywhere in the file.
            # Some publishers declare a prefix on inner subtrees only, so a
            # different subtree using the same prefix still raises
            # "unbound prefix".  We re-declare every used prefix on the
            # ROOT element to make the document self-contained.  Re-using
            # the same URI as inner declarations is harmless.
            declared_uris = dict(
                re.findall(r'xmlns:([A-Za-z][\w.-]*)\s*=\s*"([^"]*)"', raw)
            )
            # Well-known fallbacks for prefixes that publishers commonly
            # forget to declare.
            DEFAULT_URIS = {
                'xlink': 'http://www.w3.org/1999/xlink',
                'mml':   'http://www.w3.org/1998/Math/MathML',
                'oasis': 'http://docs.oasis-open.org/ns/oasis-exchange/table',
                'ali':   'http://www.niso.org/schemas/ali/1.0/',
            }

            patched = raw
            if used_prefixes:
                injections = []
                for p in sorted(used_prefixes):
                    uri = declared_uris.get(p) or DEFAULT_URIS.get(p) or f'urn:local:undeclared:{p}'
                    injections.append(f' xmlns:{p}="{uri}"')
                injection = ''.join(injections)
                # Inject into the FIRST element opening tag (the root).
                # Strip any existing xmlns:foo declarations on the root for
                # the same prefixes, so our injection is authoritative.
                def _patch_root(m):
                    head = m.group(1)
                    tail = m.group(2)
                    for p in used_prefixes:
                        head = re.sub(
                            rf'\s+xmlns:{re.escape(p)}\s*=\s*"[^"]*"',
                            '', head,
                        )
                    return head + injection + tail

                patched, n = re.subn(
                    r'(<[A-Za-z][\w.-]*\b[^>]*?)(\s*/?>)',
                    _patch_root,
                    patched,
                    count=1,
                )
                if n:
                    print(
                        f"  injected xmlns declarations for: {sorted(used_prefixes)}",
                        file=sys.stderr,
                    )

            try:
                root = ET.fromstring(patched)
                tree = ET.ElementTree(root)
            except ET.ParseError as second_err:
                # Final fallback: lxml recovering parser.  We strip
                # namespaces from every element so the resulting tree is
                # serializable through ET without re-triggering the same
                # error.
                try:
                    from lxml import etree as LET
                except ImportError:
                    raise second_err
                print(
                    f"  namespace injection insufficient ({second_err}); "
                    f"falling back to lxml recover=True with namespace strip",
                    file=sys.stderr,
                )
                lxml_parser = LET.XMLParser(recover=True, huge_tree=True)
                lxml_root = LET.fromstring(raw.encode('utf-8'), lxml_parser)
                # Strip namespace from element tags so ET can serialize.
                for elem in lxml_root.iter():
                    if isinstance(elem.tag, str) and '}' in elem.tag:
                        elem.tag = elem.tag.split('}', 1)[1]
                LET.cleanup_namespaces(lxml_root)
                root = ET.fromstring(LET.tostring(lxml_root))
                tree = ET.ElementTree(root)

        # Detect format and dispatch
        fmt = detect_xml_format(root)

        if fmt == 'tei':
            return {'text': extract_tei_methods(root), 'is_main': False}

        if fmt == 'bioc':
            return {'text': extract_bioc_methods(root), 'is_main': False}

        if fmt == 'unknown':
            print(f"Warning: could not detect XML format for {xml_file}, trying JATS", file=sys.stderr)

        # JATS format (default) — original logic follows
        # Find the methods section
        methods_section, is_main = find_methods_section(root)
        
        if methods_section is None:
            # Check if methods are in supplementary materials
            supplementary_note = check_supplementary_methods(root)
            if supplementary_note:
                # Methods are in supplementary files - don't write a file
                print("Methods are in supplementary materials (not extracted).", file=sys.stderr)
                return {'text': None, 'is_main': False}
            return {'text': None, 'is_main': False}
        
        # Extract all text from the methods section
        # extract_text_from_element does its own inline tidying, but NOT the
        # shared clean_extracted_text pass, so until now the JATS methods path
        # was the one route that skipped the shared citation cleaning
        # (numbered brackets, flattened superscript markers).  The TEI and
        # BioC paths above, and both sibling extractors, all clean here.
        methods_text = clean_extracted_text(extract_text_from_element(methods_section))
        
        # Final cleanup: strip trailing whitespace
        methods_text = methods_text.strip()
        
        # Check if this is an online-only methods note (Nature journals, etc.)
        # Only flag if short AND clearly states methods are elsewhere
        if methods_text and len(methods_text.split()) < 50:
            lower_text = methods_text.lower()
            
            # Strong indicators that methods are NOT in this document
            # (not just that supplementary info exists)
            strong_indicators = [
                'available in the online version',
                'available at http',
                'available at https',
                'available at 10.',  # DOI references like 10.1038/...
                'available at doi',
                'online content',
            ]
            
            # Additional check: if it starts with phrases indicating redirection
            redirection_starts = [
                'methods and any',
                'any methods',
                'methods are available',
                'methods, including',
            ]
            
            has_strong_indicator = any(indicator in lower_text for indicator in strong_indicators)
            starts_with_redirection = any(lower_text.startswith(phrase) for phrase in redirection_starts)
            
            if has_strong_indicator or starts_with_redirection:
                # This looks like an online-only stub, but check if there's another methods section
                # (Some Nature papers have a stub followed by "ONLINE METHODS")
                all_methods_sections = find_all_methods_sections(root)
                if len(all_methods_sections) > 1:
                    # Try the next methods section
                    for section in all_methods_sections[1:]:
                        alt_text = clean_extracted_text(
                            extract_text_from_element(section)).strip()
                        if alt_text and len(alt_text.split()) >= 50:
                            # Found a real methods section
                            return {'text': alt_text, 'is_main': False}

                # No alternative methods section found.  For older Nature
                # Genetics / Cell Press / AJHG papers, the actual methods text
                # is unsectioned in the body — the only <sec> tagged "Methods"
                # is this online-only stub.  Fall back to the whole body, the
                # same way we do for main-journal articles with zero methods
                # candidates.
                if _is_main_journal(root):
                    body = root.find('.//{*}body')
                    if body is not None:
                        body_text = clean_extracted_text(
                            extract_text_from_element(body)).strip()
                        if body_text and len(body_text.split()) >= 50:
                            return {'text': body_text, 'is_main': True}

                # No alternative found, this is truly online-only
                print("Methods are only available online (not extracted).", file=sys.stderr)
                return {'text': None, 'is_main': False}
        
        return {'text': methods_text, 'is_main': is_main}
        
    except ET.ParseError as e:
        print(f"Error parsing XML file: {e}", file=sys.stderr)
        return {'text': None, 'is_main': False}
    except Exception as e:
        print(f"Error processing file: {e}", file=sys.stderr)
        return {'text': None, 'is_main': False}


def check_supplementary_methods(root):
    """
    Check if methods section is in supplementary materials (common in Cell Press journals).
    Returns a note if supplementary methods are found, None otherwise.
    """
    # Look for supplementary-material sections
    for elem in root.iter():
        tag = elem.tag.split('}')[-1] if '}' in elem.tag else elem.tag
        if tag == 'supplementary-material' or (tag == 'sec' and elem.get('sec-type') == 'supplementary-material'):
            # Check all text in this element for methods references
            all_text = ' '.join(elem.itertext()).lower()
            if 'method' in all_text and ('supplemental' in all_text or 'supplementary' in all_text):
                # Found reference to supplementary methods
                # Try to extract the file name if available
                media_elem = elem.find('.//{*}media')
                if media_elem is not None:
                    href = media_elem.get('{http://www.w3.org/1999/xlink}href')
                    if href:
                        return f"NOTE: Methods section is in supplementary materials file: {href}\n\nThis XML file does not contain the methods text inline. Please refer to the supplementary materials document."
                
                return "NOTE: Methods section is in supplementary materials.\n\nThis XML file does not contain the methods text inline. Please refer to the supplementary materials document."
    
    return None


def main():
    parser = argparse.ArgumentParser(
        description='Extract methods section from JATS XML files'
    )
    parser.add_argument(
        'input_file',
        help='Path to the input JATS XML file'
    )
    parser.add_argument(
        '-o', '--output',
        help='Path to the output text file (if not specified, prints to stdout)'
    )
    
    args = parser.parse_args()
    
    # Extract methods section
    result = extract_methods_section(args.input_file)
    methods_text = result['text']
    is_main = result['is_main']
    
    if methods_text is None:
        print("No methods section found in the XML file.", file=sys.stderr)
        sys.exit(1)
        
    # Output the results
    if args.output:
        output_path = Path(args.output)
        # If this is Nature Genetics main fallback, add '_main' before file extension
        if is_main:
            stem = output_path.stem
            suffix = output_path.suffix
            output_path = output_path.parent / f"{stem}_main{suffix}"

        # NOTE: naming for .pdf.tei.xml inputs (-> *_pdf_tei_methods.txt) is
        # handled by the caller (batch_process_methods.sh). Keep the output
        # path unchanged here so the caller can rely on what it passed in.

        output_path.write_text(methods_text, encoding='utf-8')
        print(f"Methods section extracted to: {output_path}")
    else:
        print(methods_text)
        


if __name__ == '__main__':
    main()
