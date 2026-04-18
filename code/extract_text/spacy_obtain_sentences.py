# Break article text into sentences using Spacy
# from pydoc import doc
import warnings
import scispacy
import spacy
from spacy.lang.en import English
from typing import List, Dict, Tuple
from difflib import SequenceMatcher
import argparse
import os
import time
import json
import re


# Suppress specific warnings
warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning, message=".*CUDA is not available.*")

# Parse command-line arguments
parser = argparse.ArgumentParser(description="Break articles into sentences using Spacy")
parser.add_argument(
    "--input_dir",
    type=str,
    help="Input directory containing .txt article files"
)
parser.add_argument(
    "--output_dir",
    type=str,
    help="Output directory to save sentence JSON files"
)
parser.add_argument(
  "--model",
  type=str,
  default="en_core_sci_scibert",
  help="Spacy model to use for sentence segmentation (default: en_core_sci_scibert)"
)
args = parser.parse_args()

# let's use the scispacy model for better performance on scientific text
nlp = spacy.load(args.model)

def strip_latex(text: str) -> str:
    """Convert LaTeX math expressions to readable plain text."""
    # Remove \( ... \) and \[ ... \] delimiters
    text = re.sub(r'\\\(|\\\)', '', text)
    text = re.sub(r'\\\[|\\\]', '', text)
    # \text{cFDR} or \\text{cFDR} → cFDR
    text = re.sub(r'\\{1,2}text\{([^}]*)\}', r'\1', text)
    # \log_{10} → log10
    text = re.sub(r'\\(log|ln|exp|sin|cos|tan)\b', r'\1', text)
    # x^{2} → x2 or x^2 → x2
    text = re.sub(r'\^\{([^}]*)\}', r'\1', text)
    text = re.sub(r'\^(\w)', r'\1', text)
    # x_{10} → x10 or x_i → xi
    text = re.sub(r'_\{([^}]*)\}', r'\1', text)
    text = re.sub(r'_(\w)', r'\1', text)
    # \left( → ( and \right) → )
    text = re.sub(r'\\left([(\[|])', r'\1', text)
    text = re.sub(r'\\right([)\]|])', r'\1', text)
    # \frac{a}{b} → a/b
    text = re.sub(r'\\frac\{([^}]*)\}\{([^}]*)\}', r'\1/\2', text)
    # Strip remaining backslashes before commands
    text = re.sub(r'\\(\w+)', r'\1', text)
    # Remove stray { }
    text = re.sub(r'[{}]', '', text)
    # Collapse whitespace
    text = re.sub(r'  +', ' ', text).strip()
    return text


def _normalize(s: str) -> str:
    """Collapse whitespace so trivial differences don't count as mismatches."""
    return re.sub(r'\s+', ' ', s).strip()

def _has_unmatched_brackets(s: str) -> bool:
    """Return True if s contains unmatched ( ) or [ ] brackets."""
    return (s.count('(') != s.count(')') or
            s.count('[') != s.count(']'))

def compare_sentence_splits(
    scibert_sentences: List[str],
    regex_sentences: List[str],
    context: int = 1,
) -> Dict[str, List[Dict]]:
    """
    Compare two sentence-splitting methods and return unmatched sentences
    with their surrounding context.

    Returns a dict with:
      - 'only_in_scibert': sentences the scispacy parser produced that the
        regex splitter did not, each with surrounding sentences from the
        scibert list.
      - 'only_in_regex': the inverse.
      - 'diff_opcodes':  a list of ('replace'|'delete'|'insert', ...) blocks
        from difflib for a side-by-side view.
    """
    sci_norm = [_normalize(s) for s in scibert_sentences]
    rx_norm  = [_normalize(s) for s in regex_sentences]

    sci_set = set(sci_norm)
    rx_set  = set(rx_norm)

    def with_context(sentences: List[str], idx: int) -> Dict:
        lo = max(0, idx - context)
        hi = min(len(sentences), idx + context + 1)
        return {
            "index": idx,
            "sentence": sentences[idx],
            "before": sentences[lo:idx],
            "after":  sentences[idx + 1:hi],
        }

    # Regex fragments with unmatched brackets: scibert likely merged these
    # correctly, so suppress scibert mismatches that contain such a fragment.
    rx_unmatched_fragments = {s for s in rx_norm if s and _has_unmatched_brackets(s)}

    only_in_scibert = [
        with_context(scibert_sentences, i)
        for i, s in enumerate(sci_norm)
        if s
        and s not in rx_set
        and not _has_unmatched_brackets(s)
        and not any(frag in s for frag in rx_unmatched_fragments)
    ]
    only_in_regex = [
        with_context(regex_sentences, i)
        for i, s in enumerate(rx_norm)
        if s and s not in sci_set and not _has_unmatched_brackets(s)
    ]

    # Opcodes align the two lists so you can see replace/insert/delete blocks
    matcher = SequenceMatcher(a=sci_norm, b=rx_norm, autojunk=False)
    diff_opcodes = []
    for tag, i1, i2, j1, j2 in matcher.get_opcodes():
        if tag == "equal":
            continue
        diff_opcodes.append({
            "tag": tag,
            "scibert_block": scibert_sentences[i1:i2],
            "regex_block":   regex_sentences[j1:j2],
            "scibert_range": (i1, i2),
            "regex_range":   (j1, j2),
        })

    return {
        "only_in_scibert": only_in_scibert,
        "only_in_regex":   only_in_regex,
        "diff_opcodes":    diff_opcodes,
    }

def remove_section_numbers(sentences: List[str]) -> List[str]:
    """
    Remove section headers like '2.4.' or '3.1.2.'.
    """
    cleaned = []
    for sent in sentences:
        # Remove section headers like '2.4.' or '3.1.2.'
        cleaned.append(re.sub(r'^\d+(\.\d+)*\.\s*', '', sent))
    return cleaned
  
def merge_lone_punctuation(sentences: List[str]) -> List[str]:
    """
    Merge lone punctuation back onto previous or next sentence.
    """

    merged = []

    punct_pattern = re.compile(r'^[\s\.\,\)\]\}\!\?\u201d\u201c]+$')

    for sent in sentences:
        s = sent.strip()

        # Case 1: lone punctuation → attach to previous sentence
        if merged and punct_pattern.fullmatch(sent):
            merged[-1] = merged[-1].rstrip() + sent.strip()
            continue

        # Case 2: leading opening bracket → attach to previous sentence
        while s and s[0] in '([':
            if merged:
                merged[-1] = merged[-1].rstrip() + ' ' + s[0]
                s = s[1:].lstrip()
            else:
                break  # nothing to attach to

        # Only append remainder if non-empty
        if s:
            merged.append(s)

    return merged

# def merge_lone_punctuation(sentences: List[str]) -> List[str]:
#     """
#     Merge lone punctuation back onto the previous or next sentence as appropriate.
#     """
# 
#     merged = []
# 
#     punct_pattern = re.compile(r'^[\s\.\,\)\]\}\!\?\u201d\u201c]+$')
# 
#     for sent in sentences:
#         s = sent.strip()
# 
#         # Case 1: lone punctuation → attach to previous sentence
#         if merged and punct_pattern.fullmatch(sent):
#             merged[-1] = merged[-1].rstrip() + sent.strip()
#             continue
#         
#         # Case 2: lone opening punctuation (e.g. '(') → attach to next sentence
#         if merged and s and s[0] in '([':
#             merged[-1] = merged[-1].rstrip() + ' ' + s[0]
#             sent = s[1:].lstrip()
#             if not sent:
#                 continue
# 
#         # Case 2: normal sentence → keep
#         merged.append(sent)
# 
#     return merged

# def merge_lone_punctuation(sentences: List[str]) -> List[str]:
#     """Merge lone punctuation (e.g. '.', ')', '.)') back onto the previous sentence, 
#     or the next sentence (where relevant)"""
#     merged = []
#     for sent in sentences:
#         # A 'lone punctuation' sentence: only punctuation/whitespace characters
#         if merged and merged[-1].strip() and re.fullmatch(r'[\s\.\,\)\]\}\!\?\”.\”]+', sent):
#             merged[-1] = merged[-1].rstrip() + sent.rstrip()
#         elif merged and merged[-1].strip() and re.fullmatch(r'[\s(]', sent):
#             merged.append(sent)
#     return merged

def merge_sentences_starting_with_conjunction(sentences: List[str]) -> List[str]:
    """
    Merge sentences that start with lowercase "and", "or", "but" 
    back onto the previous sentence.
    """
    CONJUNCTIONS = ('and', 
                    'or', 
                    'but')
    
    merged = []
    for sent in sentences:
        # If the current sentence starts with a lower case conjunction,
        # merge it with the previous sentence
        # as long as merged is not empty, 
        # and the last merged sentence is not just whitespace
        if sent.lstrip().startswith(CONJUNCTIONS) and merged and merged[-1].strip():
            merged[-1] = merged[-1].rstrip() + ' ' + sent.strip()
        else:
            merged.append(sent)
    return merged

def merge_sentences_ending_with_conjunction(sentences: List[str]) -> List[str]:
    """
    Merge sentences that end with "and", "or", "but" 
    back onto the next sentence.
    """

    CONJUNCTIONS = ('and', 
                    'or', 
                    'but')

    merged = []
    for sent in sentences:
        # If the prior sentence ends with a conjunction, merge it with the current sentence
        # as long as merged is not empty,
        # and the last merged sentence is not just whitespace
        if merged and merged[-1].strip() and merged[-1].rstrip().endswith(CONJUNCTIONS) and sent.strip():
            merged[-1] = merged[-1].rstrip() + ' ' + sent.lstrip()
        else:
            merged.append(sent)
    return merged

def merge_continuation_starts(sentences: List[str]) -> List[str]:
    """
    Merge sentences that start with math/symbol characters 
    back onto the previous sentence.
    """
    CONTINUATION_STARTS = ('<', '×', '=', '>', 
                           '≤', '≥', '±', '+', 
                           '−', '–', '~', "-",
                           ",", ":", ";",)
    
    merged = []
    for sent in sentences:
        if merged and merged[-1].strip() and sent.lstrip().startswith(CONTINUATION_STARTS):
            merged[-1] = merged[-1].rstrip() + ' ' + sent.strip()
        else:
            merged.append(sent)
    return merged

def merge_continuation_ends(sentences: List[str]) -> List[str]:
    """
    Merge sentences that end with math/symbol 
    (indicating a word break) onto the next sentence.
    """
    CONTINUATION_ENDS = ('<', '×', '=', '>', 
                         '≤', '≥', '±', '+', 
                         '−', '–', '~', "-",
                         ",", ":", ";", "vs.", "e.g.", "i.e."
                         )

    merged = []
    for sent in sentences:
        if merged and merged[-1].strip() and merged[-1].rstrip().endswith(CONTINUATION_ENDS):
           merged[-1] = merged[-1].rstrip() + ' ' + sent.lstrip()
        else:
            merged.append(sent)
    return merged
  
def merge_bracket_continuations(sentences: List[str]) -> List[str]:
  """
  Merge sentences that end with a bracket (no full stop), and the next
  sentence starts with a lowercase letter, back together.
  
  i.e. merge this: 
    "Using the same covariates as in the joint model, we used PLINK 2 (ref. 98)",
  "to analyze the association of the corresponding index variant with protein levels in each cohort.",
  """
  merged = []
  for sent in sentences:
    stripped = sent.lstrip()
    
    if merged and merged[-1].strip() and re.match(r'.*[\)\]]\s*$', merged[-1]) and stripped and stripped[0].islower():
        merged[-1] = merged[-1].rstrip() + ' ' + sent.strip()
        
    else:
        merged.append(sent)
  return merged

def merge_sentences_starting_with_parenthetical(sentences: List[str]) -> List[str]:
  """
  Merge sentences that start with a parenthetical (e.g. ") analysis.") back onto the previous sentence.
  """
  merged = []
  for sent in sentences:
    if merged and merged[-1].strip() and re.match(r'^[\)\]]\s*', sent) and merged[-1].rstrip() and not merged[-1].rstrip().endswith('.'):
        merged[-1] = merged[-1].rstrip() + ' ' + sent.strip()
    else:
        merged.append(sent)
  return merged

def fix_sentences_starting_with_quotation(sentences: List[str]) -> List[str]:
  """
  If sentence starts with '” [A-Z]', 
  and the previous sentence ends with a full stop, and contains an unmatched starting quotation '“',
  merge '” ' onto the previous sentence, and remove the '” ' from the start of the current sentence.
  """
  merged = []
  for sent in sentences:
    if merged and merged[-1].strip() and re.match(r'^”\s*[A-Z]', sent) and merged[-1].rstrip().endswith('.') and '“' in merged[-1]:
        merged[-1] = merged[-1].rstrip() + '”'
        merged.append(sent.lstrip('”').strip())
    else:
        merged.append(sent)
  return merged


def merge_brackets(sentences: List[str]) -> List[str]:
  """
  Where a sentence has more opening than closing brackets, 
  and the next sentence has more closing than opening brackets, merge them together.
  """
  merged = []
  for sent in sentences:
    # sentence contains opening brackets
    if sent.count('(') + sent.count('[') > sent.count(')') + sent.count(']'):
        merged[-1] = merged[-1].rstrip() + ' ' + sent.strip()
    else:
        merged.append(sent)
  return merged



def merge_unmatched_closing_brackets(sentences: List[str]) -> List[str]:
    """
    Merge short sentences (<50 chars) that end with an unmatched closing
    bracket/paren back onto the previous sentence.
    """

    def has_unmatched_close(text: str) -> bool:
        stripped = text.strip()
        if not stripped or not stripped.endswith((')', ']', ').', '].')):
            return False

        balance = 0
        for ch in stripped:
            if ch in '([':
                balance += 1
            elif ch in ')]':
                balance -= 1

        return balance < 0  # more closing than opening

    merged = []

    for sent in sentences:
        s = sent.strip()

        if (
            merged
            and merged[-1].strip()
            and len(s) < 50
            and has_unmatched_close(s)
        ):
            merged[-1] = merged[-1].rstrip() + ' ' + s.lstrip()
        else:
            merged.append(sent)

    return merged

# def merge_unmatched_closing_brackets(sentences: List[str]) -> List[str]:
#     """
#     Merge short sentences (<50 chars) that end with an unmatched closing
#     bracket/paren back onto the previous sentence.
#     """
#     def has_unmatched_close(text: str) -> bool:
#         """
#         Return True if text ends with ) or ] and has no matching open bracket.
#         """
#         stripped = text.strip()
#         if not stripped or stripped[-1] not in (').', '].', ')', ']'):
#             return False
#         opens  = stripped.count('(') + stripped.count('[')
#         closes = stripped.count(')') + stripped.count(']')
#         return closes > opens
# 
#     merged = []
#     for sent in sentences:
#         if merged and merged[-1].strip() and len(sent.strip()) < 50 and has_unmatched_close(sent):
#             merged[-1] = merged[-1].rstrip() + ' ' + sent.strip()
#         else:
#             merged.append(sent)
#     
#     return merged

def remove_trailing_numbers_after_period(sentences: List[str]) -> List[str]:
    """
    Remove trailing citation patterns like:
    - ".30" or ".30-40", or "30,40" at the end of sentence,
    (only if NOT preceded by a digit, e.g. to preserve "v2.1")
    - [15].
    """
    cleaned = []
    
    for sent in sentences:
        # Remove trailing citation patterns like: [15]
        sent = re.sub(r'\[\d+\]$', '', sent)
        sent = re.sub(r'\[\d+\]\.$', '.', sent)

        # Remove .30 / .30-40 / .30,40 (but not 2.1)
        sent = re.sub(r'(?<!\d)\.(\d+(?:[-,]\d+)*)\s*$', '.', sent)
        
        cleaned.append(sent)
        
    return cleaned
  
  
def remove_starting_numbers(sentences: List[str]) -> List[str]:
    """
    Remove leading citation markers:
      - plain numbers:    "1 Sentence..."  or  "3, 5, 6 Sentence..."
      - bracketed numbers: "(18) Sentence..." or "(21) Obesity..."
        (only when followed by a capital letter, to avoid removing
         legitimate parenthetical values like "(18) ml of solution")
    """
    cleaned = []
    for sent in sentences:
        # Remove bracketed citation like "(18) " or "(3,5) " before a capital letter
        sent = re.sub(r'^\s*\(\d+(?:[,\s]\d+)*\)\s+(?=[A-Z])', '', sent)
        # Remove plain citation numbers like "1 " or "3, 5, 6 "
        sent = re.sub(r'^\s*\d+(?:[-,]\d+)*\s+(?=[A-Z])', '', sent)
        cleaned.append(sent)
    return cleaned
  
def remove_inline_citations(sentences: List[str]) -> List[str]:
    """Remove mid-sentence citation markers like [15], [15, 16-18], etc."""
    cleaned = []
    for sent in sentences:
        # Square brackets — safe to remove broadly
        sent = re.sub(r'\s*\[\d+(?:\s*[,\-–]\s*\d+)*\]', '', sent)
        # Parenthetical citations after a word/period
        sent = re.sub(r'(?<=[a-zA-Z.])\s*\(\d+(?:\s*[,\-–]\s*\d+)*\)', '', sent)
        # Collapse any double-spaces left behind
        sent = re.sub(r'  +', ' ', sent).strip()
        cleaned.append(sent)
    return cleaned
  

def clean_sentences(sentences: List[str]) -> List[str]:
    """Fix common spacy splitting issues."""
    sentences = [s.strip() for s in sentences if s.strip()]
    
    #  # remove \n in sentences (spacy sometimes leaves these in)
    sentences = [s.replace("\n", " ") for s in sentences]

    # Further split long sentences on ". " followed by a capital letter,
    # but avoid splitting on common abbreviations.
    split_regex = re.compile(
        r"(?<!St\.)(?<!Fig\.)(?<!no\.)(?<!nos\.)(?<!Nos\.)(?<!No\.)(?<!vs\.)(?<!inc\.)(?<!i\.e\.)(?<!et\.al\.)(?<!e\.g\.)(?<!Inc\.)(?<!Co\.)"
        r"(?<=[a-z]\.)\s+(?=[A-Z])"
    )
    
    split_sentences = []
    
    for sent in sentences:
        sent = sent.strip()
          
        if len(sent) > 50:
           parts = [p.strip() for p in split_regex.split(sent) if p.strip()]
           split_sentences.extend(parts) 
        
        else:
          split_sentences.append(sent)

    sentences = split_sentences
    
    # fix quotation sentences:
    sentences = fix_sentences_starting_with_quotation(sentences)
    
    # merge sentences that are just punctuation, e.g. "."
    sentences = merge_lone_punctuation(sentences)  
    # merge sentences that start with math/symbol characters (e.g. '= 0.05') back onto the previous sentence
    sentences = merge_continuation_starts(sentences)
    # merge sentences that end with math/symbol characters (e.g. 'p =') back onto the next sentence
    sentences = merge_continuation_ends(sentences)
    
    # merge sentences that start with "and", "or", "but" back onto the previous sentence
    sentences = merge_sentences_starting_with_conjunction(sentences)
    # merge sentences that end with "and", "or", "but" back onto the
    sentences = merge_sentences_ending_with_conjunction(sentences)
    
    # merge sentences that end with an unmatched closing bracket/paren back onto the previous sentence
    sentences = merge_unmatched_closing_brackets(sentences)
    # merge unmatched brackets back together
    sentences = merge_brackets(sentences)

    # remove section headers (e.g. '2.4.', '3.1.2.' and the short title-case header sentence that typically follows them)
    sentences = remove_section_numbers(sentences)

    # remove trailing numbers after periods, i.e. not caught citations
    sentences = remove_trailing_numbers_after_period(sentences)
    
    # remove starting numbers that are citations, e.g. "1 ", "3, 5, 6"
    sentences = remove_starting_numbers(sentences)
    # remove inline citations like [15], [15, 16-18], etc.
    sentences = remove_inline_citations(sentences)
    
    sentences = merge_unmatched_closing_brackets(sentences)
    sentences = merge_bracket_continuations(sentences)
    sentences = merge_sentences_starting_with_parenthetical(sentences)

    # drop sentences that are just blank "" or " "
    sentences = [s for s in sentences if s.strip()]

    return sentences

def split_text_into_chunks(text: str, max_tokens: int = 400) -> List[str]:
    """
    Split text into chunks that stay within the model's token limit.

    Splits first on double newlines (paragraphs), then on single newlines,
    then on sentence-ending punctuation if any individual unit still
    exceeds max_tokens. Token count is estimated using the scispacy
    tokenizer (whitespace + punctuation), which is a close proxy for
    BERT sub-word tokens without needing the full model vocab.

    max_tokens is set conservatively at 400 to stay safely under
    SciBERT's 512 limit after sub-word expansion.
    """
    def token_count(s: str) -> int:
        # Fast proxy: count whitespace-separated words.
        # Sub-word tokenisers typically expand by ~1.2–1.5x, so
        # a budget of 400 words keeps us under 512 BERT tokens.
        return len(s.split())

    def split_unit(text_unit: str) -> List[str]:
        """Recursively split a unit until every piece is within budget."""
        if token_count(text_unit) <= max_tokens:
            return [text_unit]
        # Try splitting on single newlines first
        parts = text_unit.split("\n")
        if len(parts) > 1:
            return _pack(parts)
        # Fall back to splitting on sentence-ending punctuation
        parts = re.split(r'(?<=[.!?])\s+', text_unit)
        if len(parts) > 1:
            return _pack(parts)
        # Last resort: hard split by word count
        words = text_unit.split()
        return [" ".join(words[i:i + max_tokens])
                for i in range(0, len(words), max_tokens)]

    def _pack(units: List[str]) -> List[str]:
        """Greedily pack units into chunks without exceeding max_tokens."""
        chunks = []
        current = ""
        for unit in units:
            if token_count(current) + token_count(unit) <= max_tokens:
                current = (current + "\n" + unit).strip() if current else unit
            else:
                if current:
                    chunks.append(current)
                # The unit itself may still be too large — recurse
                if token_count(unit) > max_tokens:
                    chunks.extend(split_unit(unit))
                    current = ""
                else:
                    current = unit
        if current:
            chunks.append(current)
        return chunks

    paragraphs = text.split("\n\n")
    return _pack(paragraphs)

def break_text_into_sentences(input_dir: str, pubmed_id:str, output_dir: str) -> List[str]:
    # Read the file
    file_name = f"{input_dir}/{pubmed_id}.txt"
    
    # Check if file exists, 
    # if doesn't exists, try bioC version of the file, or main methods
    if not os.path.isfile(file_name):
        file_name = f"{input_dir}/{pubmed_id}_bioc.txt"
          
    if not os.path.isfile(file_name):
       file_name = f"{input_dir}/{pubmed_id}_main_methods.txt"
    
    if not os.path.isfile(file_name):
        print(f"Warning: File not found for PMID {pubmed_id}: {file_name}")
        return []
      
    
    with open(file_name, 'r', encoding='utf-8') as f:
        file_text = f.read()
        
    # remove space between cis and -eQTL in the text, as spacy often splits these into separate sentences
    file_text = re.sub(r'\bcis\s*-\s*eQTL\b', 'cis-eQTL', file_text)
    
    # remove space between 	p and ‐value in the text, as spacy often splits these into separate sentences
    file_text = re.sub(r'\bp\s*[-‐]\s*value\b', 'p-value', file_text)
    
    # convert LaTeX math expressions to readable plain text.
    file_text = strip_latex(file_text)
    
    # remove \n in sentences (spacy sometimes leaves these in)
    file_text = file_text.replace("\n", " ")
    
    # remove □ character
    file_text = file_text.replace("□", " ")
    
    # Split into chunks if too long (to avoid BERT token limit of 512)
    chunks = split_text_into_chunks(file_text, max_tokens=400)
    
    all_sentences = []
    for chunk in chunks:
        try:
            # Process text with spaCy
            doc = nlp(chunk)
            # Extract sentences
            sentences = [sent.text.strip() for sent in doc.sents]
            all_sentences.extend(sentences)
        except RuntimeError as e:
            print(f"Warning: Could not process chunk in {pubmed_id}: {e}")
            continue
    
    sentences = all_sentences
    sentences = clean_sentences(sentences)
    
    # compare to splitting on punctuation followed by capital letter (a common heuristic for sentences)
    # _SENT_SPLIT_RE = re.compile(r'(?<=[.!?])\s+(?=[A-Z0-9\(\[])')
    # regex_sentences = _SENT_SPLIT_RE.split(file_text)
    # regex_sentences = clean_sentences(regex_sentences)   # apply same cleaners for a fair comparison
    # regex_sentences = [s for s in regex_sentences if s.strip()]
    # 
    # diff = compare_sentence_splits(sentences, regex_sentences, context=1)
    # if diff['only_in_scibert'] or diff['only_in_regex']:
    #   print(f"\n[{pubmed_id}] Sentence split differences:")
    #   print(f"  Only in scibert: {len(diff['only_in_scibert'])} sentences")
    #   print(f"  Only in regex: {len(diff['only_in_regex'])} sentences")
    
    # Save to JSON file
    output_file = f"{output_dir}/{pubmed_id}_sentences.json"
    if output_file:
        with open(output_file, 'w', encoding='utf-8') as out:
            json.dump(sentences, out, ensure_ascii=False, indent=2)
    
############## Loading and processing files ##############

# get all pubmed ids from texts directory
texts_dir = args.input_dir
pubmed_ids = [f.split(".")[0] for f in os.listdir(texts_dir) if f.endswith(".txt")]

print(f"\n Processing {len(pubmed_ids)} files from {texts_dir}...")

start_time = time.time()
for pubmed_id in pubmed_ids:
    try:
        break_text_into_sentences(args.input_dir, pubmed_id, args.output_dir)
    except Exception as e:
        print(f"Error processing {pubmed_id}: {e}")

elapsed_minutes = (time.time() - start_time) / 60
print(f"\n Completed in {elapsed_minutes:.2f} minutes \n\n")

