# Break abstract text into sentences using Spacy
from pydoc import doc
import warnings
import scispacy
import spacy
from spacy.lang.en import English
from typing import List
import argparse
import os
import time
import json
import re


# Suppress specific warnings
warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning, message=".*CUDA is not available.*")

# let's use the scispacy model for better performance on scientific text
load_model = spacy.load("en_core_sci_scibert")

# Parse command-line arguments
parser = argparse.ArgumentParser(description="Break abstracts into sentences using Spacy")
parser.add_argument(
    "--input_dir",
    type=str,
    default="output/abstracts",
    help="Input directory containing .txt abstract files (default: output/abstracts)"
)
parser.add_argument(
    "--output_dir",
    type=str,
    default="output/abstracts",
    help="Output directory to save sentence JSON files (default: output/abstracts)"
)
args = parser.parse_args()

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
    """Merge lone punctuation (e.g. '.', ')', '.)') back onto the previous sentence."""
    merged = []
    for sent in sentences:
        # A 'lone punctuation' sentence: only punctuation/whitespace characters
        if merged and merged[-1].strip() and re.fullmatch(r'[\s\.\,\)\]\}\!\?]+', sent):
            merged[-1] = merged[-1].rstrip() + sent.rstrip()
        else:
            merged.append(sent)
    return merged

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
                           '−', '–', '~',
                           ",")
    
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
                         '−', '–', '~', 
                         ",", ":")

    merged = []
    for sent in sentences:
        if merged and merged[-1].strip() and merged[-1].rstrip().endswith(CONTINUATION_ENDS):
            merged[-1] = merged[-1].rstrip()[:-1] + sent.lstrip()
        else:
            merged.append(sent)
    return merged

def merge_unmatched_closing_brackets(sentences: List[str]) -> List[str]:
    """
    Merge short sentences (<50 chars) that end with an unmatched closing
    bracket/paren back onto the previous sentence.
    """
    def has_unmatched_close(text: str) -> bool:
        """
        Return True if text ends with ) or ] and has no matching open bracket.
        """
        stripped = text.strip()
        if not stripped or stripped[-1] not in (').', '].', ')', ']'):
            return False
        opens  = stripped.count('(') + stripped.count('[')
        closes = stripped.count(')') + stripped.count(']')
        return closes > opens

    merged = []
    for sent in sentences:
        if merged and merged[-1].strip() and len(sent.strip()) < 50 and has_unmatched_close(sent):
            merged[-1] = merged[-1].rstrip() + ' ' + sent.strip()
        else:
            merged.append(sent)
    return merged

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
        sent = re.sub(r'\[\d+\]\s*$', '', sent)
        # Remove trailing pattern like ".30" or ".30-40", or ".30,40"    
        # but only if NOT preceded by a digit (to preserve "2.1")
        cleaned.append(re.sub(r'(?<!\d)\.(\d+(?:[-,]\d+)*)\s*$', '.', sent))
    return cleaned

def clean_sentences(sentences: List[str]) -> List[str]:
    """Fix common spacy splitting issues."""
    sentences = [s.strip() for s in sentences if s.strip()]

    # merge sentences that are just punctuation, e.g. "."
    sentences = merge_lone_punctuation(sentences)  
    # merge sentences that start with math/symbol characters (e.g. '= 0.05') back onto the previous sentence
    sentences = merge_continuation_starts(sentences)
    # merge sentences that end with math/symbol characters (e.g. 'p =') back onto the next sentence
    sentences = merge_continuation_ends(sentences)
    # merge sentences that end with an unmatched closing bracket/paren back onto the previous sentence
    sentences = merge_unmatched_closing_brackets(sentences)
    # merge sentences that start with "and", "or", "but" back onto the previous sentence
    sentences = merge_sentences_starting_with_conjunction(sentences)
    # merge sentences that end with "and", "or", "but" back onto the
    sentences = merge_sentences_ending_with_conjunction(sentences)

    # remove section headers (e.g. '2.4.', '3.1.2.' and the short title-case header sentence that typically follows them)
    sentences = remove_section_numbers(sentences)

    # remove trailing numbers after periods, i.e. not caught citations
    sentences = remove_trailing_numbers_after_period(sentences)

    return sentences

def split_text_into_chunks(text: str, max_chars: int = 2000) -> List[str]:
    """Split text into chunks by sentence boundaries to avoid exceeding token limits."""
    # First pass: split by double newlines (paragraphs)
    paragraphs = text.split("\n\n")
    chunks = []
    current_chunk = ""
    
    for para in paragraphs:
        if len(current_chunk) + len(para) > max_chars:
            if current_chunk:
                chunks.append(current_chunk)
            current_chunk = para
        else:
            current_chunk += "\n\n" + para if current_chunk else para
    
    if current_chunk:
        chunks.append(current_chunk)
    
    return chunks

def break_abstract_into_sentences(input_dir: str, pubmed_id:str, output_dir: str) -> List[str]:
    # Read the file
    file_name = f"{input_dir}/{pubmed_id}.txt"
    with open(file_name, 'r', encoding='utf-8') as f:
        file_text = f.read()
    
    # Split into chunks if too long (to avoid BERT token limit of 512)
    chunks = split_text_into_chunks(file_text, max_chars=2000)
    
    all_sentences = []
    for chunk in chunks:
        try:
            # Process text with spaCy
            doc = load_model(chunk)
            
            # Extract sentences
            sentences = [sent.text.strip() for sent in doc.sents]
            all_sentences.extend(sentences)
        except RuntimeError as e:
            print(f"Warning: Could not process chunk in {pubmed_id}: {e}")
            continue
    
    sentences = all_sentences
    sentences = clean_sentences(sentences)
    
    # Save to JSON file
    output_file = f"{output_dir}/{pubmed_id}_sentences.json"
    if output_file:
        with open(output_file, 'w', encoding='utf-8') as out:
            json.dump(sentences, out, ensure_ascii=False, indent=2)
    
    return sentences

############## Loading and processing files ##############

# get all pubmed ids from abstracts directory
abstracts_dir = args.input_dir
pubmed_ids = [f.split(".")[0] for f in os.listdir(abstracts_dir) if f.endswith(".txt")]

print(f"\n Processing {len(pubmed_ids)} files from {abstracts_dir}...")

start_time = time.time()
for pubmed_id in pubmed_ids:
    try:
        break_abstract_into_sentences(args.input_dir, pubmed_id, args.output_dir)
    except Exception as e:
        print(f"Error processing {pubmed_id}: {e}")

elapsed_minutes = (time.time() - start_time) / 60
print(f"\n Completed in {elapsed_minutes:.2f} minutes \n\n")

