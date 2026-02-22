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


# saving sentences to json
import json

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
    
    # Save to JSON file
    output_file = f"{output_dir}/{pubmed_id}_sentences.json"
    if output_file:
        with open(output_file, 'w', encoding='utf-8') as out:
            json.dump(sentences, out, ensure_ascii=False, indent=2)
    
    return sentences

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

