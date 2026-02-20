# Break abstract text into sentences using Spacy
from pydoc import doc
import scispacy
import spacy
from spacy.lang.en import English
from typing import List
# load_model = spacy.load("en_core_web_trf")
#load_model = spacy.load('en_core_web_sm')
# let's use the scispacy model for better performance on scientific text


load_model = spacy.load("en_core_sci_scibert")
# use en_core_sci_scibert

# saving sentences to json
import json

def break_abstract_into_sentences(input_dir: str, pubmed_id:str, output_dir: str) -> List[str]:
    # Read the file
    file_name = f"{input_dir}/{pubmed_id}.txt"
    with open(file_name, 'r', encoding='utf-8') as f:
        file_text = f.read()
    
    # Process text with spaCy
    doc = load_model(file_text)
    
    # Extract sentences
    sentences = [sent.text.strip() for sent in doc.sents]
    
    # Save to JSON file
    output_file = f"{output_dir}/{pubmed_id}_sentences.json"
    if output_file:
        with open(output_file, 'w', encoding='utf-8') as out:
            json.dump(sentences, out, ensure_ascii=False, indent=2)
    
    return sentences


# get all pubmed ids from abstracts directory
import os
abstracts_dir = "output/abstracts"
pubmed_ids = [f.split(".")[0] for f in os.listdir(abstracts_dir) if f.endswith(".txt")]

for pubmed_id in pubmed_ids:
    break_abstract_into_sentences("output/abstracts", pubmed_id, "output/abstracts")

