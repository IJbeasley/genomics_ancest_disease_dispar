"""
Embed GWAS-study text with NCBI's MedCPT Article Encoder.

Outputs (default: output/clustering/):
  - medcpt_embeddings.npy
  - medcpt_embeddings.csv   (PUBMED_ID + V1..Vn columns, for R / other tools)
  - medcpt_pmids.txt
"""
from __future__ import annotations

import json
from optparse import OptionParser
from pathlib import Path

import numpy as np
import pandas as pd
import torch
from pyprojroot import here
from transformers import AutoModel, AutoTokenizer


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------
parser = OptionParser(
        usage="usage: %prog [options]",
        description=(
            "Embed GWAS-study text with NCBI's MedCPT Article Encoder. "
            "Paths default to project-root-relative locations resolved via "
            "pyprojroot."
        ),
    )
parser.add_option(
        "-g", "--gwas-csv",
        dest="gwas_csv",
        type="string",
        help="Path to GWAS Catalog Study CSV (relative path)",
    )
parser.add_option(
        "-t", "--text-dir",
        dest="text_dir",
        type="string",
        help="Directory of *_sentences.json text files (relative path)",
    )
parser.add_option(
        "-o", "--out-path",
        dest="out_path",
        type="string",
        default=str(here("output/clustering")),
        help="Output directory for embeddings [default: %default] (relative path)",
    )
parser.add_option(
        "-m", "--model",
        default = "ncbi/MedCPT-Article-Encoder",
        type="string",
        dest="model_name",
        help="Name/path of the huggingface model used to embed text." 
    )
  
opts, _ = parser.parse_args()

# ---------------------------------------------------------------------------
# Defaults (resolved relative to the project root via pyprojroot)
# ---------------------------------------------------------------------------
# ---- Embedding ----
BATCH_SIZE = 16
MAX_LEN = 512

INFECTIOUS_CAUSES = {
    "HIV/AIDS", "Tuberculosis", "Malaria",
    "Lower respiratory infections", "Diarrhoeal diseases",
    "Neonatal disorders", "Tetanus", "Diphtheria",
    "Pertussis", "Measles", "Maternal disorders",
}

# model string name (for saving output files)
MODEL_NAME = opts.model_name
model_str = MODEL_NAME.split("/")[-1].split("-")[0]
model_str = model_str.lower()

out_path  = here(opts.out_path)

emb_csv = out_path / f"{model_str}_embeddings.csv"


if opts.gwas_csv is None:
    parser.error("--gwas-csv is required")

if opts.text_dir is None:
    parser.error("--text-dir is required")
    
gwas_csv = here(opts.gwas_csv)
text_dir = here(opts.text_dir)

# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------
def load_study_pmids(gwas_csv: Path) -> set[str]:
    try:
        df = pd.read_csv(gwas_csv, encoding="utf-8")
    except UnicodeDecodeError:
        df = pd.read_csv(gwas_csv, encoding="latin-1")
    df.columns = [c.replace(" ", "_") for c in df.columns]
    df = df[~df["cause"].isin(INFECTIOUS_CAUSES)]
    df = df[df["cause"].fillna("") != ""]
    return {str(p) for p in df["PUBMED_ID"].dropna().unique()}


def load_text(
    text_dir: Path, study_pmids: set[str]
) -> tuple[list[str], list[str]]:
    pmids, texts = [], []
    for jf in sorted(text_dir.glob("*_sentences.json")):
        pmid = jf.name.replace("_sentences.json", "")
        if pmid not in study_pmids:
            continue
        try:
            with open(jf) as fh:
                sentences = json.load(fh)
        except Exception as e:
            print(f"  ! could not parse {jf.name}: {e}")
            continue
        if not isinstance(sentences, list):
            continue
        sentences = [s for s in sentences if isinstance(s, str) and s.strip()]
        if not sentences:
            continue
        pmids.append(pmid)
        texts.append(" ".join(sentences))
    return pmids, texts


# ---------------------------------------------------------------------------
# Embedding
# ---------------------------------------------------------------------------
def embed_texts(texts: list[str]) -> np.ndarray:
    device = (
        "cuda" if torch.cuda.is_available()
        else "mps" if torch.backends.mps.is_available()
        else "cpu"
    )
    print(f"Loading {MODEL_NAME} on {device} ...")
    tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)
    model = AutoModel.from_pretrained(MODEL_NAME).to(device)
    model.eval()

    pairs = [["", t] for t in texts]
    all_emb: list[np.ndarray] = []
    with torch.no_grad():
        for i in range(0, len(pairs), BATCH_SIZE):
            batch = pairs[i : i + BATCH_SIZE]
            enc = tokenizer(
                batch,
                truncation=True,
                padding=True,
                return_tensors="pt",
                max_length=MAX_LEN,
            ).to(device)
            out = model(**enc)
            emb = out.last_hidden_state[:, 0, :].cpu().numpy()
            all_emb.append(emb)
            print(f"  embedded {min(i + BATCH_SIZE, len(pairs))}/{len(pairs)}")
    return np.vstack(all_emb)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main(gwas_csv, text_dir, emb_csv) -> None:
    
    study_pmids = load_study_pmids(gwas_csv)
    
    print(f"Eligible studies after filtering: {len(study_pmids)}")
    pmids, texts = load_text(text_dir, study_pmids)
    
    print(f"Texts available for embedding: {len(pmids)}")
    if not pmids:
        raise SystemExit("No texts to embed.")
      
    embeddings = embed_texts(texts)

    # CSV for R / other tools: PUBMED_ID + V1..Vn embedding columns
    emb_df = pd.DataFrame(
        embeddings,
        columns=[f"V{i + 1}" for i in range(embeddings.shape[1])],
    )
    emb_df.insert(0, "PUBMED_ID", pmids)
    emb_df.to_csv(emb_csv, index=False)

    print(f"Saved embeddings (CSV) -> {emb_csv}")


if __name__ == "__main__":
    main(gwas_csv, text_dir, emb_csv)
