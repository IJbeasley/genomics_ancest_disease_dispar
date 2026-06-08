"""
Embed GWAS-study abstracts with NCBI's MedCPT Article Encoder.

Outputs (in output/clustering/):
  - medcpt_embeddings.npy
  - medcpt_embeddings.csv   (PUBMED_ID + V1..Vn columns, for R / other tools)
  - medcpt_pmids.txt
"""
from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
import torch
from transformers import AutoModel, AutoTokenizer

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parents[1]
GWAS_CSV = REPO_ROOT / "output" / "icd_map" / "gwas_study_gbd_causes.csv"
ABSTRACT_DIR = REPO_ROOT / "output" / "abstracts"
OUT_DIR = REPO_ROOT / "output" / "clustering"
OUT_DIR.mkdir(parents=True, exist_ok=True)

EMB_OUT = OUT_DIR / "medcpt_embeddings.npy"
EMB_CSV = OUT_DIR / "medcpt_embeddings.csv"
PMID_OUT = OUT_DIR / "medcpt_pmids.txt"

# ---- Embedding ----
MODEL_NAME = "ncbi/MedCPT-Article-Encoder"
BATCH_SIZE = 16
MAX_LEN = 512

INFECTIOUS_CAUSES = {
    "HIV/AIDS", "Tuberculosis", "Malaria",
    "Lower respiratory infections", "Diarrhoeal diseases",
    "Neonatal disorders", "Tetanus", "Diphtheria",
    "Pertussis", "Measles", "Maternal disorders",
}


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------
def load_study_pmids() -> set[str]:
    try:
        df = pd.read_csv(GWAS_CSV, encoding="utf-8")
    except UnicodeDecodeError:
        df = pd.read_csv(GWAS_CSV, encoding="latin-1")
    df.columns = [c.replace(" ", "_") for c in df.columns]
    df = df[~df["cause"].isin(INFECTIOUS_CAUSES)]
    df = df[df["cause"].fillna("") != ""]
    return {str(p) for p in df["PUBMED_ID"].dropna().unique()}


def load_abstracts(study_pmids: set[str]) -> tuple[list[str], list[str]]:
    pmids, texts = [], []
    for jf in sorted(ABSTRACT_DIR.glob("*_sentences.json")):
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
def main() -> None:
    study_pmids = load_study_pmids()
    print(f"Eligible studies after filtering: {len(study_pmids)}")
    pmids, texts = load_abstracts(study_pmids)
    print(f"Abstracts available for embedding: {len(pmids)}")
    if not pmids:
        raise SystemExit("No abstracts to embed.")

    embeddings = embed_texts(texts)

    np.save(EMB_OUT, embeddings)
    PMID_OUT.write_text("\n".join(pmids))

    # CSV for R / other tools: PUBMED_ID + V1..Vn embedding columns
    emb_df = pd.DataFrame(
        embeddings,
        columns=[f"V{i + 1}" for i in range(embeddings.shape[1])],
    )
    emb_df.insert(0, "PUBMED_ID", pmids)
    emb_df.to_csv(EMB_CSV, index=False)

    print(f"Saved embeddings -> {EMB_OUT}")
    print(f"Saved embeddings (CSV) -> {EMB_CSV}")
    print(f"Saved PMIDs -> {PMID_OUT}")


if __name__ == "__main__":
    main()
