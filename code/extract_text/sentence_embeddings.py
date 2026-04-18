# After finding sentences with cohort names (see relevant Rmarkdown page)
# In this script,
# identify semantically similar sentences
#  and inspect them to see if they are good hard negatives for 
# training a model to distinguish between cohort and non-cohort sentences. 
# Also look for semantically similar sentences within the cohort set to 
# get a sense of how similar cohort sentences are to each other.
import json
import os
import time
from optparse import OptionParser

import matplotlib.pyplot as plt
import numpy as np
import torch
import torch.nn.functional as F
from transformers import AutoModel, AutoTokenizer

try:
    from tqdm import tqdm
except ImportError:  # tqdm is optional
    tqdm = None

# ---------------------------------------------------------------------------
# Model: NCBI MedCPT Query Encoder
# ---------------------------------------------------------------------------
# MedCPT-Query-Encoder is a BERT-based encoder released by NCBI and trained on
# PubMed user click logs. It is a bi-encoder and produces a single dense
# embedding per input by taking the [CLS] token of the last hidden state.
# Reference: https://huggingface.co/ncbi/MedCPT-Query-Encoder
#
# Notes on usage:
#   - The model was trained with short queries (max_length=64). Longer inputs
#     will still run, but quality may degrade past ~64 tokens. We default to
#     64 to match the training regime; bump MAX_LENGTH if your sentences are
#     longer and you want to trade some accuracy for coverage.
#   - Similarity is cosine similarity on the [CLS] embeddings.
MODEL_NAME = "ncbi/MedCPT-Query-Encoder"
MAX_LENGTH = 64

_device = "cuda" if torch.cuda.is_available() else "cpu"
tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)
model = AutoModel.from_pretrained(MODEL_NAME).to(_device)
model.eval()


def encode_sentences(
    sentences,
    batch_size: int = 64,
    max_length: int = MAX_LENGTH,
    show_progress_bar: bool = True,
) -> torch.Tensor:
    """Encode a list of sentences with MedCPT-Query-Encoder.

    Returns a float tensor of shape (len(sentences), hidden_size) on the
    selected device. Uses the [CLS] token of the final hidden state as the
    sentence embedding, matching the recipe in the MedCPT model card.
    """
    if len(sentences) == 0:
        return torch.empty(0, model.config.hidden_size, device=_device)

    iterator = range(0, len(sentences), batch_size)
    if show_progress_bar and tqdm is not None:
        iterator = tqdm(iterator, desc="Encoding", total=(len(sentences) + batch_size - 1) // batch_size)

    all_embeds = []
    with torch.no_grad():
        for start in iterator:
            batch = sentences[start:start + batch_size]
            encoded = tokenizer(
                batch,
                truncation=True,
                padding=True,
                return_tensors="pt",
                max_length=max_length,
            ).to(_device)
            outputs = model(**encoded)
            # [CLS] pooling – first token of the last hidden state
            embeds = outputs.last_hidden_state[:, 0, :]
            all_embeds.append(embeds)
    return torch.cat(all_embeds, dim=0)


def cosine_similarity_matrix(a: torch.Tensor, b: torch.Tensor) -> torch.Tensor:
    """Row-wise cosine similarity between rows of a and rows of b.

    Returns a tensor of shape (a.shape[0], b.shape[0]).
    """
    a_norm = F.normalize(a, p=2, dim=1)
    b_norm = F.normalize(b, p=2, dim=1)
    return a_norm @ b_norm.T

def run_similarity_analysis(
    label: str,
    cohort_path: str,
    non_cohort_path: str,
    figure_save_path: str,
    percentile_threshold: float,
) -> None:
    cohort_sentences = json.load(open(cohort_path, "r"))
    non_cohort_sentences = json.load(open(non_cohort_path, "r"))

    # Ensure uniqueness
    cohort_sentences = sorted(set(cohort_sentences))
    non_cohort_sentences = sorted(set(non_cohort_sentences))
    # check / confirm that there is no overlap between the two sets
    non_cohort_sentences = [s for s in non_cohort_sentences if s not in set(cohort_sentences)]

    print(f"\n[{label}] Number of cohort sentences: {len(cohort_sentences)}")
    print(f"[{label}] Number of non-cohort sentences: {len(non_cohort_sentences)}")

    cohort_embeddings = encode_sentences(
        cohort_sentences,
        batch_size=64,
        show_progress_bar=True,
    )
    non_cohort_embeddings = encode_sentences(
        non_cohort_sentences,
        batch_size=64,
        show_progress_bar=True,
    )

    # Compute cosine similarities between non-cohort and cohort embeddings
    similarities = cosine_similarity_matrix(non_cohort_embeddings, cohort_embeddings)
    similarities = similarities.cpu().numpy()

    # For each non-cohort sentence, find the most similar cohort sentence and its similarity score
    best_cohort_idx = np.argmax(similarities, axis=1)
    best_similarity = np.max(similarities, axis=1)

    # For each cohort sentence, find the most similar cohort sentence (excluding itself) and its similarity score
    cohort_similarities = cosine_similarity_matrix(cohort_embeddings, cohort_embeddings)
    cohort_similarities = cohort_similarities.cpu().numpy()
    np.fill_diagonal(cohort_similarities, -np.inf)  # Exclude self-similarities
    cohort_best_similarity = np.max(cohort_similarities, axis=1)

    # Print median similarities
    print(f"[{label}] Median similarity of non-cohort sentences to their best cohort match: {np.median(best_similarity):.3f}")
    print(f"[{label}] Median similarity of cohort sentences to their best cohort match: {np.median(cohort_best_similarity):.3f}")

    # Plot histogram comparing best similarities
    plt.figure(figsize=(10, 6))
    plt.hist(cohort_best_similarity, bins=50, alpha=0.6, label='Cohort sentences', color='steelblue', edgecolor='black')
    plt.hist(best_similarity, bins=50, alpha=0.6, label='Non-cohort sentences', color='salmon', edgecolor='black')
    plt.axvline(np.median(cohort_best_similarity), color='steelblue', linestyle='--', linewidth=2, label=f'Cohort median: {np.median(cohort_best_similarity):.3f}')
    plt.axvline(np.median(best_similarity), color='salmon', linestyle='--', linewidth=2, label=f'Non-cohort median: {np.median(best_similarity):.3f}')
    plt.xlabel('Best Similarity to Cohort Sentences')
    plt.ylabel('Frequency')
    plt.title(f'Distribution of Best Similarity Scores to Cohort Sentences ({label})')
    plt.legend()
    plt.grid(axis='y', alpha=0.3)
    plt.tight_layout()
    histogram_path = figure_save_path
    output_dir = os.path.dirname(histogram_path)
    if output_dir:      
        os.makedirs(output_dir, exist_ok=True)
    plt.savefig(histogram_path, dpi=300)
    print(f"\n[{label}] Histogram saved to {histogram_path}")

    # keep non-cohort sentences that fall above the bottom 25% of cohort similarities
    # how many sentences does this select, and what is the ratio of selected hard negatives to cohort sentences?
    #threshold = np.percentile(cohort_best_similarity, 25)
    # hard_negatives_mask = best_similarity >= threshold
    # print(f"[{label}] Threshold: {threshold:.3f}")
    # print(f"[{label}] Hard negatives selected: {hard_negatives_mask.sum()} / {len(similarities)}")
    # print(f"[{label}] Ratio of hard negatives to cohort sentences: {hard_negatives_mask.sum()} / {len(cohort_sentences)}")

    # repeat, but for 50% threshold
    threshold = np.percentile(cohort_best_similarity, percentile_threshold)
    hard_negatives_mask = best_similarity >= threshold
    print("n[{label}] Threshold: {percentile_threshold}th percentile of cohort similarities: {threshold:.3f}")
    print(f"\n[{label}] Threshold: {threshold:.3f}")
    print(f"[{label}] Hard negatives selected: {hard_negatives_mask.sum()} / {len(similarities)}")
    print(f"[{label}] Ratio of hard negatives to cohort sentences: {hard_negatives_mask.sum()} / {len(cohort_sentences)}")

    # inspect some of the hard negatives
    hard_negatives = np.array(non_cohort_sentences)[hard_negatives_mask]
    # randomly sample 10 hard negatives to print
    np.random.seed(42)
    if len(hard_negatives) > 10:
        #hard_negatives_examples = np.random.choice(hard_negatives, size=10, replace=False)
        hard_negatives_examples = (
    np.random.default_rng(42).choice(hard_negatives, size=10, replace=False)
    if len(hard_negatives) > 10 else hard_negatives
)


    print("\nExample hard negatives:")
    for i in range(min(10, len(hard_negatives_examples))):
        print(f"{i+1}. {hard_negatives_examples[i]}")

    # inspect some of the most similar non-cohort sentences to cohort sentences
    most_similar_non_cohort_idx = np.argsort(best_similarity)[-10:]
    print("\nMost similar non-cohort sentences to cohort sentences:")
    for i in most_similar_non_cohort_idx:
        print(f"{i+1}. {non_cohort_sentences[i]} (similarity: {best_similarity[i]:.3f})")

    # inspect some of the least similar non-cohort sentences to cohort sentences
    least_similar_non_cohort_idx = np.argsort(best_similarity)[:10]
    print("\nLeast similar non-cohort sentences to cohort sentences:")
    for i in least_similar_non_cohort_idx:
        print(f"{i+1}. {non_cohort_sentences[i]} (similarity: {best_similarity[i]:.3f})")

    # inspect some of the most similar cohort sentences to other cohort sentences
    most_similar_cohort_idx = np.argsort(cohort_best_similarity)[-10:]
    print("\nMost similar cohort sentences to other cohort sentences:")
    for i in most_similar_cohort_idx:
        print(f"{i+1}. {cohort_sentences[i]} (similarity: {cohort_best_similarity[i]:.3f})")

    # inspect some of the least similar cohort sentences to other cohort sentences
    least_similar_cohort_idx = np.argsort(cohort_best_similarity)[:10]
    print("\nLeast similar cohort sentences to other cohort sentences:")
    for i in least_similar_cohort_idx:
        print(f"{i+1}. {cohort_sentences[i]} (similarity: {cohort_best_similarity[i]:.3f})")

    # save hard negatives to file
    hard_negatives_output_file = f"output/doccano/hard_negatives_{label}.json"
    with open(hard_negatives_output_file, 'w', encoding='utf-8') as out:
        json.dump(hard_negatives.tolist(), out, ensure_ascii=False, indent=2)
    print(f"\n[{label}] Hard negatives saved to {hard_negatives_output_file}")


def parse_args():
    parser = OptionParser()
    parser.add_option(
        "-l",
        "--label",
        dest="label",
        help="Label used in logs and output filenames",
    )
    parser.add_option(
        "-c",
        "--cohort-path",
        "--cohort_path",
        dest="cohort_path",
        help="Path to JSON file containing cohort sentences",
    )
    parser.add_option(
        "-n",
        "--non-cohort-path",
        "--non_cohort_path",
        dest="non_cohort_path",
        help="Path to JSON file containing non-cohort sentences",
    )
    parser.add_option(
        "-s",
        "--figure_save_path",
        dest="figure_save_path",
        default="output/similarities_histogram.png",
        help="Path to save the similarity histogram figure/s",
    )
    parser.add_option(
      "-p",
      "--percentile",
      dest="percentile_threshold",
      default=50.0,
      help = "Percentile threshold for selecting hard negatives (default: 50)"
    )

    options, _ = parser.parse_args()

    missing = []
    if not options.label:
        missing.append("--label")
    if not options.cohort_path:
        missing.append("--cohort-path")
    if not options.non_cohort_path:
        missing.append("--non-cohort-path")

    if missing:
        parser.error(f"Missing required option(s): {', '.join(missing)}")

    return options


if __name__ == "__main__":
    opts = parse_args()
    run_similarity_analysis(
        opts.label,
        opts.cohort_path,
        opts.non_cohort_path,
        opts.figure_save_path,
        float(opts.percentile_threshold)
)



