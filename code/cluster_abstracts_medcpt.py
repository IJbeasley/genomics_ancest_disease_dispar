"""
Embed GWAS-study abstracts with NCBI's MedCPT Article Encoder and cluster
them using two complementary approaches, side by side:

  A. K-means on UMAP-reduced embeddings, with an improved optimal-k
     selection combining three metrics:
         1. Elbow Method (WCSS) – "kneedle" knee detection
         2. Gap Statistic      – Tibshirani rule
         3. Silhouette Score   – global max
     Final k = median of the three suggestions (robust to outliers).

  B. HDBSCAN on UMAP-reduced embeddings – automatically discovers the
     number of clusters and labels noise points separately. We sweep a
     small grid of min_cluster_size values and pick the configuration
     maximizing DBCV / silhouette.

Outputs (in output/clustering/):
  - medcpt_embeddings.npy, medcpt_pmids.txt
  - cluster_selection_metrics.csv
  - S1_cluster_selection.png           (3-metric figure)
  - S2_clustering_comparison.png       (UMAP scatter, K-means vs HDBSCAN)
  - medcpt_kmeans_clusters.csv
  - medcpt_hdbscan_clusters.csv

Why the change from the previous 4-cluster script:
  - 768-d MedCPT embeddings suffer from the curse of dimensionality;
    UMAP to ~15 dims makes distance-based clustering meaningful.
  - K-means assumes spherical clusters of similar size; HDBSCAN does
    not, which matches the heterogeneous density of GWAS topics.
  - Taking max() of three disagreeing methods was too aggressive; a
    median / consensus is more robust.
"""

from __future__ import annotations

import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import torch
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_samples, silhouette_score
from sklearn.preprocessing import normalize
from transformers import AutoModel, AutoTokenizer

# Optional deps, imported lazily with clear error messages
# try:
#     import umap
# except ImportError as e:
#     raise SystemExit(
#         "Missing dependency 'umap-learn'. Install with: "
#         "pip install umap-learn hdbscan kneed"
#     ) from e

# try:
#     import hdbscan
# except ImportError as e:
#     raise SystemExit(
#         "Missing dependency 'hdbscan'. Install with: "
#         "pip install umap-learn hdbscan kneed"
#     ) from e
# 
# try:
#     from kneed import KneeLocator
#     HAS_KNEED = True
# except ImportError:
#     HAS_KNEED = False


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
METRICS_OUT = OUT_DIR / "cluster_selection_metrics.csv"
FIG_SELECTION = OUT_DIR / "S1_cluster_selection.png"
FIG_COMPARISON = OUT_DIR / "S2_clustering_comparison.png"
KMEANS_OUT = OUT_DIR / "medcpt_kmeans_clusters.csv"
HDBSCAN_OUT = OUT_DIR / "medcpt_hdbscan_clusters.csv"

# ---- Embedding ----
MODEL_NAME = "ncbi/MedCPT-Article-Encoder"
BATCH_SIZE = 16
MAX_LEN = 512

# ---- UMAP ----
UMAP_N_COMPONENTS = 15
UMAP_N_NEIGHBORS = 15
UMAP_MIN_DIST = 0.0
UMAP_METRIC = "cosine"
UMAP_RANDOM_STATE = 42

# ---- K-means sweep ----
K_MIN = 2
K_MAX = 60
K_RANGE = list(range(K_MIN, K_MAX + 1))
GAP_N_REFS = 10

# ---- HDBSCAN sweep ----
HDBSCAN_MIN_SIZES = [3, 5, 8, 10, 15, 20]

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
# K-means selection metrics (on UMAP-reduced, L2-normalized embeddings)
# ---------------------------------------------------------------------------
def compute_wcss(X: np.ndarray) -> list[float]:
    wcss = []
    for k in K_RANGE:
        km = KMeans(n_clusters=k, random_state=42, n_init=10)
        km.fit(X)
        wcss.append(km.inertia_)
    return wcss


def compute_silhouette(X: np.ndarray) -> list[float]:
    scores = []
    for k in K_RANGE:
        km = KMeans(n_clusters=k, random_state=42, n_init=10)
        labels = km.fit_predict(X)
        scores.append(silhouette_score(X, labels))
    return scores


def compute_gap_statistic(
    X: np.ndarray, n_refs: int = GAP_N_REFS
) -> tuple[list[float], list[float]]:
    rng = np.random.default_rng(42)
    mins, maxs = X.min(axis=0), X.max(axis=0)
    n, d = X.shape

    gaps, gap_stds = [], []
    for k in K_RANGE:
        km = KMeans(n_clusters=k, random_state=42, n_init=10)
        km.fit(X)
        log_wk = np.log(km.inertia_)

        ref_log_wks = []
        for _ in range(n_refs):
            ref = rng.uniform(mins, maxs, size=(n, d))
            km_ref = KMeans(n_clusters=k, random_state=42, n_init=10)
            km_ref.fit(ref)
            ref_log_wks.append(np.log(km_ref.inertia_))

        mean_ref = np.mean(ref_log_wks)
        std_ref = np.std(ref_log_wks) * np.sqrt(1 + 1 / n_refs)
        gaps.append(mean_ref - log_wk)
        gap_stds.append(std_ref)
    return gaps, gap_stds


def find_optimal_k(
    wcss: list[float],
    silhouettes: list[float],
    gaps: list[float],
    gap_stds: list[float],
) -> tuple[int, dict]:
    ks = K_RANGE

    # Elbow via kneedle (preferred) or fallback to relative-change threshold
    if HAS_KNEED:
        kn = KneeLocator(
            ks, wcss, curve="convex", direction="decreasing", S=1.0
        )
        elbow_k = int(kn.knee) if kn.knee is not None else ks[len(ks) // 2]
    else:
        # Fallback: find first k where marginal WCSS reduction drops below
        # 5% of the largest marginal reduction
        wcss_arr = np.array(wcss)
        drops = -np.diff(wcss_arr)
        ratio = drops / max(drops.max(), 1e-12)
        elbow_k = ks[-1]
        for i, r in enumerate(ratio):
            if r < 0.05:
                elbow_k = ks[i + 1]
                break

    # Gap (Tibshirani rule)
    gap_k = ks[-1]
    for i in range(len(gaps) - 1):
        if gaps[i] >= gaps[i + 1] - gap_stds[i + 1]:
            gap_k = ks[i]
            break

    # Silhouette global max
    sil_k = ks[int(np.argmax(silhouettes))]

    # Robust consensus: median of the three
    suggestions = sorted([elbow_k, gap_k, sil_k])
    optimal = int(np.median(suggestions))

    info = {
        "elbow_k": elbow_k,
        "gap_k": gap_k,
        "silhouette_k": sil_k,
        "suggestions": suggestions,
        "optimal_k": optimal,
    }
    return optimal, info


# ---------------------------------------------------------------------------
# HDBSCAN sweep
# ---------------------------------------------------------------------------
def run_hdbscan_sweep(X: np.ndarray) -> tuple[np.ndarray, dict]:
    best = None
    results = []
    for mcs in HDBSCAN_MIN_SIZES:
        clusterer = hdbscan.HDBSCAN(
            min_cluster_size=mcs,
            metric="euclidean",  # UMAP output is already cosine-aware
            cluster_selection_method="eom",
        )
        labels = clusterer.fit_predict(X)
        n_clusters = int((labels >= 0).sum() and labels.max() + 1)
        n_noise = int((labels == -1).sum())
        frac_noise = n_noise / len(labels)

        # Silhouette on non-noise points (if we have at least 2 clusters)
        mask = labels != -1
        if n_clusters >= 2 and mask.sum() > n_clusters:
            sil = silhouette_score(X[mask], labels[mask])
        else:
            sil = float("nan")

        # DBCV-ish score: hdbscan exposes relative_validity_
        try:
            rv = clusterer.relative_validity_
        except Exception:
            rv = float("nan")

        results.append({
            "min_cluster_size": mcs,
            "n_clusters": n_clusters,
            "n_noise": n_noise,
            "frac_noise": round(frac_noise, 3),
            "silhouette": sil,
            "relative_validity": rv,
        })
        print(
            f"  HDBSCAN mcs={mcs:3d} -> {n_clusters} clusters, "
            f"noise={n_noise} ({frac_noise:.0%}), "
            f"sil={sil:.3f}, DBCV={rv:.3f}"
        )

        # Pick the config that maximizes relative_validity, tie-break on
        # higher silhouette, penalizing >50% noise.
        score = (rv if not np.isnan(rv) else -1.0) + (
            (sil if not np.isnan(sil) else 0.0) * 0.1
        )
        if frac_noise > 0.5:
            score -= 1.0

        if best is None or score > best["score"]:
            best = {
                "min_cluster_size": mcs,
                "labels": labels,
                "n_clusters": n_clusters,
                "n_noise": n_noise,
                "score": score,
            }

    return best["labels"], {"sweep": results, "best": best}


# ---------------------------------------------------------------------------
# Figures
# ---------------------------------------------------------------------------
def plot_selection_metrics(
    wcss, silhouettes, gaps, gap_stds, info
) -> None:
    ks = K_RANGE
    fig, axes = plt.subplots(1, 3, figsize=(16, 4.5))

    ax = axes[0]
    ax.plot(ks, wcss, "o-", markersize=3)
    ax.axvline(info["elbow_k"], color="red", ls="--",
               label=f"elbow: k={info['elbow_k']}")
    ax.axvline(info["optimal_k"], color="black", ls=":",
               label=f"chosen: k={info['optimal_k']}")
    ax.set(xlabel="k", ylabel="WCSS", title="Elbow Method")
    ax.legend()

    ax = axes[1]
    ax.errorbar(ks, gaps, yerr=gap_stds, fmt="o-", markersize=3, capsize=2)
    ax.axvline(info["gap_k"], color="red", ls="--",
               label=f"gap: k={info['gap_k']}")
    ax.axvline(info["optimal_k"], color="black", ls=":",
               label=f"chosen: k={info['optimal_k']}")
    ax.set(xlabel="k", ylabel="Gap", title="Gap Statistic")
    ax.legend()

    ax = axes[2]
    ax.plot(ks, silhouettes, "o-", markersize=3)
    ax.axvline(info["silhouette_k"], color="red", ls="--",
               label=f"sil: k={info['silhouette_k']}")
    ax.axvline(info["optimal_k"], color="black", ls=":",
               label=f"chosen: k={info['optimal_k']}")
    ax.set(xlabel="k", ylabel="Silhouette", title="Silhouette Score")
    ax.legend()

    fig.suptitle(
        f"Cluster Selection (elbow={info['elbow_k']}, "
        f"gap={info['gap_k']}, sil={info['silhouette_k']}, "
        f"chosen={info['optimal_k']})",
        fontsize=12, fontweight="bold"
    )
    fig.tight_layout()
    fig.savefig(FIG_SELECTION, dpi=200)
    plt.close(fig)
    print(f"Saved -> {FIG_SELECTION}")


def plot_comparison(X2d, km_labels, hd_labels) -> None:
    fig, axes = plt.subplots(1, 2, figsize=(14, 6))

    ax = axes[0]
    n_km = len(set(km_labels))
    sc = ax.scatter(X2d[:, 0], X2d[:, 1], c=km_labels, cmap="tab20", s=12)
    ax.set_title(f"K-means (k = {n_km})")
    ax.set_xlabel("UMAP-1"); ax.set_ylabel("UMAP-2")

    ax = axes[1]
    is_noise = hd_labels == -1
    ax.scatter(X2d[is_noise, 0], X2d[is_noise, 1],
               c="lightgrey", s=10, label="noise", alpha=0.6)
    ax.scatter(X2d[~is_noise, 0], X2d[~is_noise, 1],
               c=hd_labels[~is_noise], cmap="tab20", s=12)
    n_hd = int(hd_labels.max() + 1) if (hd_labels >= 0).any() else 0
    ax.set_title(f"HDBSCAN ({n_hd} clusters, "
                 f"{is_noise.sum()} noise)")
    ax.set_xlabel("UMAP-1"); ax.set_ylabel("UMAP-2")
    ax.legend(loc="best", fontsize=8)

    fig.suptitle("Clustering comparison on UMAP 2-D projection",
                 fontsize=12, fontweight="bold")
    fig.tight_layout()
    fig.savefig(FIG_COMPARISON, dpi=200)
    plt.close(fig)
    print(f"Saved -> {FIG_COMPARISON}")


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
      
        # ---- Embed (reuse if already saved) ----
    # if EMB_OUT.exists() and PMID_OUT.exists() and \
    #         PMID_OUT.read_text().splitlines() == pmids:
    #     print(f"Loading cached embeddings from {EMB_OUT}")
    #     embeddings = np.load(EMB_OUT)
    # else:  
      
    embeddings = embed_texts(texts)
    np.save(EMB_OUT, embeddings)
    PMID_OUT.write_text("\n".join(pmids))

        # Also save as CSV for R / other tools:
        # first column = PUBMED_ID, then V1..Vn embedding columns.
    emb_df = pd.DataFrame(
            embeddings,
            columns=[f"V{i + 1}" for i in range(embeddings.shape[1])],
        )
    emb_df.insert(0, "PUBMED_ID", pmids)
    emb_df.to_csv(EMB_CSV, index=False)

    print(f"Saved embeddings -> {EMB_OUT}")
    print(f"Saved embeddings (CSV) -> {EMB_CSV}")

    # ---- L2-normalize so Euclidean ≈ cosine ----
    embeddings_n = normalize(embeddings, norm="l2", axis=1)

    # ---- UMAP reduction (for clustering) ----
    # print(f"\nUMAP -> {UMAP_N_COMPONENTS} dims for clustering ...")
    # reducer = umap.UMAP(
    #     n_components=UMAP_N_COMPONENTS,
    #     n_neighbors=UMAP_N_NEIGHBORS,
    #     min_dist=UMAP_MIN_DIST,
    #     metric=UMAP_METRIC,
    #     random_state=UMAP_RANDOM_STATE,
    # )
    # X = reducer.fit_transform(embeddings_n)

    # 2-D projection purely for plotting
    # print("UMAP -> 2 dims for visualization ...")
    # vis_reducer = umap.UMAP(
    #     n_components=2,
    #     n_neighbors=UMAP_N_NEIGHBORS,
    #     min_dist=0.1,
    #     metric=UMAP_METRIC,
    #     random_state=UMAP_RANDOM_STATE,
    # )
    # X2d = vis_reducer.fit_transform(embeddings_n)

    # ---- Cluster selection code (commented out) ----
    # print(f"\nEvaluating K-means k = {K_MIN}..{K_MAX} (on UMAP-{UMAP_N_COMPONENTS}) ...")
    # print("  WCSS ..."); wcss = compute_wcss(X)
    # print("  Silhouette ..."); sils = compute_silhouette(X)
    # print("  Gap statistic ..."); gaps, gap_stds = compute_gap_statistic(X)
    #
    # pd.DataFrame({
    #     "k": K_RANGE, "wcss": wcss, "silhouette": sils,
    #     "gap": gaps, "gap_std": gap_stds,
    # }).to_csv(METRICS_OUT, index=False)
    # print(f"Saved metrics -> {METRICS_OUT}")
    #
    # optimal_k, info = find_optimal_k(wcss, sils, gaps, gap_stds)
    # print(f"\nK-means suggestions: {info['suggestions']}  "
    #       f"(elbow={info['elbow_k']}, gap={info['gap_k']}, "
    #       f"sil={info['silhouette_k']})")
    # print(f"Chosen K-means k = {optimal_k}")
    # plot_selection_metrics(wcss, sils, gaps, gap_stds, info)
    #
    # # ---- HDBSCAN sweep ----
    # print(f"\nHDBSCAN sweep over min_cluster_size = {HDBSCAN_MIN_SIZES} ...")
    # hd_labels, hd_info = run_hdbscan_sweep(X)
    # best = hd_info["best"]
    # print(f"Best HDBSCAN: min_cluster_size={best['min_cluster_size']}  "
    #       f"{best['n_clusters']} clusters, {best['n_noise']} noise")
    # pd.DataFrame({"PUBMED_ID": pmids, "cluster": hd_labels}).to_csv(
    #     HDBSCAN_OUT, index=False
    # )
    # print(f"Saved -> {HDBSCAN_OUT}")
    # plot_comparison(X2d, km_labels, hd_labels)

    # ---- K-means with fixed k = 17 ----
    N_CLUSTERS = 4#17
    print(f"\nRunning K-means with k = {N_CLUSTERS} ...")
    km = KMeans(n_clusters=N_CLUSTERS, random_state=42, n_init=10)
    km_labels = km.fit_predict(embeddings_n)

    # Per-point silhouette scores + overall average
    sil_values = silhouette_samples(embeddings_n, km_labels)
    avg_sil = silhouette_score(embeddings_n, km_labels)
    print(f"Average silhouette score: {avg_sil:.4f}")

    pd.DataFrame({
        "PUBMED_ID": pmids,
        "cluster": km_labels,
        "silhouette": sil_values,
    }).to_csv(KMEANS_OUT, index=False)
    print(f"Saved -> {KMEANS_OUT}")

    # ---- Summary ----
    print("\n=== Summary ===")
    print(f"N abstracts:              {len(pmids)}")
    print(f"K-means k:                {N_CLUSTERS}")
    print(f"Average silhouette score: {avg_sil:.4f}")
    print("K-means cluster sizes:")
    print(pd.Series(km_labels).value_counts().sort_index().to_string())
    print("\nMean silhouette by cluster:")
    print(pd.DataFrame({"cluster": km_labels, "silhouette": sil_values})
          .groupby("cluster")["silhouette"].mean().round(4).to_string())


if __name__ == "__main__":
    main()
