from sentence_transformers import SentenceTransformer
import json
import numpy as np
import matplotlib.pyplot as plt
import time

model = SentenceTransformer("neuml/pubmedbert-base-embeddings")

cohort_sentences = json.load(open("output/doccano/methods_cohort_sentences.json", "r"))
non_cohort_sentences = json.load(open("output/doccano/methods_non_cohort_sentences.json", "r"))

#cohort_sentences = json.load(open("output/doccano/abstract_cohort_sentences.json", "r"))
#non_cohort_sentences = json.load(open("output/doccano/abstract_non_cohort_sentences.json", "r"))

cohort_embeddings = model.encode(cohort_sentences)

from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from sentence_transformers import SentenceTransformer

# Choose k - try a range and use silhouette score to guide choice
#silhouette_scores = []
#k_range = range(2, 20)

#for k in k_range:
#    kmeans = KMeans(n_clusters=k, random_state=42, n_init=10)
#    labels = kmeans.fit_predict(cohort_embeddings)
#    score = silhouette_score(cohort_embeddings, labels, metric="cosine")
#    silhouette_scores.append(score)
#    print(f"k={k}: silhouette={score:.3f}")

#best_k = k_range[np.argmax(silhouette_scores)]
#print(f"\nBest k: {best_k}")

# Compute cosine similarities
non_cohort_embeddings = model.encode(non_cohort_sentences)
similarities = model.similarity(non_cohort_embeddings, cohort_embeddings)
similarities = similarities.cpu().numpy() if hasattr(similarities, 'cpu') else np.array(similarities)

# For each non-cohort sentence, find the most similar cohort sentence and its similarity score
best_cohort_idx = np.argmax(similarities, axis=1)
best_similarity = np.max(similarities, axis=1)

# For each cohort sentence, find the most similar cohort sentence (excluding itself) and its similarity score
cohort_similarities = model.similarity(cohort_embeddings, cohort_embeddings)
cohort_similarities = cohort_similarities.cpu().numpy() if hasattr(cohort_similarities, 'cpu') else np.array(cohort_similarities)
np.fill_diagonal(cohort_similarities, -np.inf)  # Exclude self-similarities
cohort_best_similarity = np.max(cohort_similarities, axis=1)

# Print median similarities
print(f"Median similarity of non-cohort sentences to their best cohort match: {np.median(best_similarity):.3f}")
print(f"Median similarity of cohort sentences to their best cohort match: {np.median(cohort_best_similarity):.3f}")

# Plot histogram comparing best similarities
plt.figure(figsize=(10, 6))
plt.hist(cohort_best_similarity, bins=50, alpha=0.6, label='Cohort sentences', color='steelblue', edgecolor='black')
plt.hist(best_similarity, bins=50, alpha=0.6, label='Non-cohort sentences', color='salmon', edgecolor='black')
plt.axvline(np.median(cohort_best_similarity), color='steelblue', linestyle='--', linewidth=2, label=f'Cohort median: {np.median(cohort_best_similarity):.3f}')
plt.axvline(np.median(best_similarity), color='salmon', linestyle='--', linewidth=2, label=f'Non-cohort median: {np.median(best_similarity):.3f}')
plt.xlabel('Best Similarity to Cohort Sentences')
plt.ylabel('Frequency')
plt.title('Distribution of Best Similarity Scores to Cohort Sentences')
plt.legend()
plt.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('output/similarities_histogram.png', dpi=300)
print("\nHistogram saved to output/similarities_histogram.png")

# keep non-cohort sentences that fall above the bottom 25% of cohort similarities
# how many sentences does this select, and what is the ratio of selected hard negatives to cohort sentences?

threshold = np.percentile(cohort_best_similarity, 25)
hard_negatives_mask = best_similarity >= threshold
print(f"Threshold: {threshold:.3f}")
print(f"Hard negatives selected: {hard_negatives_mask.sum()} / {len(similarities)}")
print(f"Ratio of hard negatives to cohort sentences: {hard_negatives_mask.sum()} / {len(cohort_sentences)}")

# repeat, but for 50% threshold
threshold = np.percentile(cohort_best_similarity, 50)
hard_negatives_mask = best_similarity >= threshold
print(f"\nThreshold: {threshold:.3f}")
print(f"Hard negatives selected: {hard_negatives_mask.sum()} / {len(similarities)}")
print(f"Ratio of hard negatives to cohort sentences: {hard_negatives_mask.sum()} / {len(cohort_sentences)}")


# Perform fast clustering
# Two parameters to tune:
# min_cluster_size: Only consider cluster that have at least 25 elements
# threshold: Consider sentence pairs with a cosine-similarity larger than threshold as similar
from sentence_transformers import util
import pandas as pd

results = []

for threshold in [0.3, 0.4, 0.5, 0.6, 0.7]:
    for min_size in [10, 25, 50, 100]:
        clusters = util.community_detection(
            cohort_embeddings,
            min_community_size=min_size,
            threshold=threshold
        )
        
        clustered_sentences = sum(len(c) for c in clusters)
        cluster_sizes = [len(c) for c in clusters] if clusters else []
        
        results.append({
            "threshold": threshold,
            "min_size": min_size,
            "n_clusters": len(clusters),
            "sentences_clustered": clustered_sentences,
            "pct_clustered": round(100 * clustered_sentences / len(cohort_sentences), 1),
            "avg_cluster_size": round(clustered_sentences / len(clusters), 1) if clusters else 0,
            "median_cluster_size": np.median(cluster_sizes) if cluster_sizes else 0,
            "min_cluster_size": min(cluster_sizes) if cluster_sizes else 0
        })

print("\nCluster tuning results:")
pd.DataFrame(results).to_csv("output/cluster_tuning.csv", index=False)
print(pd.DataFrame(results).to_string())

# Plot pct_clustered vs avg_cluster_size, faceted by threshold
df = pd.DataFrame(results)
thresholds = sorted(df['threshold'].unique())

# Get consistent y-axis limits
y_min = df['pct_clustered'].min() - 5
y_max = df['pct_clustered'].max() + 5

fig, axes = plt.subplots(1, len(thresholds), figsize=(15, 4))
if len(thresholds) == 1:
    axes = [axes]

for ax, thresh in zip(axes, thresholds):
    thresh_data = df[df['threshold'] == thresh]
    ax.scatter(thresh_data['avg_cluster_size'], thresh_data['pct_clustered'], s=100, alpha=0.6)
    ax.set_xlabel('Average Cluster Size')
    ax.set_ylabel('Percentage Clustered (%)')
    ax.set_title(f'Threshold = {thresh}')
    ax.set_ylim(y_min, y_max)
    ax.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('output/cluster_tuning_plot.png', dpi=300)
print("\nPlot saved to output/cluster_tuning_plot.png")

# Plot pct_clustered vs avg_cluster_size, faceted by min_size
min_sizes = sorted(df['min_size'].unique())

fig, axes = plt.subplots(1, len(min_sizes), figsize=(15, 4))
if len(min_sizes) == 1:
    axes = [axes]

for ax, min_sz in zip(axes, min_sizes):
    min_size_data = df[df['min_size'] == min_sz]
    ax.scatter(min_size_data['avg_cluster_size'], min_size_data['pct_clustered'], s=100, alpha=0.6)
    ax.set_xlabel('Average Cluster Size')
    ax.set_ylabel('Percentage Clustered (%)')
    ax.set_title(f'Min Size = {min_sz}')
    ax.set_ylim(y_min, y_max)
    ax.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('output/cluster_tuning_plot_by_min_size.png', dpi=300)
print("Plot saved to output/cluster_tuning_plot_by_min_size.png")


def inspect_clusters(clusters, sentences, n_examples=3):
    for i, cluster in enumerate(clusters):
        print(f"\n--- Cluster {i+1} | n={len(cluster)} ---")
        # sample from across the cluster, not just top/bottom
        sample_ids = cluster[:n_examples] + cluster[-n_examples:]
        for sid in sample_ids:
            print(f"  {sentences[sid]}")

# try a candidate combination
clusters = util.community_detection(cohort_embeddings, min_community_size=25, threshold=0.5)
inspect_clusters(clusters, cohort_sentences)

print("\n \n \n Inspecting clusters for threshold = 0.6, and min_size = 25 \n")
for threshold, min_size in [(0.6, 25)]:
    clusters = util.community_detection(cohort_embeddings, 
                                        min_community_size=min_size, 
                                        threshold=threshold)
    print(f"\n{'='*60}")
    print(f"threshold={threshold}, min_size={min_size}")
    inspect_clusters(clusters, cohort_sentences)


print("\n Inspecting clusters for threshold = 0.5, and min_size = 25 \n")
for threshold, min_size in [(0.5, 25)]:
    clusters = util.community_detection(cohort_embeddings, 
                                        min_community_size=min_size, 
                                        threshold=threshold)
    print(f"\n{'='*60}")
    print(f"threshold={threshold}, min_size={min_size}")
    inspect_clusters(clusters, cohort_sentences)
    
    # Calculate centroid for each cluster
    print(f"\nCluster centroids:")
    cluster_centroids = []
    for i, cluster in enumerate(clusters):
        cluster_centroid = np.mean(cohort_embeddings[cluster], axis=0)
        cluster_centroids.append(cluster_centroid)
        print(f"Cluster {i+1} centroid shape: {cluster_centroid.shape}")


# Find sentences in the non-cohort set that are similar to the cohort cluster(s)
non_cohort_embeddings = model.encode(non_cohort_sentences)

# Convert cluster_centroids list to numpy array (n_clusters x embedding_dim)
cluster_centroids_array = np.array(cluster_centroids)

# Compute similarity: non_cohort_embeddings (n x d) vs cluster_centroids_array (n_clusters x d)
# Result shape: (n_non_cohort, n_clusters)
similarities = model.similarity(non_cohort_embeddings, cluster_centroids_array)
similarities = np.array(similarities)  # Convert to numpy

# For each non-cohort sentence, find the most similar cluster and its similarity score
best_cluster_idx = np.argmax(similarities, axis=1)
best_similarity = np.max(similarities, axis=1)

# Print top non-cohort sentences by similarity to their best matching cluster
print(f"\nTop non-cohort sentences by similarity to clusters:")
ranked_indices = np.argsort(best_similarity)[::-1]
for rank, idx in enumerate(ranked_indices[:15]):
    print(f"{rank+1}. Cluster {best_cluster_idx[idx]+1}, Similarity: {best_similarity[idx]:.3f}")
    print(f"   {non_cohort_sentences[idx]}\n")

# Compute best similarity for cohort sentences to cluster centroids
cohort_similarities = model.similarity(cohort_embeddings, cluster_centroids_array)
cohort_similarities = np.array(cohort_similarities)  # Convert to numpy
cohort_best_similarity = np.max(cohort_similarities, axis=1)

# Plot histogram comparing best similarities
plt.figure(figsize=(10, 6))
plt.hist(cohort_best_similarity, bins=50, alpha=0.6, label='Cohort sentences', color='steelblue', edgecolor='black')
plt.hist(best_similarity, bins=50, alpha=0.6, label='Non-cohort sentences', color='salmon', edgecolor='black')
plt.axvline(np.median(cohort_best_similarity), color='steelblue', linestyle='--', linewidth=2, label=f'Cohort median: {np.median(cohort_best_similarity):.3f}')
plt.axvline(np.median(best_similarity), color='salmon', linestyle='--', linewidth=2, label=f'Non-cohort median: {np.median(best_similarity):.3f}')
plt.xlabel('Best Similarity to Cluster Centroid')
plt.ylabel('Frequency')
plt.title('Distribution of Best Similarity Scores to Cluster Centroids')
plt.legend()
plt.grid(axis='y', alpha=0.3)
plt.tight_layout()
plt.savefig('output/best_similarity_histogram.png', dpi=300)
print("\nHistogram saved to output/best_similarity_histogram.png")

#centroid = np.mean(cohort_embeddings, axis=0)

# Similarity of every non-cohort sentence to the cohort centroid
#similarities = model.similarity(non_cohort_embeddings, centroid.reshape(1, -1))
#similarities = similarities.flatten()  # flatten to 1D array
#similarities = np.array(similarities)  # convert to numpy array

# Get indices of most similar sentences, sorted descending
#ranked_indices = np.argsort(similarities)[::-1]

# Inspect top results
#for idx in ranked_indices[:10]:
#    print(f"{similarities[idx]:.3f}  {non_cohort_sentences[idx]}")

# Plot and save histogram
#plt.figure(figsize=(10, 6))
#plt.hist(similarities, bins=50, edgecolor='black', alpha=0.7)
#plt.xlabel('Similarity to Cohort Centroid')
#plt.ylabel('Frequency')
#plt.title('Distribution of Sentence Similarities to Cohort Centroid')
#plt.grid(axis='y', alpha=0.3)
#plt.tight_layout()
#plt.savefig('output/similarities_histogram.png', dpi=300)
print("\nHistogram saved to output/methods_similarities_histogram.png")


#cohort_similarities = model.similarity(cohort_embeddings, centroid.reshape(1, -1)).flatten()

#plt.figure(figsize=(10, 6))
#plt.hist(cohort_similarities, bins=50, alpha=0.6, label='Cohort sentences', color='steelblue')
#plt.hist(similarities, bins=50, alpha=0.6, label='Non-cohort sentences', color='salmon')
#plt.axvline(np.median(cohort_similarities), color='steelblue', linestyle='--', label='Cohort median')
#plt.xlabel('Similarity to Cohort Centroid')
#plt.ylabel('Frequency')
#plt.legend()
#plt.tight_layout()
#plt.savefig('output/similarities_histogram_overlay.png', dpi=300)

# e.g. keep non-cohort sentences that fall within the bottom 25% of cohort similarities
# (similar enough to be relevant, but clearly below the cohort cluster)
#threshold = np.percentile(cohort_similarities, 50)
#hard_negatives_mask = similarities >= threshold
#print(f"Threshold: {threshold:.3f}")
#print(f"Hard negatives selected: {hard_negatives_mask.sum()} / {len(similarities)}")
#print(f"Ratio of hard negatives to cohort sentences: {hard_negatives_mask.sum()} / {len(cohort_sentences)}")