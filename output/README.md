# Output

Processed data files produced by the analysis pipeline are saved here.

## Folders

| Folder name | Description |
| ----------- | ----------- |
| abstracts | PubMed abstract plain text (`{PMID}.txt`) and their sentence-segmented versions (`{PMID}_sentences.json`) |
| clustering | Abstract embedding and clustering results: MedCPT embeddings (`.csv`/`.npy`), k-means cluster assignments, and cluster-selection metrics and plots |
| doccano | Doccano annotation-project files: cohort / non-cohort sentence sets (`.json`/`.jsonl`) and sentence-similarity plots for manual labeling |
| fulltexts | Retrieved full-text articles organized into per-publisher / per-source subfolders (e.g. `elsevier`, `bmj`, `cambridge`, `ers`) |
| gwas_cat | Processed GWAS Catalog tables: ancestry and cohort term lists, study groupings, PMID→PMCID mapping, and author-provided ICD-10 codes |
| gwas_cohorts | GWAS cohort-identification outputs: dbGaP / EGA cohort names and accessions, corrected cohort names, and sample sizes |
| icd_map | GWAS Catalog metadata mapped to ICD-10 codes and and GBD conditions. |
| methods | Methods sections extracted from full-text articles (`{PMID}_methods.txt`), from multiple extraction sources (BioC, PDF/TEI) |
| methods_sentences | Sentence-segmented Methods sections (`{PMID}_methods_sentences.json`) |
| supplement | Supplementary materials downloaded per article, organized into per-PMCID subfolders |
| text_mining_predictions | Text-mining model outputs: PubMedBERT entity/relation annotations, entity predictions, confusion matrices, and grid-search results |
| trait_ontology | EFO trait/disease ontology term lists: descendants of selected EFO terms and a combined disease-term spreadsheet |
