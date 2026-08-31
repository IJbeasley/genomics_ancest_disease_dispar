# Data

Downloaded external data files here.

## Folders

| Folder name | Description | Source |
| ----------- | ----------- | ------ |
| cdc | CDC mortality data: underlying cause-of-death counts by race for ICD code groups (`underlying_cod_race_*.csv`), plus full 2019 mortality microdata (`full_mort/`) | CDC WONDER (https://wonder.cdc.gov) and NVSS Mortality Public Use Files (https://www.cdc.gov/nchs/nvss/mortality_public_use_data.htm) |
| cohort | Manually curated cohort description spreadsheet (`cohort_desc.xlsx`) used to characterize study cohorts | Internal (project-generated) |
| europe_pmc | Europe PMC article-linkage tables: PMID–PMCID–DOI ID mapping (`PMID_PMCID_DOI.csv`) and dbGaP / EGA accession links to PMC articles (`dbgap.csv`, `ega.csv`) | Europe PMC downloads (https://europepmc.org/downloads) |
| gbd | Global Burden of Disease study epidemiology data (disease burden and population attributable fraction rates; see `gbd/README.md`) | https://vizhub.healthdata.org/gbd-results |
| gwas_catalog | GWAS Catalog study and ancestry metadata (`*-studies-*.tsv`, `*-ancestries-*.tsv`) | https://www.ebi.ac.uk/gwas/docs/file-downloads |
| icd | ICD code datasets and disease-to-ICD-10 mappings: UMLS Metathesaurus (`2025AA/`), Disease Ontology cross-references (`allXREFinDO.tsv`), EMBL-EBI OxO ontology mappings (`oxo/`), phecode maps, GBD cause–ICD maps, UK Biobank codes, and manual mappings (see `icd/README.md`) | Multiple (UMLS, Disease Ontology, EMBL-EBI OxO, IHME, phecode, UK Biobank) |
| nih | NIH RCDC (Research, Condition, and Disease Categorization) funding summary (`RCDCFundingSummary_*.xlsx`) | NIH RePORT (https://report.nih.gov/funding/categorical-spending) |
