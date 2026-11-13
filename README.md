# genomics_ancest_disease_dispar

Code for a meta-science study of the GWAS Catalog. 

[Workflowr page](https://ijbeasley.github.io/genomics_ancest_disease_dispar/index.html)

# Software requirements 

- R version 4.3.2 (packages tracked by renv, see [renv.lock](https://github.com/IJbeasley/genomics_ancest_disease_dispar/blob/main/renv.lock))
- Python version (3.12.12, venv environment packages see [spacy_venv_requirements.txt](https://github.com/IJbeasley/genomics_ancest_disease_dispar/blob/main/spacy_venv_requirements.txt))

Also see [spacy_venv.def](https://github.com/IJbeasley/genomics_ancest_disease_dispar/blob/main/spacy_venv.def) for how the apptainer container for spacy_venv was built.

For annotating text: 
- Doccano v1.8.5 (install via docker image, see https://doccano.github.io/doccano/)

For processing publisher specific xml formats: 
- Auto-CORPus v1.1.1 ([spacy_venv_requirements.txt](https://github.com/IJbeasley/genomics_ancest_disease_dispar/blob/main/spacy_venv_requirements.txt), https://github.com/omicsNLP/Auto-CORPus) 

For detecting abbreviations in text: 
- Ab3p (https://github.com/ncbi-nlp/Ab3P)

See [ab3p.def](https://github.com/IJbeasley/genomics_ancest_disease_dispar/blob/main/ab3p.def) for how apptainer container forAb3p was built. 

