# collapse cohorts:


gwas_study_info = data.table::fread("data/gwas_catalog/gwas-catalog-v1.0.3.1-studies-r2025-06-10.tsv",
                                    sep = "\t",
                                    quote = "")

gwas_study_info = gwas_study_info |>
  dplyr::rename_all(~gsub(" ", "_", .x))

gwas_study_info$COHORT |> unique() |> length() #1,208 cohorts

gwas_study_info |>
  dplyr::group_by(COHORT) |>
  dplyr::summarise(n_studies = dplyr::n()) |>
  dplyr::arrange(desc(n_studies))


###################### UK Biobank ######################


# UKBB and UKB are strait-forwardly UK Biobank
# UKBB White British
gwas_study_info |>
  dplyr::filter(COHORT == "UKBB" |
                COHORT == "UKB" |
                COHORT == "UKBB White British") |>
  nrow()


UKBB_PUBMED_ID = gwas_study_info |>
  dplyr::filter(COHORT == "UKBB" |
                  COHORT == "UKB" |
                  COHORT == "UKBB White British") |>
  dplyr::pull(PUBMED_ID) |>
  unique()



# RefManageR::WriteBib(UKBB_Paper_Info, "UKBB_Paper_Info.bib")
#
# UKBB_Paper_Info_v2 = litsearchr::import_results(file = "UKBB_Paper_Info.bib")
#
# citationchaser::get_refs(UKBB_PUBMED_ID[1],
#                          type = "pmid",
#                          #UKBB_Paper_Info_v2$doi,
#                          token = "WCFlpCtuJXYI1sDhZcZ8y7hHpri0SEmTnLNkeU4OEM5JTQRNXB9w")
# ? What about UKB-PPP


#################### Get info from PubMED #################

pmids = UKBB_PUBMED_ID

# Split into chunks of 50
pmid_chunks <- split(pmids, ceiling(seq_along(pmids)/50))

# Initialize storage
all_pubmed_records <- list()
all_dois <- list()

# API key for NCBI
api_key <- "d3ebf9f66f76f211053d838d473fca765a08"

for (i in seq_along(pmid_chunks)) {
  message("Processing batch ", i)

  chunk <- pmid_chunks[[i]]
  query <- paste0("(", paste(chunk, collapse = " OR "), ")[uid]")

  # Check number of records to retrieve
  total <- pubmedR::pmQueryTotalCount(query = query, api_key = api_key)

  if(length(chunk) == total$total_count){

    ukbb_pubmedr <- pubmedR::pmApiRequest(query = query,
                                  limit = total$total_count,
                                  api_key = api_key)

    ukbb_pubmedr_mat = bibliometrix::convert2df(ukbb_pubmedr,
                                            dbsource = "pubmed",
                                            format = "api")

    # Store
    all_pubmed_records[[i]] <- ukbb_pubmedr_mat

    # Get DOIs for OpenAlex
    dois <- RefManageR::GetPubMedByID(id = chunk)$doi
    all_dois[[i]] <- unlist(dois)

} else {

  message("something has gone wrong - number of inputted pmids != returned records")
  warning("Batch ", i, ": PMIDs != returned records. Skipping.")
}

}

# Combine all batches into one data frame
ukbb_pubmed_df <- do.call(rbind, all_pubmed_records)

bibliometrix::missingData(ukbb_pubmed_df)


################ Get info from openAlex #########

# get doi information:
ukbb_dois_all <- as.vector(unlist(all_dois))

library(openalexR)

# Your list of DOIs (after flattening and removing NAs)
ukbb_dois_all <- as.vector(unlist(all_dois))
ukbb_dois_all <- ukbb_dois_all[!is.na(ukbb_dois_all) & ukbb_dois_all != ""]

# Split into chunks of 50
doi_chunks <- split(ukbb_dois_all, ceiling(seq_along(ukbb_dois_all)/50))

# Initialize storage
openalex_results <- list()
openalex_biblio <- list()

# Loop through each chunk
for (i in seq_along(doi_chunks)) {
  message("Processing DOI batch ", i)

  chunk <- doi_chunks[[i]]

  # Create OpenAlex query
  query <- openalexR::oa_query(
    doi = chunk,
    entity = "works"
  )

  # Request data
  res <- openalexR::oa_request(
    query_url = query,
    count_only = FALSE,
    verbose = FALSE
  )

  # Convert results
  works_df <- openalexR::oa2df(res, entity = "works")
  works_biblio <- openalexR::oa2bibliometrix(works_df)

  # Store
  openalex_results[[i]] <- works_df
  openalex_biblio[[i]] <- works_biblio
}

# Combine all results
ukbb_openalex_df <- do.call(rbind, openalex_results)
ukbb_openalex_mat <- do.call(bibliometrix::mergeDbSources, openalex_biblio)

#
# #ukbb_dois = RefManageR::GetPubMedByID(id = pmids)$doi
# query = openalexR::oa_query(doi = ukbb_dois_all[1:50],
#
# #  identifier = "doi:10.1161/CIRCGEN.119.002804", #paste0("doi:",unlist(ukbb_dois)),
#                     entity = "works")
#
# res <- openalexR::oa_request(
#   query_url = query,
#   count_only = FALSE,
#   verbose = FALSE
# )
#
# ukbb_openalex <- openalexR::oa2df(res, entity = "works")

#ukbb_openalex_mat = openalexR::oa2bibliometrix(ukbb_openalex)

bibliometrix::missingData(ukbb_openalex_mat)

# merging not working ...
# trying to merge because pubmedr doesn't collect cited references, but open alex doesn't for keywords
#ukbb_bib_mat = bibliometrix::mergeDbSources(ukbb_pubmedr_mat,  ukbb_openalex_mat, remove.duplicated = TRUE)

#bibliometrix::missingData(ukbb_bib_mat)

# Computes occurrences between cited ref across papers
CR = bibliometrix::cocMatrix(ukbb_openalex_mat, Field = "CR")

#  most commonly cited publications in these articles
most_cited = sort(Matrix::colSums(CR), decreasing = TRUE)
most_cited[1:10]


library(ggplot2)

data.frame(cr = most_cited, paper = names(most_cited)) |>
  dplyr::slice_head(n = 20) |>
  ggplot(aes(y = cr, x = paper)) +
  geom_col() +
  theme_bw()

data.frame(cr = most_cited, paper = names(most_cited)) |>
  dplyr::slice_head(n = 50) |>
  dplyr::mutate(paper = reorder(paper, cr)) |>   # reorder by citation count
  ggplot(aes(y = cr, x = paper)) +
  geom_col() +
  coord_flip() +                          # optional: flip for readability
  theme_bw() +
  labs(x = "Paper", y = "Citations")

data.frame(cr = most_cited, paper = names(most_cited)) |>
  dplyr::slice_head(n = 10) |>
  dplyr::mutate(paper = reorder(paper, cr)) |>   # reorder by citation count
  ggplot(aes(y = cr, x = paper)) +
  geom_col() +
  coord_flip() +                          # optional: flip for readability
  theme_bw() +
  labs(x = "Paper", y = "Citations")

ukbb_papers =
openalexR::oa_fetch(entity = "works", identifier = names(most_cited[1:50]))

ukbb_papers |>
  dplyr::filter(grepl("UK B", title)) |>
  dplyr::select(publication_year, title, doi) |>
  dplyr::arrange(desc(publication_year))

bibliometrix::cocMatrix(ukbb_openalex_mat, Field = "AU")

# Local citations measure how many times an author (or a document)
# included in this collection have been cited by other authors also in the collection.
CR <- bibliometrix::localCitations(ukbb_openalex_mat)
CR$Papers |> dplyr::arrange(desc(LCS)) |> head()

NetMatrix <- bibliometrix::biblioNetwork(ukbb_openalex_mat,
                                         analysis = "coupling",
                                         network = "references",
                                         short = T)

# co-citation of two articles when both are cited in a third article.
NetMatrix <- bibliometrix::biblioNetwork(ukbb_openalex_mat,
                                         analysis = "co-citation",
                                         network = "references",
                                         short = T)

plot = bibliometrix::networkPlot(NetMatrix,
            n = 20, #dim(NetMatrix)[1],
            #normalize = "salton",
            weighted=NULL,
            type= "mds", # "fruchterman", #"kamada", #"mds", #"circle", #"auto",  "fruchterman",
            Title = "Ref coupling",
            size=T,
            halo = T,
            remove.multiple=TRUE,
            labelsize=0.7,
            cluster="none") # "optimal")

graph <- bibliometrix::splitCommunities(plot$graph, n = 2)

plot(graph)

NetMatrix
#net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)



################### Finding UK BB Cohort name
gwas_study_info |>
  dplyr::filter(grepl("UKB", COHORT)) |>
  dplyr::group_by(COHORT) |>
  dplyr::summarise(n_studies = dplyr::n())

gwas_study_info |>
  dplyr::filter(grepl("UK", COHORT)) |>
  dplyr::group_by(COHORT) |>
  dplyr::summarise(n_studies = dplyr::n())





################### Knight_ADRC

gwas_study_info |>
  dplyr::filter(grepl("Knight_ADRC", COHORT)) |>
  dplyr::group_by(PUBMED_ID, COHORT) |>
  dplyr::summarise(n_studies = dplyr::n()) |>
  dplyr::group_by(PUBMED_ID) |>
  dplyr::arrange(desc(n_studies))


# https://www.ebi.ac.uk/gwas/publications/39528825
# 2024 Western et al.

# study analyzed 4,989 CSF proteomic samples from 4,968 unique individuals in eight cohorts (ADNI, DIAN, Knight-ADRC (October 2021) and Knight-ADRC (June 2023), FACE, Barcelona-1, (PPMI), Stanford Iqbal Farrukh and Asad Jamal ADRC and Aging Memory Study (Stanford) and MARS).
# To account for sample size differences across the cohorts, only aptamers present in the largest dataset (that consisted of ADNI, DIAN, Knight-ADRC (October 2021), FACE and Barcelona-1; Naptamers = 7,008) were kept for analysis.
#  Sample sizes ranged from 2,326 for aptamers only assayed in ADNI, Barcelona-1, DIAN, FACE and Knight-ADRC (October 2021) to 3,506 for aptamers assayed in all cohorts.

# ok so one cohort per disease / trait ... some subset of 4,968 individuals
gwas_study_info |>
  dplyr::filter(PUBMED_ID == "39528825") |>
  View()

gwas_study_info |>
  dplyr::filter(PUBMED_ID == "39528825") |>
  nrow()

gwas_study_info |>
  dplyr::filter(PUBMED_ID == "39528825") |>
  dplyr::group_by(`DISEASE/TRAIT`) |>
  dplyr::summarise(n = dplyr::n()) |>
  View()

gwas_study_info |>
  dplyr::filter(PUBMED_ID == "39528825") |>
  dplyr::group_by(COHORT, INITIAL_SAMPLE_SIZE) |>
  dplyr::summarise(n_studies = dplyr::n()) |>
  dplyr::arrange(COHORT)


# The CSF samples were collected from participants of five cohorts, including MAP from Knight ADRC, Dominantly Inherited Alzheimer Network (DIAN), Barcelona-1 (longitudinal observational study from the memory and disorder unit at the university hospital Mutua de Terrasa, Terrassa, Barcelona, Spain), Alzheimer’s Disease Neuroimaging Initiative (ADNI) and Fundació ACE (ACE Alzheimer Center Barcelona, Barcelona, Spain;

# Knight_ADRC|ADNI|Barcelona-1|GR@ACE|DIAN|NR|Stanford_ADRC|PPMI


# Knight_ADRC|DIAN|Barcelona-1|ADNI|GR@ACE|WADRC|WRAP


################# "" COHORT not provided ##############

gwas_study_info |>
  dplyr::filter(COHORT == "")
# Check if UK BB
# 1. Year .. TBD

gwas_study_info |>
  dplyr::filter(COHORT == "")  |>
  dplyr::filter(DATE > '2015-01-1') |>
  dplyr::arrange(DATE)

# 2. Genotyping tech

gwas_study_info |>
  dplyr::filter(COHORT == "") |>
  dplyr::filter(grepl("UK",GENOTYPING_TECHNOLOGY))

gwas_study_info |>
  dplyr::filter(COHORT == "")  |>
  dplyr::filter(DATE > '2015-01-1') |>
  dplyr::select(DATE, INITIAL_SAMPLE_SIZE) |>
  dplyr::distinct()

gwas_study_info |>
  dplyr::filter(COHORT == "UKBB" |
                  COHORT == "UKB" |
                  COHORT == "UKBB White British") |>
dplyr::select(GENOTYPING_TECHNOLOGY, "PLATFORM_[SNPS_PASSING_QC]") |>
dplyr::distinct()

gwas_study_info |>
  dplyr::filter(COHORT == "UKBB" |
                  COHORT == "UKB" |
                  COHORT == "UKBB White British") |>
  dplyr::select(GENOTYPING_TECHNOLOGY) |>
  dplyr::distinct()

gwas_study_info |>
  dplyr::filter(COHORT == "UKBB" |
                  COHORT == "UKB" |
                  COHORT == "UKBB White British") |>
  dplyr::arrange(DATE) |>
  dplyr::select(PUBMED_ID, DATE, DATE_ADDED_TO_CATALOG, FIRST_AUTHOR, JOURNAL) |>
  dplyr::distinct()

gwas_study_info |>
  dplyr::filter(grepl("UK", COHORT)) |>
  dplyr::arrange(DATE) |>
  dplyr::select(PUBMED_ID, DATE, DATE_ADDED_TO_CATALOG, FIRST_AUTHOR, JOURNAL) |>
  dplyr::distinct()

gwas_study_info |>
  dplyr::filter(PUBMED_ID %in% c(27073872, 27073872)) |>
  dplyr::select(COHORT) |>
  dplyr::distinct()
