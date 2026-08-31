
library(xml2)
library(rentrez)
library(purrr)

library(xml2)
library(rentrez)
library(purrr)

MeSH_from_pmids <- function(pmids,
                            batch_size = 200,
                            verbose = TRUE,
                            sleep = 0.1,
                            ENTREZ_KEY = Sys.getenv("NCBI_API_KEY")) {

  # one place defining the schema, used by both NA fallbacks
  empty_row <- function(pmid) {
    data.frame(pubmed_id    = pmid,
               mesh_ui      = NA_character_,
               descriptor   = NA_character_,
               qualifier_ui = NA_character_,
               qualifier    = NA_character_,
               major_topic  = NA,
               stringsAsFactors = FALSE)
  }

  pmids   <- unique(as.character(pmids))
  batches <- split(pmids, ceiling(seq_along(pmids) / batch_size))
  out     <- vector("list", length(batches))

  for (i in seq_along(batches)) {

    batch_pmids <- batches[[i]]

    if (verbose) message(sprintf("Fetching batch %d of %d (%d PMIDs)...",
                                 i, length(batches), length(batch_pmids)))

    xml_data <- tryCatch(
      entrez_fetch(db = "pubmed",
                   id = paste(batch_pmids, collapse = ","),
                   rettype = "xml",
                   parsed = FALSE,
                   api_key = ENTREZ_KEY),
      error = function(e) {
        warning(sprintf("Batch %d failed: %s", i, conditionMessage(e)))
        NULL
      }
    )

    if (is.null(xml_data)) next

    doc      <- read_xml(xml_data)
    articles <- xml_find_all(doc, ".//PubmedArticle")

    out[[i]] <- map(articles, function(article) {

      pmid     <- xml_text(xml_find_first(article, ".//MedlineCitation/PMID"))
      headings <- xml_find_all(article, ".//MeshHeadingList/MeshHeading")

      if (length(headings) == 0) return(empty_row(pmid))

      map(headings, function(h) {
        d <- xml_find_first(h, "./DescriptorName")
        q <- xml_find_all(h, "./QualifierName")
        data.frame(
          pubmed_id    = pmid,
          mesh_ui      = xml_attr(d, "UI"),
          descriptor   = xml_text(d),
          qualifier_ui = if (length(q)) xml_attr(q, "UI") else NA_character_,
          qualifier    = if (length(q)) xml_text(q)       else NA_character_,
          major_topic  = xml_attr(d, "MajorTopicYN") == "Y",
          stringsAsFactors = FALSE
        )
      }) |> list_rbind()

    }) |> list_rbind()

    Sys.sleep(sleep)
  }

  res <- list_rbind(out)

  missing <- setdiff(pmids, res$pubmed_id)
  if (length(missing) > 0) {
    if (verbose) message(sprintf("%d PMIDs returned no record.", length(missing)))
    res <- rbind(res, empty_row(missing))
  }

  res
}

gwas_pmids <- gwas_study_info$PUBMED_ID |> unique()

gwas_mesh <- MeSH_from_pmids(gwas_pmids)

# awk -F'|' '$2=="ENG" && $12=="MSH"' data/icd/2025AA/META/MRCONSO.RRF > data/icd/2025AA/META/MRCONSO_MSH.RRF
# awk -F'|' '$2=="ENG" && $12=="ICD10"' data/icd/2025AA/META/MRCONSO.RRF > data/icd/2025AA/META/MRCONSO_ICD10.RRF
# awk -F'|' '$2=="ENG" && $12=="ICD10CM"' data/icd/2025AA/META/MRCONSO.RRF > data/icd/2025AA/META/MRCONSO_ICD10CM.RRF
# awk -F'|' '$2=="ENG" && $12=="ICD9CM"' data/icd/2025AA/META/MRCONSO.RRF > data/icd/2025AA/META/MRCONSO_ICD9CM.RRF

{
cols <- c("CUI","LAT","TS","LUI","STT","SUI","ISPREF","AUI","SAUI","SCUI",
          "SDUI","SAB","TTY","CODE","STR","SRL","SUPPRESS","CVF","EXTRA")

umls_mesh <- data.table::fread(
  here::here("data/icd/2025AA/META/MRCONSO_MSH.RRF"),
  sep = "|", header = FALSE, quote = "", fill = TRUE,
  col.names = cols, colClasses = "character",
  na.strings = c("", "NA")
)

umls_mesh <-
  umls_mesh  |>
  distinct(CUI, mesh = CODE)

umls_icd10 <-  data.table::fread(
  here::here("data/icd/2025AA/META/MRCONSO_ICD10.RRF"),
  sep = "|", header = FALSE, quote = "", fill = TRUE,
  col.names = cols, colClasses = "character",
  na.strings = c("", "NA")
)

umls_icd10  <-
  umls_icd10  |>
  distinct(CUI, icd10_code = CODE)

umls_crosswalk <-
  left_join(umls_mesh,
            umls_icd10,
            by = "CUI",
            relationship ="many-to-many")


umls_icd10cm <-  data.table::fread(
  here::here("data/icd/2025AA/META/MRCONSO_ICD10CM.RRF"),
  sep = "|", header = FALSE, quote = "", fill = TRUE,
  col.names = cols, colClasses = "character",
  na.strings = c("", "NA")
)

umls_icd10cm  <-
  umls_icd10cm  |>
  distinct(CUI, icd10cm_code = CODE)

umls_crosswalk <-
left_join(umls_crosswalk,
          umls_icd10cm,
          by = "CUI",
          relationship = "many-to-many")

umls_icd9cm <-  data.table::fread(
  here::here("data/icd/2025AA/META/MRCONSO_ICD9CM.RRF"),
  sep = "|", header = FALSE, quote = "", fill = TRUE,
  col.names = cols, colClasses = "character",
  na.strings = c("", "NA")
)


umls_icd9cm  <-
  umls_icd9cm  |>
  distinct(CUI, icd9cm_code = CODE)

umls_crosswalk <-
  left_join(umls_crosswalk,
            umls_icd9cm,
            by = "CUI",
            relationship = "many-to-many")

umls_crosswalk <-
  umls_crosswalk |>
 filter(!if_all(contains("icd"), is.na)) |>
  rename(mesh_ui = mesh)
}

# number of unique terms
gwas_mesh |>
  distinct(descriptor, mesh_ui) |>
  nrow()
# nearly 6,000

mesh_maps <-
left_join(gwas_mesh |>
          distinct(descriptor, mesh_ui),
          umls_crosswalk,
          by = "mesh_ui",
          relationship = "many-to-many")


gwas_mesh <-
  left_join(gwas_mesh |>
            distinct(pubmed_id, descriptor, mesh_ui),
            mesh_maps,
            by = c("mesh_ui", "descriptor"),
            relationship = "many-to-many"
  )

# how does having ICD code compare to disease pmids
icd10_pmids <-
gwas_mesh |>
  filter(!if_all(contains("icd"), is.na)) |>
  pull(pubmed_id) |>
  unique()

icd10_pmids |> length()

disease_pmids |> length()

disease_pmids[disease_pmids %in% icd10_pmids] |> length()

disease_pmids[!c(disease_pmids %in% icd10_pmids)]
#  gwas_mesh |> filter(pubmed_id == "35984985") misses disease
# Kidney Neoplasms

icd10_pmids[!c(icd10_pmids %in% disease_pmids)] |> length()


# 37017090
