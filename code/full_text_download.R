# Check how many abstracts not extracted from retrenz

length(unique(pmids))

list.files(here::here("output/abstracts")) |> length()

gsub(".txt", "", list.files(here::here("output/abstracts"))) -> abstracts_pmids

pmids[!pmids %in% abstracts_pmids] -> missing_abstracts

library(openalexR)

oa_fetch(pmid = missing_abstracts,
               entity = "works",
               abstract = T
               ) -> missing_abstracts_data


# get dois
oa_fetch(pmid = pmids[1:10],
         entity = "works"
         # options = list(fields = c(#"doi",
         #                           "best_oa_location.landing_page_url")
                        # )
) -> test_dois


test_dois |> View()


library(httr)
library(rvest)
library(xml2)

download_fulltext_txt <- function(pubmed_id) {

  doi_url <- "https://www.ncbi.nlm.nih.gov/research/bionlp/RESTful/pmcoa.cgi/BioC_json/pubmed_id/ascii"

  destfile <- paste0(here::here("output/fulltexts/"), pubmed_id, ".txt")
  # Resolve DOI and fetch the page
  # resp <- httr::GET(
  #   doi_url,
  #   httr::config(followlocation = TRUE),
  #   httr::add_headers(`User-Agent` =
  #                       "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
  # )
  resp <- httr::GET(doi_url, httr::config(followlocation = TRUE))

  if (httr::status_code(resp) != 200) {
    stop("Failed to fetch page: status ", httr::status_code(resp))
  }

  ctype <- httr::headers(resp)[["content-type"]]
  message("Content-Type: ", ctype)

  # Case 1: HTML text (e.g. open-access article page)
  if (grepl("html", ctype, ignore.case = TRUE)) {
    html <- httr::content(resp, as = "text", encoding = "UTF-8")
    doc <- xml2::read_html(html)

    # Extract visible text (main content only)
    text <- doc %>% rvest::html_text2()

    # Save as text file
    writeLines(text, destfile, useBytes = TRUE)
    message("Saved plain text to ", destfile)
    return(invisible(destfile))
  }

  # Case 2: PDF file
  if (grepl("pdf", ctype, ignore.case = TRUE)) {
    message("Detected PDF — converting to text using pdftools...")
    tmp_pdf <- tempfile(fileext = ".pdf")
    writeBin(httr::content(resp, as = "raw"), tmp_pdf)
    text <- pdftools::pdf_text(tmp_pdf)
    writeLines(text, destfile)
    message("Converted PDF to text and saved to ", destfile)
    return(invisible(destfile))
  }

  stop("Unknown content type: ", ctype)
}

download_fulltext_txt(23143602)



library(httr)
library(jsonlite)

get_best_oa_location <- function(doi) {

  base_url <- "https://api.openalex.org/works/"
  query_url <- paste0(base_url, "https://doi.org/", doi)

  resp <- httr::GET(query_url)
  if (httr::status_code(resp) != 200) {
    stop("Failed to fetch metadata for DOI ", doi,
         ": HTTP ", httr::status_code(resp))
  }

  data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))

  if (is.null(data$best_oa_location)) {
    message("No open-access location found for DOI ", doi)
    return(NULL)
  }

  oa <- data$best_oa_location
  tibble::tibble(
    doi = doi,
    is_oa = oa$is_oa,
    version = oa$version,
    license = oa$license,
    pdf_url = oa$url_for_pdf,
    landing_page = oa$landing_page_url,
    repository_rankings = oa$repository_rankings,
    pdf_url = oa$pdf_url
  )
}


get_best_oa_location("10.1126/science.1135245")
download_fulltext_txt("https://pmc.ncbi.nlm.nih.gov/articles/PMC4410764/",
                      "1135245")


library(httr)
library(jsonlite)

download_biocpmc <- function(pmcid,
                             destfile = paste0(pmcid, ".txt")) {
  base <- "https://www.ncbi.nlm.nih.gov/research/bionlp/RESTful/pmcoa.cgi/BioC_json/"
  url <- paste0(base, pmcid, "/unicode")

  message("Fetching: ", url)
  resp <- httr::GET(url)
  if (httr::status_code(resp) != 200) {
    stop("Failed: HTTP ", httr::status_code(resp))
  }

  data <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))

  # Extract all text passages
  passages <- unlist(lapply(data$documents[[1]]$passages, function(p) p$text))

  # Collapse to plain text
  text <- paste(passages, collapse = "\n\n")
  writeLines(text, destfile, useBytes = TRUE)
  message("Saved full text to ", destfile)

  invisible(destfile)
}


download_biocpmc("23143602")



# EGAS50000000090



library(httr)
library(jsonlite)
library(xml2)

download_fulltext_pmc_by_pmid <- function(pmid,
                                          out_dir = "fulltexts_pmc",
                                          tool = "myTool",
                                          email = "you@example.com") {
  # 1. Map PMID → PMCID (via converter API)
  conv_url <- "https://pmc.ncbi.nlm.nih.gov/tools/idconv/api/v1/articles/"
  resp <- httr::GET(conv_url,
                    query = list(ids = pmid,
                                 idtype = "pmid",
                                 tool = tool,
                                 email = email,
                                 format = "json"))
  httr::stop_for_status(resp)
  dat <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
  if (nrow(dat$records) == 0 || is.null(dat$records$pmcid)) {
    stop("PMCID not found for PMID ", pmid)
  }
  pmcid <- dat$records$pmcid

  # 2. Build download URL (for example: the OA subset on AWS)
  # Note: you may need to discover the correct path structure; here is a simple heuristic
  base_aws <- "https://pmc-oa-opendata.s3.amazonaws.com/"  # example public bucket
  xml_filename <- paste0(pmcid, ".xml")
  url_xml <- paste0(base_aws, xml_filename)

  # Create output directory
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_file <- file.path(out_dir, xml_filename)

  # 3. Download
  resp2 <- httr::GET(url_xml)
  if (httr::status_code(resp2) != 200) {
    stop("Failed to download full text XML for PMCID ", pmcid,
         ". Status: ", httr::status_code(resp2))
  }
  writeBin(httr::content(resp2, as = "raw"), out_file)
  message("Downloaded XML for PMCID ", pmcid, " → ", out_file)

  # 4. (Optional) parse or extract text if needed
  xml_doc <- read_xml(out_file)
  return(xml_doc)
}

