#iciteR
# iCiteR::get_metrics("27599104")
# iciteR - believe doesn't work because of &format=cv ain't working properly


# pmid <- "23456789"
# pmid = c('27599104', '27830815', '28968388', '28968381')

# Load required packages
library(httr)
library(jsonlite)

# Function to fetch one chunk of PMIDs
fetch_icite_chunk <- function(pmid_chunk) {

  pmid_vec <- paste0(pmid_chunk, collapse = ",")

url <- paste0("https://icite.od.nih.gov/api/pubs?pmids=",
              pmid_vec
              #"&format=csv" - doesn't work for rcr ...
              )  # Replace with real URL

# Make GET request
response <- GET(url)

#df <- read.csv(text = content(response, "text"))
# Check status
#stop_for_status(response)

# Parse JSON content
data_list <- fromJSON(content(response, "text"), flatten = TRUE)

# Convert to a data frame
# If it's a single publication, convert it like this:
pub_df <- as.data.frame(data_list)

pub_df = pub_df |> dplyr::rename_all(~gsub("data.", "", .x))

pub_df = pub_df |> dplyr::select(-c(citedByPmidsByYear))

# View the data
#print(pub_df)
}

gwas_study_info = data.table::fread("data/gwas_catalog/gwas-catalog-v1.0.3.1-studies-r2025-06-10.tsv",
                                    sep = "\t",
                                    quote = "")

gwas_study_info = gwas_study_info |>
  dplyr::rename_all(~gsub(" ", "_", .x))

gwas_study_info = gwas_study_info |>
  dplyr::select(FIRST_AUTHOR, DATE, JOURNAL, PUBMED_ID) |>
  dplyr::distinct()

pmid = gwas_study_info$PUBMED_ID

pmid_chunks <- split(pmid, ceiling(seq_along(pmid)/400))

# Fetch and combine all chunks
all_results <- purrr::map_dfr(pmid_chunks, fetch_icite_chunk)

library(ggplot2)

all_results |>
  ggplot(aes(x = relative_citation_ratio)) +
  geom_histogram() +
  theme_bw()

all_results |>
  ggplot(aes(x = citation_count)) +
  geom_histogram() +
  theme_bw()

cor(all_results$citation_count,
    all_results$relative_citation_ratio,
    method = "spearman",
    use = "pairwise.complete.obs")

cor(all_results$citations_per_year,
    all_results$relative_citation_ratio,
    method = "spearman",
    use = "pairwise.complete.obs")


summary(all_results$citation_count)


all_results |>
  ggplot(aes(x = citation_count + 1, y=relative_citation_ratio)) +
  scale_x_log10() +
  geom_point() +
#  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

all_results |>
  ggplot(aes(x =expected_citations_per_year, y=relative_citation_ratio)) +
  scale_x_log10() +
  geom_point() +
  #  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

all_results |>
  ggplot(aes(x = field_citation_rate, y=relative_citation_ratio)) +
  scale_x_log10() +
  geom_point() +
  #  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

head(all_results) |>
  dplyr::select(field_citation_rate,
                expected_citations_per_year,
                citations_per_year,
                relative_citation_ratio
                ) |>
  dplyr::mutate(rcr = citations_per_year / expected_citations_per_year)


all_results |>
  ggplot(aes(x = citations_per_year+1,
             y=relative_citation_ratio)) +
  geom_point() +
  theme_bw()



summary(all_results$relative_citation_ratio) # 287 NAs

all_results |>
  dplyr::filter(is.na(relative_citation_ratio)) |>
  ggplot(aes(x = year)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(min(all_results$year, na.rm = TRUE),
                                  max(all_results$year, na.rm = TRUE),
                                  by = 1)) +
  #geom_histogram() +
  theme_bw()

# so all na rcr is from this year (2025), and last year (2024)

# what about 0 rcr?
all_results |>
  dplyr::filter(relative_citation_ratio == 0) |>
  ggplot(aes(x = year)) +
  geom_histogram() +
  scale_x_continuous(breaks = seq(min(all_results$year, na.rm = TRUE),
                                  max(all_results$year, na.rm = TRUE),
                                  by = 1)) +
  #geom_histogram() +
  theme_bw()

# all 0 rcr is from <=2023
all_results |>
  dplyr::filter(relative_citation_ratio == 0) |>
  nrow() # 37 zero rcr ..

all_results |>
  dplyr::filter(relative_citation_ratio == 0) |>
  dplyr::select(field_citation_rate,
                expected_citations_per_year,
                citation_count,
                citations_per_year,
                relative_citation_ratio
  )
# 0 rcr correspond to zero citation count



all_results |>
  dplyr::filter(!is.na(relative_citation_ratio)) |>
  dplyr::filter(relative_citation_ratio != 0) |>
  ggplot(aes(x = year, y = relative_citation_ratio)) +
  #scale_y_log10() +
  geom_point() +
  theme_bw()

all_results |>
  dplyr::filter(!is.na(relative_citation_ratio)) |>
  dplyr::filter(relative_citation_ratio != 0) |>
  ggplot(aes(x = year, y = relative_citation_ratio)) +
  scale_y_log10() +
  geom_point() +
  theme_bw()

all_results |>
  dplyr::filter(!is.na(relative_citation_ratio)) |>
  dplyr::filter(relative_citation_ratio != 0) |>
  ggplot(aes(x = factor(year), y = relative_citation_ratio)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_y_log10() +
  theme_bw()

all_results |>
  dplyr::filter(!is.na(relative_citation_ratio)) |>
  dplyr::filter(relative_citation_ratio != 0) |>
  ggplot(aes(x = year, y = citations_per_year)) +
  #scale_y_log10() +
  geom_point() +
  theme_bw()
