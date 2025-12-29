
{
library(stringr)
library(readxl)
library(dplyr)
library(stringi)
library(rentrez)
library(xml2)
library(jsonlite)
library(tokenizers)
}

#################### dbgap, EGA, JGAS ####################
# papers with Data Archive IDs:
# dbgap, EAG, or JGAS ids


{
  sentences_df <- data.table::fread(
    here::here("output/gwas_cat/gwas_study_dbgap_ega_sentences.csv")
  )

  conversion <- data.table::fread(
    here::here("output/gwas_cat/gwas_pubmed_to_pmcid_mapping.csv")
  )

# studies with at least one dbgap id:
dbgap_pmcids <- sentences_df |>
  dplyr::filter(dbgap_id != "") |>
  dplyr::pull(pmcid) |>
  unique()

dbgap_pmids <-
  conversion |>
  filter(pmcids %in% dbgap_pmcids) |>
  pull(pmids)

ega_pmcids <- sentences_df |>
  dplyr::filter(ega_id != "") |>
  dplyr::pull(pmcid) |>
  unique()

ega_pmids <-
  conversion |>
  filter(pmcids %in% ega_pmcids) |>
  pull(pmids)

jgas_pmcids <- sentences_df |>
  dplyr::filter(jgas_id != "") |>
  dplyr::pull(pmcid) |>
  unique()

jgas_pmids <-
  conversion |>
  filter(pmcids %in% jgas_pmcids) |>
  pull(pmids)


}

####################### cohort names #########
{
cohort_names <- readxl::read_xlsx(here::here("data/cohort/cohort_desc.xlsx"),
                                  sheet = 1)


cohort_full_names = cohort_names$full_name[!is.na(cohort_names$full_name)]
cohort_full_names <- str_trim(cohort_full_names)
cohort_full_names <- iconv(cohort_full_names, to = "UTF-8")
cohort_full_names <- gsub("[\u00A0\r\n]", " ", cohort_full_names)  # replace non-breaking spaces, CR, LF with space
cohort_full_names <- str_squish(cohort_full_names)  # trims and removes extra spaces
# sort by length of name (longest first) to match longest names first
cohort_full_names <- cohort_full_names[order(-nchar(cohort_full_names))]
cohort_full_names <- cohort_full_names[cohort_full_names != "Not Reported"]


cohort_abbr_names = cohort_names$cohort[!is.na(cohort_names$cohort)]
cohort_abbr_names <- str_trim(cohort_abbr_names)
cohort_abbr_names <- iconv(cohort_abbr_names, to = "UTF-8")
cohort_abbr_names <- gsub("[\u00A0\r\n]", " ", cohort_abbr_names)  # replace non-breaking spaces, CR, LF with space
cohort_abbr_names <- str_squish(cohort_abbr_names)  # trims and removes extra spaces
# remove abbreviations that are too short
cohort_abbr_names <- cohort_abbr_names[nchar(cohort_abbr_names) >= 4]
small_abbr_to_keep <- c("C4D",
                        "BBJ",
                        "UKB",
                        "MVP",
                        "TWB",
                        "QBB",
                        "MEC"
)
cohort_abbr_names <- unique(c(cohort_abbr_names,
                              small_abbr_to_keep
))
cohort_abbr_names <- cohort_abbr_names[!str_detect(pattern = "\\?", cohort_abbr_names)]
# sort by length of name (longest first) to match longest names first
cohort_abbr_names <- cohort_abbr_names[order(-nchar(cohort_abbr_names))]
}


#################### cohort mentioned in abstract ####################

{

  ## Step 1:
  # get only disease studies
  gwas_study_info <- data.table::fread(here::here("output/gwas_cat/gwas_study_info_trait_group_l2.csv"))

  gwas_study_info = gwas_study_info |>
    dplyr::rename_with(~ gsub(" ", "_", .x))

  gwas_study_info =
    gwas_study_info |>
    dplyr::filter(DISEASE_STUDY == T) |>
    dplyr::select(-COHORT)

  pmids = sort(gwas_study_info$PUBMED_ID) |> unique()

  abstract_files <- list.files(here::here("output/abstracts/"),
                               pattern = "*.txt",
                               full.names = FALSE
  )

  pmids_with_abstracts = gsub("\\.txt$", "", abstract_files)

  abstracts <- sapply(abstract_files,
                      function(file) {
                        readLines(here::here(paste0("output/abstracts/",file)),
                                  warn = FALSE) |>
                          paste(collapse = " ")
                      }
  )

  pmids = pmids[pmids %in% pmids_with_abstracts]

  cohort_sentences_df <- extract_cohort_sentences(abstracts,
                                                  cohort_full_names,
                                                  ignore_case = TRUE
  )

  cohort_sentences_df_p2 = extract_cohort_sentences(abstracts,
                                                    cohort_abbr_names,
                                                    ignore_case = FALSE
  )

  abstract_ids_full_cohort_names <-
  cohort_sentences_df|>
    filter(has_cohort == T) |>
    pull(abstract_id) |>
    unique()

  pmids_with_cohort_abstract = pmids[abstract_ids_full_cohort_names]

  writeLines(as.character(pmids_with_cohort_abstract),
             here::here("figures/pmids_with_cohort_abstract.txt")
  )

  abstract_ids_abbrev_cohort_names <-
    cohort_sentences_df_p2 |>
    filter(has_cohort == T) |>
    pull(abstract_id) |>
    unique()

  pmids_with_abbrev_abstract = pmids[abstract_ids_abbrev_cohort_names]

  writeLines(as.character(pmids_with_abbrev_abstract),
             here::here("figures/pmids_with_abbrev_abstract.txt")
  )

  pmids_with_cohort_abstract <-
    readLines(here::here("figures/pmids_with_cohort_abstract.txt"))

  pmids_with_abbrev_abstract <-
    readLines(here::here("figures/pmids_with_abbrev_abstract.txt"))

  abstract_cohort_pmids <-
    unique(c(pmids_with_cohort_abstract,
             pmids_with_abbrev_abstract
    ))


}

#################### cohort mentioned in abstract ####################

europeanpmc_full_texts <-
  list.files(here::here("output/fulltexts/europepmc"),
             pattern = "\\.txt$"
  )

# get pmcids of these files
europeanpmc_full_texts <-
  gsub("\\.txt$",
       "",
       europeanpmc_full_texts
  )

pmcids = conversion |>
  filter(pmcids != "") |>
  pull(pmcids) |>
  unique()

left_over_pmcids = pmcids[!pmcids %in% europeanpmc_full_texts]

author_manu = data.table::fread(here::here("output/fulltexts/aws_locations/author_manuscript.filelist.txt"))
oa_noncomm = data.table::fread(here::here("output/fulltexts/aws_locations/oa_noncomm.filelist.txt"))

author_manu_to_get <-
  author_manu |>
  dplyr::filter(AccessionID %in% left_over_pmcids)

oa_noncomm_to_get =
  oa_noncomm |>
  dplyr::filter(AccessionID %in% left_over_pmcids)

ncbi_texts <-   c(oa_noncomm_to_get$AccessionID,
                  author_manu_to_get$AccessionID)

all_fulltexts =
  c(
    list.files(here::here("output/fulltexts/europepmc"),
               pattern = "\\.txt$",
               full.names = TRUE
    ),
    here::here(paste0("output/fulltexts/", ncbi_texts, ".txt"))
  )

full_texts <- sapply(all_fulltexts,
                    function(file) {
                      readLines(file,
                                warn = FALSE) |>
                        paste(collapse = " ")
                    }
                    )


all_fulltexts_pmcids =
  all_fulltexts |>
  gsub(pattern = ".*/", replacement = "") |>
  gsub(pattern = "\\.txt$", replacement = "")


cohort_sentences_fulltext_df <- extract_cohort_sentences(full_texts,
                                                  cohort_full_names,
                                                  ignore_case = TRUE)


cohort_sentences_fulltext_df_p2 <- extract_cohort_sentences(full_texts,
                                                    cohort_abbr_names,
                                                    ignore_case = FALSE
)

fulltext_ids_full_cohort_names <-
  cohort_sentences_fulltext_df |>
  filter(has_cohort == T) |>
  pull(abstract_id) |>
  unique()

pmcids_with_cohort_fulltext =
  all_fulltexts_pmcids[fulltext_ids_full_cohort_names]

pmids_with_cohort_fulltext = conversion |>
  filter(pmcids %in% pmcids_with_cohort_fulltext) |>
  pull(pmids) |>
  unique()

writeLines(as.character(pmids_with_cohort_fulltext),
          here::here("figures/pmids_with_cohort_fulltext.txt")
          )

fulltext_ids_abbrev_cohort_names <-
  cohort_sentences_fulltext_df_p2 |>
  filter(has_cohort == T) |>
  pull(abstract_id) |>
  unique()

pmcids_with_abbrev_fulltext =
  all_fulltexts_pmcids[fulltext_ids_abbrev_cohort_names]

pmids_with_abbrev_fulltext = conversion |>
  filter(pmcids %in% pmcids_with_abbrev_fulltext) |>
  pull(pmids) |>
  unique()

writeLines(as.character(pmids_with_abbrev_fulltext),
           here::here("figures/pmids_with_abbrev_fulltext.txt")
)

pmids_with_cohort_fulltext <-
readLines(here::here("figures/pmids_with_cohort_fulltext.txt"))

pmids_with_abbrev_fulltext <-
  readLines(here::here("figures/pmids_with_abbrev_fulltext.txt"))

fulltext_cohort_pmids = c(pmids_with_cohort_fulltext,
                         pmids_with_abbrev_fulltext) |>
  unique()

#################### cohort info in GWAS catalog ####################
{

gwas_study_info_cohort =
  data.table::fread(here::here("output/gwas_cohorts/gwas_cohort_name_corrected.csv"))

gwas_study_info_cohort =
  gwas_study_info_cohort |>
  dplyr::rename_with(~ gsub(" ", "_", .x))

gwas_study_info_cohort =
  gwas_study_info_cohort |>
  dplyr::filter(STUDY_ACCESSION %in% unique(gwas_study_info$STUDY_ACCESSION))

# gwas_study_info = gwas_study_info |>
#   dplyr::rename_with(~ gsub(" ", "_", .x))
#
# gwas_study_info =
#   gwas_study_info |>
#   dplyr::filter(DISEASE_STUDY == T) |>
#   dplyr::select(-COHORT)
#

#
# gwas_study_info_cohort =
#   gwas_study_info_cohort |>
#   select(STUDY_ACCESSION,
#          COHORT) |>
#   distinct()
#
# gwas_study_info =
#   left_join(gwas_study_info,
#             gwas_study_info_cohort,
#             by = "STUDY_ACCESSION"
#   )

# with complete cohort info:
complete_pmids =
gwas_study_info_cohort |>
  dplyr::mutate(cohort = stringr::str_remove_all(pattern = "(^NR$)|(^NR\\|)|(\\|NR$)|(\\|NR\\|)",
                                                 COHORT)) |>
  dplyr::mutate(cohort = stringr::str_remove_all(pattern = "(^other$)|(^other\\|)|(\\|other$)(\\|other|\\|)",
                                                 tolower(COHORT))) |>
dplyr::mutate(cohort = stringr::str_remove_all(pattern = "(^multiple$)|(^multiple\\|)|(\\|multiple$)(\\|multiple|\\|)",
                                                                 tolower(COHORT))) |>
  filter(cohort != "") |>
  dplyr::pull(STUDY_ACCESSION) |>
  unique()

complete_pmids =
gwas_study_info |>
  filter(STUDY_ACCESSION %in% complete_pmids) |>
  dplyr::pull(PUBMED_ID) |>
  unique()

}

####################### all publications #####################

all_pmids =
  gwas_study_info |>
  pull(PUBMED_ID) |>
  unique()



###################### Bar plot #####################


upset_plot_data <- list(
  "dbGaP ids\n(Full text)" = dbgap_pmids,
  "EGA ids\n(Full text)" = ega_pmids,
  "JGAS ids\n(Full text)" = jgas_pmids,
  "Author Provided\nCohort Metadata\n(GWAS Catalog)" = complete_pmids,
  "Cohort\nMentioned\n(Abstracts)" = abstract_cohort_pmids,
  "Cohort\nMentioned\n(Full text)" = fulltext_cohort_pmids
  #"All\nPublications" = all_pmids
)

all_pmids_union =
  Reduce(union, upset_plot_data)

data.frame(n_publications = c(all_pmids_union, all_pmids),
           source =
             c(rep("All with likely cohort info", length(all_pmids_union)),
               rep("All Publications", length(all_pmids))
               ) )|>
ggplot(aes(x = source, y = n_publications)) +
  geom_col()


bar_plot_df <-
data.frame(source = c(rep("dbGaP ids\n(Full text)", length(dbgap_pmids)),
                      rep("EGA ids\n(Full text)", length(ega_pmids)),
                      rep("JGAS ids\n(Full text)", length(jgas_pmids)),
                      rep("Author Provided\nCohort Metadata\n(GWAS Catalog)", length(complete_pmids)),
                      rep("Cohort\nMentioned\n(Abstracts)", length(abstract_cohort_pmids)),
                      rep("Cohort\nMentioned\n(Full text)", length(fulltext_cohort_pmids)),
                      rep("At least\none source", length(all_pmids_union))
                      ),

           pmids = c(dbgap_pmids,
                     ega_pmids,
                     jgas_pmids,
                     complete_pmids,
                     abstract_cohort_pmids,
                     fulltext_cohort_pmids,
                     all_pmids_union
           )
                        )

# bar_plot_df <-
# upset_plot_data  |>
#   as.data.frame() |>
#   tidyr::pivot_longer(
#     cols = everything(),
#     names_to = "source",
#     values_to = "pmids"
#   ) |>
#   distinct()

bar_plot_df <-
  bar_plot_df |>
  dplyr::mutate(author_provided = ifelse(pmids %in% complete_pmids,
                                        TRUE,
                                        FALSE
  )
  ) |>
  group_by(source) |>
  summarise(n_publications =  n(),
            n_author_provided = sum(author_provided)
  )

# Make a numeric x variable so we can offset "All"
bar_plot_df <-
  bar_plot_df |>
  mutate(
    x_pos = as.numeric(factor(source,
                              levels = c("dbGaP ids\n(Full text)",
                                         "EGA ids\n(Full text)",
                                         "JGAS ids\n(Full text)",
                                         "Author Provided\nCohort Metadata\n(GWAS Catalog)",
                                         "Cohort\nMentioned\n(Abstracts)",
                                         "Cohort\nMentioned\n(Full text)",
                                         "At least\none source"))),
    # add an offset only for "All"
    x_pos = ifelse(source == "All", x_pos + 0.5, x_pos)
  )


bar_plot_df <-
  bar_plot_df |>
  mutate(facet_name = ifelse(source == "At least\none source",
                             "All Publications",
                             "Publications with Cohort Information"
  )
  )

bar_plot_df <- bar_plot_df |>
  mutate(
    facet_name = factor(facet_name,
                        levels = rev(unique(facet_name)))  # reverse order
  )


# bar_plot_df <-
# list(
#   "dbGaP ids\n(Full text)" = length(dbgap_pmids),
#   "EGA ids\n(Full text)" = length(ega_pmids),
#   "JGAS ids\n(Full text)" = length(jgas_pmids),
#   "Cohort\nMentioned\n(Abstracts)" = length(abstract_cohort_pmids),
#   "Cohort\nMentioned\n(Full text)" = length(fulltext_cohort_pmids),
#   #"All\nPublications" = length(all_pmids)
# ) |>
#   as.data.frame() |>
  # tibble::rownames_to_column(var = "source") |>
  # dplyr::rename(n_publications = V1) |>
#
# library(RColorBrewer)
# library(ggpubr)
# plot =
#   ggbarplot(
#     bar_plot_df,
#     x = "source",
#     y = "n_publications",
#     sort.val = "desc",
#     facet.by = "facet_name"
#   )

plot =
bar_plot_df |>
  ggplot(aes(x = #x_pos,
               reorder(source,
                         n_publications,
                         decreasing = T),
             y=n_publications,
             fill = source)) +
  geom_bar(stat="identity", colour = "black") +
  scale_fill_manual(
    values = c(
      "At least\none source" = "black",          # make this one black
      setNames(RColorBrewer::brewer.pal(6, "Dark2"),
               setdiff(unique(bar_plot_df$source),
                       "At least\none source")) # other sources keep palette
    )
  ) +
  # annotate("text",
  #          x = Inf,
  #          y = length(all_pmids),
  #          label = paste0("Total number of published GWAS catalog studies of disease: ",
  #                         length(all_pmids)),
  #          hjust = 1.1,
  #          vjust = -0.5,
  #          size = 7
  # ) +
  facet_grid (~facet_name,
              #rows_insert() = 1,
              scales = "free_x",
              space = "free_x") +
  # facet_wrap(~ facet_name,
  #            scales = "free_x",
  #            nrow = 1,
  #            shrink = T
  #            #space = "free_x"   # allows facets to take different widths
  # ) +
  theme_bw(base_size = 20) +
  #scale_fill_brewer(palette = "Dark2") +   # choose a ColorBrewer palette
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),      # remove panel border
        axis.line = element_line(color = "black"),  # keep axis border
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.x = unit(2, "cm")
        ) +
  # geom_hline(yintercept = length(all_pmids),
  #            linetype = "dashed",
  #            color = "red",
  #            linewidth = 1) +
  # scale_x_continuous(
  #   breaks = bar_plot_df$x_pos,
  #   labels = bar_plot_df$source,
  #   expand = expansion(mult = c(0.02, 0.1)) # some padding on right
  # ) +
  labs(x = "Cohort Information Source",
       y = "Number of Publications") +
  ylim(0, 5000)


# # Combine them (line overlaid on full width)
# p_line + plot + plot_layout(heights = c(0, 1))

# Open PDF device
cairo_pdf(here::here("figures/cohort_information_source.pdf"),
          width = 13,
          height = 4.5)

print(plot)
#library(grid)

# Then overlay a horizontal line across the full plotting region
grid.lines(
  x = unit(c(0.087, 1), "npc"),    # full width (npc = normalized parent coordinates)
  y = unit(0.853, "npc"),       # adjust vertical position
  gp = gpar(
    col = "red",
    lwd = 2,                   # line width
    lty = "dashed"              # dashed line style
  )
)

# Add one global annotation
grid.text(
  paste0("Total number of published GWAS catalog studies of disease: ",
         length(all_pmids)),
  x = 0.5, y = 0.9,
  gp = gpar(fontsize = 16,
            col = "red", fontface = "bold")
)

# Close PDF device (saves file)
dev.off()

# ggsave(
#   here::here("figures/cohort_information_source.pdf"),
#   plot,
#   width = 13,
#   height = 5,
#   device = cairo_pdf
# )


####################### UpSet plot #####################

library(ComplexHeatmap)

upset_plot_data <- list(
  "dbGaP ids\n(Full text)" = dbgap_pmids,
  "EGA ids\n(Full text)" = ega_pmids,
  "JGAS ids\n(Full text)" = jgas_pmids,
  "Author Provided\nCohort Metadata\n(GWAS Catalog)" = complete_pmids,
  "Cohort\nMentioned\n(Abstracts)" = abstract_cohort_pmids
  #"All\nPublications" = all_pmids
)

upset_plot_data <-list_to_matrix(upset_plot_data,
                                 universal_set = all_pmids)

  m1 = make_comb_mat(upset_plot_data)


  UpSet(m1)



  pmcids_from_full_texts <- sapply(all_full_texts,
                                 function(file) {
                                   # extract pmcid from filename
                                   basename(file) |>
                                     stringr::str_remove(pattern = "\\.txt$")
                                 }
  )

  pmcids = unique(pmcids_from_full_texts)

  pmids = conversion |>
    filter(pmcids %in% pmcids_from_full_texts) |>
    pull(pmids)
  cohort = cohort[pmids]
  date = date[pmids]
  country = country[pmids]

  cohort_sentences_fulltext_df <- extract_cohort_sentences(,
                                                  cohort_full_names,
                                                  ignore_case = TRUE
  )
