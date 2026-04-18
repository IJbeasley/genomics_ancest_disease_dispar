#!/usr/bin/env Rscript
# ============================================================
# oxford_html_to_xml.R
#
# Converts Oxford Academic (Silverchair) article HTML pages
# to structured XML.  Works on full-text pages saved from
# academic.oup.com.
#
# Usage (command line):
#   Rscript oxford_html_to_xml.R input.html [output.xml]
#   Rscript oxford_html_to_xml.R *.html          # batch
#
# Usage (from R):
#   source("oxford_html_to_xml.R")
#   convert_oxford_html("21088011.html", "21088011.xml")
#   batch_convert_oxford("./html_files/", "./xml_files/")
#
# Dependencies: rvest, xml2, jsonlite, stringr
# Install:  install.packages(c("rvest","xml2","jsonlite","stringr"))
# ============================================================

suppressPackageStartupMessages({
  library(rvest)
  library(xml2)
  library(jsonlite)
  library(stringr)
})

# ----------------------------------------------------------------
# NULL coalescing operator  (defined early so all functions can use it)
# ----------------------------------------------------------------
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(a[1])) a else b
}

# ----------------------------------------------------------------
# Helper: clean whitespace from extracted text
# ----------------------------------------------------------------
clean_text <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  x <- str_trim(x)
  x <- str_replace_all(x, "\\s+", " ")
  x
}

# ----------------------------------------------------------------
# Extract metadata from <script> dataLayer JSON block
# ----------------------------------------------------------------
extract_datalayer <- function(page) {

  scripts <- page |>
             html_nodes("script") |>
             html_text()

  dl_script <- scripts[grepl("var dataLayer", scripts, fixed = TRUE)]
  if (length(dl_script) == 0) return(list())

  m <- regmatches(dl_script[1],
                  regexpr("\\[\\{.+?\\}\\]", dl_script[1], perl = TRUE))
  if (length(m) == 0) return(list())

  tryCatch({
    parsed <- fromJSON(m, simplifyVector = FALSE)
    if (is.list(parsed) && length(parsed) > 0) parsed[[1]] else list()
  }, error = function(e) list())
}

# ----------------------------------------------------------------
# Extract metadata from <script type="application/ld+json">
# ----------------------------------------------------------------
extract_schema_org <- function(page) {
  ld_scripts <- page %>%
    html_nodes('script[type="application/ld+json"]') %>%
    html_text()

  for (s in ld_scripts) {
    parsed <- tryCatch(fromJSON(s, simplifyVector = FALSE),
                       error = function(e) NULL)
    if (!is.null(parsed) && !is.null(parsed[["@type"]]) &&
        grepl("ScholarlyArticle", parsed[["@type"]])) {
      return(parsed)
    }
  }
  list()
}

# ----------------------------------------------------------------
# Extract citation_* <meta> tags
# ----------------------------------------------------------------
extract_meta <- function(page) {
  nodes <- page %>% html_nodes("meta[name]")
  names  <- nodes %>% html_attr("name")
  values <- nodes %>% html_attr("content")
  setNames(as.list(values), names)
}

# ----------------------------------------------------------------
# Build <article-meta> block
# ----------------------------------------------------------------
build_article_meta <- function(xml_root, page, dl, schema, meta) {

  am <- xml2::xml_add_child(xml_root, "article-meta")

  print("Maybe here 1")

  # --- DOI ---
  doi <- dl[["doi"]] %||% meta[["citation_doi"]] %||% ""
  if (nzchar(doi)) {
    ai <- xml2::xml_add_child(am, "article-id", `pub-id-type` = "doi")
    xml_text(ai) <- doi
  }

  print("Maybe here 2")

  # --- Title ---
  title <- dl[["full_title"]] %||%
    meta[["citation_title"]] %||%
    (page %>% html_node("h1.article-title") %>% html_text(trim = TRUE)) %||% ""
  if (nzchar(title)) {
    tg <- xml2::xml_add_child(am, "title-group")
    at <- xml2::xml_add_child(tg, "article-title")
    xml_text(at) <- clean_text(title)
  }

  print("Maybe here 3")

  # --- Authors ---
  build_authors(am, page, schema, meta)

  print("Maybe here 5")

  # --- Publication date ---
  pub_date <- dl[["article_date"]] %||%
    meta[["citation_publication_date"]] %||% ""
  if (nzchar(pub_date)) {

    parts <- str_split(pub_date, "[/-]")[[1]]

    pd <- xml2::xml_add_child(am,
                              "pub-date",
                              `pub-type` = "epub",
                              "year" = if (length(parts) >= 1) parts[1] else NULL,
                              "month" = if (length(parts) >= 2) parts[2] else NULL,
                              "day" = if (length(parts) >= 3) parts[3] else NULL
                              )

    print(parts)
    # if (length(parts) >= 1) pd <- xml2::xml_add_child(pd, "year" =  parts[1])
    # if (length(parts) >= 2) pd <- xml2::xml_add_child(pd, "month" = parts[2])
    # if (length(parts) >= 3) pd <- xml2::xml_add_child(pd, "day" = parts[3])
  }

  print("Maybe here 6")

  # --- Volume / Issue / Pages ---
  vol   <- meta[["citation_volume"]]  %||% ""
  issue <- meta[["citation_issue"]]   %||% ""
  fpage <- meta[["citation_firstpage"]] %||% ""
  lpage <- meta[["citation_lastpage"]]  %||% ""
  journal_title <- meta[["citation_journal_title"]] %||% ""
  journal_abbr  <- meta[["citation_journal_abbrev"]]  %||% ""
  issn          <- meta[["citation_issn"]]            %||% ""

  am <- xml2::xml_add_child(am,
                            "article-meta",
                            "volume" = if (nzchar(vol)) vol else NULL,
                            "issue"  = if (nzchar(issue)) issue else NULL,
                            "fpage"  = if (nzchar(fpage)) fpage else NULL,
                            "lpage"  = if (nzchar(lpage)) lpage else NULL,
                            "journal-title" = if (nzchar(journal_title)) journal_title else NULL,
                            "abbrev-journal-title" = if (nzchar(journal_abbr)) journal_abbr else NULL,
                            "issn" = if (nzchar(issn)) issn else NULL
                            )


  # if (nzchar(vol))   xml_text(xml2::xml_add_child(am, "volume"))  <- vol
  # if (nzchar(issue)) xml_text(xml2::xml_add_child(am, "issue"))   <- issue
  # if (nzchar(fpage)) xml_text(xml2::xml_add_child(am, "fpage"))   <- fpage
  # if (nzchar(lpage)) xml_text(xml2::xml_add_child(am, "lpage"))   <- lpage

  print("Maybe here 7")
  # --- Journal ---




  # if (nzchar(journal_title) || nzchar(issn)) {
  #   jm <- xml2::xml_add_child(am, "journal-meta")
  #   if (nzchar(journal_title)) xml_text(xml2::xml_add_child(jm, "journal-title")) <- journal_title
  #   if (nzchar(journal_abbr))  xml_text(xml2::xml_add_child(jm, "abbrev-journal-title")) <- journal_abbr
  #   if (nzchar(issn))          xml_text(xml2::xml_add_child(xml2::xml_add_child(jm, "issn"), ".")) <- issn
  # }



  # --- Keywords ---
  kwd_nodes <- page |>
               html_nodes(".kwd-group .kwd-part")

  print(kwd_nodes)

  kwds <- html_text(kwd_nodes, trim = TRUE)

  kg <- xml2::xml_add_child(am,
                            "kwd-group",
                            "kwd-group-type" = "data-keywords",
                            "kwd" = if (length(kwds) > 0) kwds else NULL
                            )

  print("Ok ok ok ok")

  # if (length(kwd_nodes) > 0) {
  #   kg <- xml2::xml_add_child(am, "kwd-group")
  #   for (k in html_text(kwd_nodes, trim = TRUE)) {
  #     xml_text(xml2::xml_add_child(kg, "kwd")) <- clean_text(k)
  #   }
  # }

  # --- Abstract ---
  build_abstract(am, page)

  print("plz")
}

# ----------------------------------------------------------------
# Build <contrib-group> with authors and affiliations
# ----------------------------------------------------------------
build_authors <- function(am, page, schema, meta) {

  # Prefer schema.org author list (richer structure)
  authors_schema <- schema[["author"]]

  print("Hey build authors 1")

  # Fallback to meta citation_author tags
  meta_names   <- unname(unlist(meta[names(meta) == "citation_author"]))
  meta_affils  <- unname(unlist(meta[names(meta) == "citation_author_institution"]))

  print("Hey build author 2")

  if (is.null(authors_schema) && length(meta_names) == 0) return(invisible(NULL))

  cg <- xml2::xml_add_child(am, "contrib-group")

  print("Hey build author 4")

  if (!is.null(authors_schema)) {

    print("Hey build author 5")
    for (auth in authors_schema) {

      contrib <- xml2::xml_add_child(cg, "contrib",
                                     `contrib-type` = "author")
      print("Hey build author 7")
      # name is "Surname, Given"
      nm <- auth[["name"]] %||% ""
      parts <- str_split(nm, ",\\s*")[[1]]
      ng <- xml2::xml_add_child(contrib,
                                "name",
                                "surname" = clean_text(parts[1]),
                                "given-names" = clean_text(parts[2] %||% "")
                                )

      print(ng)

      print("Hey build author 8")

      print(clean_text(parts[1]))
      print(xml_text(xml2::xml_add_child(ng, "surname")))
      print(parts)


      #xml2::xml_add_child(ng, "surname" = clean_text(parts[1])
      #xml_text(xml2::xml_add_child(ng, "given-names")) <- clean_text(parts[2] %||% "")

      print("Hey building author 9")

      aff_text <- auth[["affiliation"]] %||% ""
      if (nzchar(aff_text)) {
        aff <- xml2::xml_add_child(contrib, "aff")
        xml_text(aff) <- clean_text(aff_text)
      }
    }
  } else {
    print("Hey build author 6")
    for (nm in meta_names) {
      contrib <- xml2::xml_add_child(cg, "contrib", `contrib-type` = "author")
      parts <- str_split(nm, ",\\s*")[[1]]
      ng <- xml2::xml_add_child(contrib, "name")
      xml_text(xml2::xml_add_child(ng, "surname"))     <- clean_text(parts[1])
      xml_text(xml2::xml_add_child(ng, "given-names")) <- clean_text(parts[2] %||% "")
    }
  }
}

# ----------------------------------------------------------------
# Build <abstract> block
# ----------------------------------------------------------------
build_abstract <- function(am, page) {

  print("Hello darkness my old friend")

  abs_node <- page %>% html_node("section.abstract")
  if (is.null(abs_node)) return(invisible(NULL))


  print("Hello darkness my old friend 2")

  abs_xml <- xml2::xml_add_child(am, "abstract")

  # Each sub-section of the abstract (Aims, Methods, Conclusion…)
  secs <- abs_node %>% html_nodes(".sec")
  if (length(secs) > 0) {
    for (sec in secs) {

      title_node <- sec |> html_node(".title")

      sec_xml <- xml2::xml_add_child(abs_xml,
                                     "sec",
                                     "title" = if (!is.null(title_node)) clean_text(html_text(title_node, trim = TRUE)) else NULL,
                                     "p" = if (length(sec %>% html_nodes("p.chapter-para")) > 0) "true" else NULL
                                     )





      # if (!is.null(title_node)) {
      #   xml_text(xml2::xml_add_child(sec_xml, "title")) <-
      #     clean_text(html_text(title_node, trim = TRUE))
      # }
      # paras <- sec %>% html_nodes("p.chapter-para")
      # for (p in paras) {
      #   p_xml <- xml2::xml_add_child(sec_xml, "p")
      #   xml_text(p_xml) <- clean_text(html_text(p, trim = TRUE))
      # }
    }
  } else {

    sec_xml <- xml2::xml_add_child(abs_xml,
                                   "sec",
                                   "title" = if (!is.null(title_node)) clean_text(html_text(title_node, trim = TRUE)) else NULL,
                                   "p" = if (length(sec %>% html_nodes("p.chapter-para")) > 0) "true" else NULL
    )

    # No sub-sections: just grab paragraphs
    # paras <- abs_node %>% html_nodes("p.chapter-para")
    # for (p in paras) {
    #   p_xml <- xml2::xml_add_child(abs_xml, "p")
    #   xml_text(p_xml) <- clean_text(html_text(p, trim = TRUE))
    # }
  }
}

# ----------------------------------------------------------------
# Build article body sections
# ----------------------------------------------------------------
build_body <- function(xml_root, page) {


  print("Love me")

  body_node <- page %>% html_node(".article-body.js-content-body, .article-body")
  if (is.null(body_node)) return(invisible(NULL))

  body_xml <- xml2::xml_add_child(xml_root, "body")

  # Gather all h2 section titles and interleaved paragraphs
  # We iterate over children of the Silverchair article widget
  widget <- page %>%
    html_node(".widget-ArticleFulltext, .widget-instance-OUP_Article_FullText_Widget")

  if (is.null(widget)) widget <- body_node

  children <- xml_children(read_html(as.character(widget)))

  current_sec <- NULL
  current_h2  <- NULL
  current_h3  <- NULL

  # Walk direct child nodes of the wrapper
  all_nodes <- widget %>%
    html_nodes("h2.section-title, h3.section-title, h4.section-title,
                p.chapter-para, .fig.fig-section, .table-wrap")

  for (node in all_nodes) {
    tag   <- html_name(node)
    class <- html_attr(node, "class") %||% ""

    if (tag == "h2") {
      current_sec <- xml2::xml_add_child(body_xml,
                                         "sec",
                                         "id" = if (nzchar(html_attr(node, "id"))) html_attr(node, "id") else NULL,
                                         "title" = clean_text(html_text(node, trim = TRUE))
                                         )
      xml_attr(current_sec, "id") <- html_attr(node, "id") %||% ""
      # xml_text(xml2::xml_add_child(current_sec, "title")) <-
      #   clean_text(html_text(node, trim = TRUE))
      current_h2 <- current_sec
      current_h3 <- NULL

    } else if (tag == "h3") {
      parent <- if (!is.null(current_h2)) current_h2 else body_xml
      current_sec <- xml2::xml_add_child(parent,
                                         "sec",
                                         "id" = if (nzchar(html_attr(node, "id"))) html_attr(node, "id") else NULL,
                                         "title" = clean_text(html_text(node, trim = TRUE))
                                         )


      # xml_attr(current_sec, "id") <- html_attr(node, "id") %||% ""
      # xml_text(xml2::xml_add_child(current_sec, "title")) <-
      #   clean_text(html_text(node, trim = TRUE))
      current_h3 <- current_sec

    } else if (tag == "h4") {
      parent <- if (!is.null(current_h3)) current_h3 else
        if (!is.null(current_h2)) current_h2 else body_xml
      current_sec <- xml2::xml_add_child(parent,
                                         "sec",
                                         "id" = if (nzchar(html_attr(node, "id"))) html_attr(node, "id") else NULL,
                                         "title" = clean_text(html_text(node, trim = TRUE))
                                         )

      # xml_attr(current_sec, "id") <- html_attr(node, "id") %||% ""
      # xml_text(xml2::xml_add_child(current_sec, "title")) <-
      #   clean_text(html_text(node, trim = TRUE))

    } else if (tag == "p" && grepl("chapter-para", class)) {
      target <- current_sec %||% body_xml
      p_xml <- xml2::xml_add_child(target,
                                   "p",
                                   "id" = if (nzchar(html_attr(node, "id"))) html_attr(node, "id") else NULL,
                                   "class" = "chapter-para"
                                   )

      print("Hello hello")
      xml_text(p_xml) <- clean_text(html_text(node, trim = TRUE))

      print("Thank you")

    } else if (grepl("fig-section", class)) {
      target <- current_sec %||% body_xml
      print("Baby")
      build_figure(target, node)
      print("Baby done")

    } else if (grepl("table-wrap", class)) {
      target <- current_sec %||% body_xml
      print("Table time")
      build_table(target, node)
      print("Hello table done")
    }
  }
}

# ----------------------------------------------------------------
# Build <fig> element
# ----------------------------------------------------------------
build_figure <- function(parent, fig_node) {
  fig_id  <- html_attr(fig_node, "data-id") %||% ""
  fig_xml <- xml2::xml_add_child(parent, "fig", id = fig_id)

  label <- fig_node %>% html_node(".fig-label") %>% html_text(trim = TRUE)
  if (!is.null(label) && nzchar(label)) {

    print("Hello figure label")

    print(clean_text(label))

    print("Hello figure label")

    xml_text(xml2::xml_add_child(fig_xml, "label")) <- clean_text(label)



  }

  caption <- fig_node %>% html_node(".fig-caption") %>% html_text(trim = TRUE)
  if (!is.null(caption) && nzchar(caption)) {
    cap_xml <- xml2::xml_add_child(fig_xml, "caption")
    xml_text(xml2::xml_add_child(cap_xml, "p")) <- clean_text(caption)
  }

  img <- fig_node %>% html_node("img.content-image")
  if (!is.null(img)) {
    graphic <- xml2::xml_add_child(fig_xml, "graphic")
    src <- html_attr(img, "src") %||% ""
    alt <- html_attr(img, "alt") %||% ""
    # strip CDN query string from src for cleaner XML
    src_clean <- sub("\\?.*", "", src)
    xml_attr(graphic, "xlink:href") <- src_clean
    if (nzchar(alt)) xml_attr(graphic, "alt") <- alt
  }
}

# ----------------------------------------------------------------
# Build <table-wrap> element
# ----------------------------------------------------------------
build_table <- function(parent, tbl_node) {

  tbl_id  <- html_attr(tbl_node, "id") %||%
    (tbl_node %>% html_node("table") %>% html_attr("id") %||% "")
  print("Table ID:")
  label <- tbl_node %>% html_node(".table-label") %>% html_text(trim = TRUE)


  tw_xml  <- xml2::xml_add_child(parent,
                                 "table-wrap",
                                 id = tbl_id,
                                 "class" = if (grepl("table-long", html_attr(tbl_node, "class"))) "table-long" else NULL,
                                 "label" = if (!is.null(label) && nzchar(label)) clean_text(label) else NULL
                                 )


  print("OKKKKK123")

  print("OKKKKKK4")

  # if (!is.null(label) && nzchar(label)) {
  #
  #   xml2::xml_add_child(tw_xml, "label" = clean_text(label))
  #
  # }
  print("OKKKKKK")
  caption <- tbl_node %>% html_node(".table-caption") %>% html_text(trim = TRUE)
  if (!is.null(caption) && nzchar(caption)) {
    cap_xml <- xml2::xml_add_child(tw_xml, "caption",
                                   "p" = clean_text(caption)
                                   )
    #xml2::xml_add_child(cap_xml, "p")) <- clean_text(caption)
  }
  print("OK")
  # Serialize the inner HTML table as-is (preserves structure)
  inner_tbl <- tbl_node %>% html_node("table")

  if (!is.null(inner_tbl)) {
    tbl_xml <- xml2::xml_add_child(tw_xml, "table")
    # Transfer rows
    rows <- inner_tbl %>% html_nodes("tr")
    current_section <- tbl_xml
    for (row in rows) {
      # Determine thead/tbody context by presence of th
      th_cells <- row %>% html_nodes("th")
      row_xml <- xml2::xml_add_child(current_section,
                                     "tr",
                                     "cells" = length(th_cells) + length(row %>% html_nodes("td, td"))
                                     )
      #cells <- row %>% html_nodes("th, td")
      for (cell in cells) {
        cell_tag <- html_name(cell)
        cs <- html_attr(cell, "colspan") %||% "1"
        rs <- html_attr(cell, "rowspan") %||% "1"
        cell_xml <- xml2::xml_add_child(row_xml,
                                        cell_tag,
                                        "colspan" = if (cs != "1") cs else NULL,
                                        "rowspan" = if (rs != "1") rs else NULL,
                                        "class" = if (grepl("table-header", html_attr(cell, "class"))) "table-header" else NULL,
                                        "text" = clean_text(html_text(cell, trim = TRUE))
                                        )
        # if (cs != "1") xml_attr(cell_xml, "colspan") <- cs
        # if (rs != "1") xml_attr(cell_xml, "rowspan") <- rs
        # xml_text(cell_xml) <- clean_text(html_text(cell, trim = TRUE))
      }
    }
  }

  # Table footnotes
  footnotes <- tbl_node %>% html_nodes(".footnote p")
  for (fn in footnotes) {
    fn_xml <- xml2::xml_add_child(tw_xml, "table-wrap-foot")
    xml_text(xml2::xml_add_child(fn_xml, "p")) <-
      clean_text(html_text(fn, trim = TRUE))
  }
}

# ----------------------------------------------------------------
# Build <back> section with references
# ----------------------------------------------------------------
build_back <- function(xml_root, page) {
  ref_list <- page %>% html_nodes(".ref-list .js-splitview-ref-item")
  if (length(ref_list) == 0) return(invisible(NULL))

  back_xml <- xml2::xml_add_child(xml_root, "back")
  rl_xml   <- xml2::xml_add_child(back_xml, "ref-list")

  for (ref_node in ref_list) {
    ref_id   <- html_attr(ref_node, "content-id") %||%
      html_attr(ref_node, "data-legacy-id") %||% ""
    ref_xml  <- xml2::xml_add_child(rl_xml, "ref", id = ref_id)

    # Label (number)
    label <- ref_node %>% html_node(".label.title-label") %>%
      html_text(trim = TRUE)
    if (!is.null(label) && nzchar(label))
      xml_text(xml2::xml_add_child(ref_xml, "label")) <- label

    ec <- xml2::xml_add_child(ref_xml, "element-citation",
                        `publication-type` = "journal")

    # Authors
    names_nodes <- ref_node %>% html_nodes(".person-group .name")
    if (length(names_nodes) > 0) {
      pg_xml <- xml2::xml_add_child(ec, "person-group",
                              `person-group-type` = "author")
      for (n in names_nodes) {
        surname     <- n %>% html_node(".surname")    %>% html_text(trim=TRUE)
        given_names <- n %>% html_node(".given-names")%>% html_text(trim=TRUE)
        nm_xml <- xml2::xml_add_child(pg_xml, "name")
        if (!is.null(surname)     && nzchar(surname))
          xml_text(xml2::xml_add_child(nm_xml, "surname"))     <- clean_text(surname)
        if (!is.null(given_names) && nzchar(given_names))
          xml_text(xml2::xml_add_child(nm_xml, "given-names")) <- clean_text(given_names)
      }
    }

    # Collab (group author)
    collab <- ref_node %>% html_node(".collab") %>% html_text(trim = TRUE)
    if (!is.null(collab) && nzchar(collab))
      xml_text(xml2::xml_add_child(ec, "collab")) <- clean_text(collab)

    # Article title
    art_title <- ref_node %>% html_node(".article-title") %>%
      html_text(trim = TRUE)
    if (!is.null(art_title) && nzchar(art_title))
      xml_text(xml2::xml_add_child(ec, "article-title")) <- clean_text(art_title)

    # Source (journal name)
    source <- ref_node %>% html_node(".source") %>% html_text(trim = TRUE)
    if (!is.null(source) && nzchar(source))
      xml_text(xml2::xml_add_child(ec, "source")) <- clean_text(source)

    # Year
    year <- ref_node %>% html_node(".year") %>% html_text(trim = TRUE)
    if (!is.null(year) && nzchar(year))
      xml_text(xml2::xml_add_child(ec, "year")) <- clean_text(year)

    # Volume
    volume <- ref_node %>% html_node(".volume") %>% html_text(trim = TRUE)
    if (!is.null(volume) && nzchar(volume))
      xml_text(xml2::xml_add_child(ec, "volume")) <- clean_text(volume)

    # Issue
    issue <- ref_node %>% html_node(".issue") %>% html_text(trim = TRUE)
    if (!is.null(issue) && nzchar(issue))
      xml_text(xml2::xml_add_child(ec, "issue")) <- clean_text(issue)

    # Pages
    fpage <- ref_node %>% html_node(".fpage") %>% html_text(trim = TRUE)
    lpage <- ref_node %>% html_node(".lpage") %>% html_text(trim = TRUE)
    if (!is.null(fpage) && nzchar(fpage))
      xml_text(xml2::xml_add_child(ec, "fpage")) <- clean_text(fpage)
    if (!is.null(lpage) && nzchar(lpage))
      xml_text(xml2::xml_add_child(ec, "lpage")) <- clean_text(lpage)

    # DOI
    doi_link <- ref_node %>%
      html_node("a.link-doi[href]") %>% html_attr("href")
    if (!is.null(doi_link) && nzchar(doi_link)) {
      doi_val <- sub("https://doi.org/", "", doi_link, fixed = TRUE)
      doi_val <- sub("http://dx.doi.org/", "", doi_val, fixed = TRUE)
      pid <- xml2::xml_add_child(ec, "pub-id", `pub-id-type` = "doi")
      xml_text(pid) <- doi_val
    }

    # PubMed ID
    pm_link <- ref_node %>%
      html_node("a.link-pub-id[href*='pubmed']") %>% html_attr("href")
    if (!is.null(pm_link) && nzchar(pm_link)) {
      pmid <- sub(".*pubmed/(\\d+).*", "\\1", pm_link)
      pid2 <- xml2::xml_add_child(ec, "pub-id", `pub-id-type` = "pmid")
      xml_text(pid2) <- pmid
    }
  }
}

# (NULL coalescing operator defined above)

# ----------------------------------------------------------------
# Main conversion function
# ----------------------------------------------------------------
convert_oxford_html <- function(html_file, xml_file = NULL) {

  if (!file.exists(html_file))
    stop("File not found: ", html_file)

  if (is.null(xml_file)) {
    xml_file <- sub("\\.html?$", ".xml", html_file, ignore.case = TRUE)
    if (xml_file == html_file) xml_file <- paste0(html_file, ".xml")
  }

  message("Converting: ", basename(html_file), " -> ", basename(xml_file))

  # Parse HTML
  page <- read_html(html_file, encoding = "UTF-8")

  # Collect metadata from multiple sources
  dl     <- extract_datalayer(page)



  #print(dl)

  print(dl[["type"]] %||% "research-article")


  schema <- extract_schema_org(page)
  meta   <- extract_meta(page)

  # Build XML document (JATS-like structure)
  doc  <- xml2::xml_new_document()
  root <- xml2::xml_add_child(doc,
                              "article",
                        xmlns             = "https://jats.nlm.nih.gov/ns/archiving/1.3/",
                        `xmlns:xlink`     = "http://www.w3.org/1999/xlink",
                        `article-type`    = "research-article",
                        `xml:lang`        = "en")

  print("\n \n \n Ok here 2")

  front <- xml2::xml_add_child(root, "front")

  print("\n \n \n Ok here 4")


  # Article metadata
  build_article_meta(front,
                     page,
                     dl,
                     schema,
                     meta)

  print("\n \n \n Ok here 5")

  # Body
  build_body(root, page)

  print("\n \n \n Ok here 3")

  error("Testing error handling")

  # Back / References
  build_back(root, page)

  # Write
  xml_write(doc, file = xml_file, encoding = "UTF-8")
  message("  Written: ", xml_file)

  invisible(xml_file)
}

# ----------------------------------------------------------------
# Batch conversion
# ----------------------------------------------------------------
batch_convert_oxford <- function(input_dir  = ".",
                                 output_dir = input_dir,
                                 pattern    = "\\.html?$") {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  files <- list.files(input_dir, pattern = pattern,
                      full.names = TRUE, ignore.case = TRUE)

  if (length(files) == 0) {
    message("No HTML files found in: ", input_dir)
    return(invisible(character(0)))
  }

  results <- character(length(files))
  for (i in seq_along(files)) {
    xml_name <- sub("\\.html?$", ".xml",
                    basename(files[i]), ignore.case = TRUE)
    out_path <- file.path(output_dir, xml_name)
    tryCatch(
      results[i] <- convert_oxford_html(files[i], out_path),
      error = function(e) {
        message("  ERROR on ", basename(files[i]), ": ", conditionMessage(e))
        # Print full call stack for debugging
        calls <- sys.calls()
        if (length(calls) > 0) {
          message("  Call stack:")
          for (cl in calls) {
            message("    ", deparse(cl)[1])
          }
        }
        results[i] <<- NA_character_
      }
    )
  }

  invisible(results)
}

# ----------------------------------------------------------------
# Command-line interface
# ----------------------------------------------------------------
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    cat("Usage:\n")
    cat("  Rscript oxford_html_to_xml.R <input.html> [output.xml]\n")
    cat("  Rscript oxford_html_to_xml.R <dir_with_html_files>\n")
    quit(status = 1)
  }

  if (length(args) == 1 && dir.exists(args[1])) {
    # Directory mode
    batch_convert_oxford(args[1])
  } else if (length(args) == 1) {
    # Single file, auto output name
    convert_oxford_html(args[1])
  } else {
    # Single file with explicit output
    convert_oxford_html(args[1], args[2])
  }
}
