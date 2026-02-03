#!/usr/bin/env Rscript
# Wiley XML to JATS XML Converter
# Converts Wiley journal article XML files to JATS XML 1.1 format

library(xml2)
library(stringr)

# helper function to safely check if a value is not empty
# has_content <- function(x) {
#   return(!is.null(x) && is.na(x) && length(x) > 0 && nchar(as.character(x)) > 0)
# }
has_content <- function(x) {
  !is.null(x) & !is.na(x) & nzchar(trimws(as.character(x)))
}

# Extract text safely from XML node
safe_text <- function(node, trim = TRUE) {
  if (length(node) == 0 || is.na(node)) {
    return("")
  }
  xml_text(node, trim = trim)
}

# Extract attribute safely from XML node
safe_attr <- function(node, attr) {
  if (length(node) == 0 || is.na(node)) {
    return(NULL)
  }
  xml_attr(node, attr)
}

# Parse Wiley authors
parse_wiley_authors <- function(doc, ns) {
  # Find creators in contentMeta
  creators <- xml_find_all(doc, "//d1:creator[@creatorRole='author']", ns)

  author_list <- list()

  for (creator in creators) {
    author_id <- xml_attr(creator, "xml:id")
    affiliation_ref <- xml_attr(creator, "affiliationRef")

    # Get person name
    given_names <- safe_text(xml_find_first(creator, ".//d1:givenNames", ns))
    family_name <- safe_text(xml_find_first(creator, ".//d1:familyName", ns))

    # Get ORCID if present
    orcid_node <- xml_find_first(creator, ".//d1:id[@type='orcid']", ns)
    orcid <- if (!is.na(orcid_node)) safe_attr(orcid_node, "value") else NULL

    # Check if corresponding author
    #corresponding <- grepl("corresponding", safe_attr(creator, "corresponding") %||% "")
    corresp_attr <- safe_attr(creator, "corresponding")
    corresponding <- grepl("corresponding", if (is.null(corresp_attr)) "" else corresp_attr)

    author_list[[length(author_list) + 1]] <- list(
      id = author_id,
      given = given_names,
      family = family_name,
      orcid = orcid,
      affiliation_ref = affiliation_ref,
      corresponding = corresponding
    )
  }

  return(author_list)
}

# Parse Wiley affiliations
parse_wiley_affiliations <- function(doc, ns) {
  affiliations <- xml_find_all(doc, "//d1:affiliation", ns)

  aff_list <- list()

  for (aff in affiliations) {
    aff_id <- xml_attr(aff, "xml:id")

    org_div <- safe_text(xml_find_first(aff, ".//d1:orgDiv", ns))
    org_name <- safe_text(xml_find_first(aff, ".//d1:orgName", ns))
    city <- safe_text(xml_find_first(aff, ".//d1:city", ns))
    country <- safe_text(xml_find_first(aff, ".//d1:country", ns))
    country_code <- safe_attr(aff, "countryCode")

    # Build affiliation text
    aff_parts <- c()
    if (has_content(org_div)) aff_parts <- c(aff_parts, org_div)
    if (has_content(org_name)) aff_parts <- c(aff_parts, org_name)
    if (has_content(city)) aff_parts <- c(aff_parts, city)
    if (has_content(country)) aff_parts <- c(aff_parts, country)

    aff_text <- paste(aff_parts, collapse = ", ")

    aff_list[[length(aff_list) + 1]] <- list(
      id = aff_id,
      text = aff_text,
      country_code = country_code
    )
  }

  return(aff_list)
}

# Parse Wiley abstract
parse_wiley_abstract <- function(doc, ns) {
  abstract_node <- xml_find_first(doc, "//d1:abstract", ns)
  if (is.na(abstract_node)) {
    return(NULL)
  }

  # Get all paragraphs
  paragraphs <- xml_find_all(abstract_node, ".//d1:p", ns)
  abstract_text <- paste(sapply(paragraphs, safe_text), collapse = "\n\n")

  return(abstract_text)
}

# Parse Wiley keywords
parse_wiley_keywords <- function(doc, ns) {
  keywords <- xml_find_all(doc, "//d1:keyword", ns)
  keyword_list <- sapply(keywords, safe_text)
  return(keyword_list[has_content(keyword_list)])
}

# Parse Wiley body sections
parse_wiley_body <- function(doc, ns) {
  # Find body sections
  body_node <- xml_find_first(doc, "//d1:body", ns)
  if (is.na(body_node)) {
    return(list())
  }

  sections <- xml_find_all(body_node, "./d1:section", ns)

  section_list <- list()

  for (section in sections) {
    section_id <- xml_attr(section, "xml:id")

    # Get section title
    title_node <- xml_find_first(section, "./d1:title", ns)
    title <- if (!is.na(title_node)) safe_text(title_node) else ""

    # Get paragraphs
    paragraphs <- xml_find_all(section, ".//d1:p", ns)
    para_texts <- sapply(paragraphs, function(p) {
      # Get text content, preserving some structure
      safe_text(p, trim = TRUE)
    })
    para_texts <- para_texts[has_content(para_texts)]

    section_list[[length(section_list) + 1]] <- list(
      id = section_id,
      title = title,
      paragraphs = para_texts
    )
  }

  return(section_list)
}

# Parse Wiley references
parse_wiley_references <- function(doc, ns) {
  refs <- xml_find_all(doc, "//d1:bib", ns)

  ref_list <- list()

  for (ref in refs) {
    ref_id <- xml_attr(ref, "xml:id")

    # Get citation node
    citation <- xml_find_first(ref, "./d1:citation", ns)
    if (is.na(citation)) next

    citation_type <- xml_attr(citation, "type")

    # Extract citation components
    authors_nodes <- xml_find_all(citation, ".//d1:author", ns)
    authors <- sapply(authors_nodes, function(a) {
      given <- safe_text(xml_find_first(a, ".//d1:givenNames", ns))
      family <- safe_text(xml_find_first(a, ".//d1:familyName", ns))
      if (has_content(given) && has_content(family)) {
        paste(family, given, sep = " ")
      } else if (has_content(family)) {
        family
      } else {
        ""
      }
    })
    authors <- authors[has_content(authors)]

    article_title <- safe_text(xml_find_first(citation, ".//d1:articleTitle", ns))
    journal_title <- safe_text(xml_find_first(citation, ".//d1:journalTitle", ns))
    pub_year <- safe_text(xml_find_first(citation, ".//d1:pubYear", ns))
    volume <- safe_text(xml_find_first(citation, ".//d1:vol", ns))
    page_first <- safe_text(xml_find_first(citation, ".//d1:pageFirst", ns))
    page_last <- safe_text(xml_find_first(citation, ".//d1:pageLast", ns))

    ref_list[[length(ref_list) + 1]] <- list(
      id = ref_id,
      type = citation_type,
      authors = authors,
      title = article_title,
      journal = journal_title,
      year = pub_year,
      volume = volume,
      pages = if (has_content(page_first)) paste(page_first, page_last, sep = "-") else ""
    )
  }

  return(ref_list)
}

# Convert Wiley XML to JATS XML
convert_wiley_to_jats <- function(wiley_file, output_file) {
  cat(sprintf("Reading Wiley XML file: %s\n", wiley_file))

  # Read and parse Wiley XML
  doc <- read_xml(wiley_file)

  # Define namespaces
  ns <- c(d1 = "http://www.wiley.com/namespaces/wiley")

  # Extract metadata from publicationMeta
  doi_node <- xml_find_first(doc, "//d1:publicationMeta[@level='unit']/d1:doi", ns)
  doi <- safe_text(doi_node)

  # Get title
  title_node <- xml_find_first(doc, "//d1:contentMeta/d1:titleGroup/d1:title[@type='main']", ns)
  title <- safe_text(title_node)

  # Get journal information
  journal_title_node <- xml_find_first(doc, "//d1:publicationMeta[@level='product']/d1:titleGroup/d1:title[@type='main']", ns)
  journal_title <- safe_text(journal_title_node)

  # Get ISSN
  issn_print_node <- xml_find_first(doc, "//d1:publicationMeta[@level='product']/d1:issn[@type='print']", ns)
  issn_print <- safe_text(issn_print_node)

  issn_electronic_node <- xml_find_first(doc, "//d1:publicationMeta[@level='product']/d1:issn[@type='electronic']", ns)
  issn_electronic <- safe_text(issn_electronic_node)

  # Get publisher
  publisher_node <- xml_find_first(doc, "//d1:publisherName", ns)
  publisher <- safe_text(publisher_node)

  # Get volume and issue
  volume_node <- xml_find_first(doc, "//d1:publicationMeta[@level='part']//d1:numbering[@type='journalVolume']", ns)
  volume <- safe_attr(volume_node, "number")

  issue_node <- xml_find_first(doc, "//d1:publicationMeta[@level='part']//d1:numbering[@type='journalIssue']", ns)
  issue <- safe_text(issue_node)

  # Get page numbers
  page_first_node <- xml_find_first(doc, "//d1:publicationMeta[@level='unit']//d1:numbering[@type='pageFirst']", ns)
  page_first <- safe_text(page_first_node)

  page_last_node <- xml_find_first(doc, "//d1:publicationMeta[@level='unit']//d1:numbering[@type='pageLast']", ns)
  page_last <- safe_text(page_last_node)

  # Get publication date
  cover_date_node <- xml_find_first(doc, "//d1:publicationMeta[@level='part']/d1:coverDate", ns)
  cover_date <- safe_attr(cover_date_node, "startDate")

  # Parse date components
  year <- month <- day <- NULL
  if (!is.null(cover_date) && has_content(cover_date)) {
    date_parts <- strsplit(cover_date, "-")[[1]]
    if (length(date_parts) >= 1) year <- date_parts[1]
    if (length(date_parts) >= 2) month <- date_parts[2]
    if (length(date_parts) >= 3) day <- date_parts[3]
  }

  # Get copyright
  copyright_node <- xml_find_first(doc, "//d1:publicationMeta[@level='unit']/d1:copyright", ns)
  copyright_statement <- safe_text(copyright_node)

  # Parse authors and affiliations
  authors <- parse_wiley_authors(doc, ns)
  affiliations <- parse_wiley_affiliations(doc, ns)

  # Parse abstract
  abstract_text <- parse_wiley_abstract(doc, ns)

  # Parse keywords
  keywords <- parse_wiley_keywords(doc, ns)

  # Parse body sections
  body_sections <- parse_wiley_body(doc, ns)

  # Parse references
  references <- parse_wiley_references(doc, ns)

  cat(sprintf("Extracted: DOI=%s, Title=%s..., Authors=%d, Sections=%d, References=%d\n",
              ifelse(has_content(doi), doi, "N/A"),
              substr(title, 1, 50),
              length(authors),
              length(body_sections),
              length(references)))

  # Create JATS XML structure
  jats_doc <- xml_new_root("article",
                           "article-type" = "research-article",
                           "dtd-version" = "1.1",
                           "xml:lang" = "en",
                           "xmlns:xlink" = "http://www.w3.org/1999/xlink",
                           "xmlns:mml" = "http://www.w3.org/1998/Math/MathML")

  # Front matter
  front <- xml_add_child(jats_doc, "front")

  # Journal metadata
  journal_meta <- xml_add_child(front, "journal-meta")

  if (has_content(journal_title)) {
    journal_title_group <- xml_add_child(journal_meta, "journal-title-group")
    xml_add_child(journal_title_group, "journal-title", journal_title)
  }

  if (has_content(issn_print)) {
    xml_add_child(journal_meta, "issn", issn_print, "pub-type" = "ppub")
  }

  if (has_content(issn_electronic)) {
    xml_add_child(journal_meta, "issn", issn_electronic, "pub-type" = "epub")
  }

  if (has_content(publisher)) {
    publisher_node <- xml_add_child(journal_meta, "publisher")
    xml_add_child(publisher_node, "publisher-name", publisher)
  }

  # Article metadata
  article_meta <- xml_add_child(front, "article-meta")

  # DOI
  if (has_content(doi)) {
    xml_add_child(article_meta, "article-id", doi, "pub-id-type" = "doi")
  }

  # Title
  if (has_content(title)) {
    title_group <- xml_add_child(article_meta, "title-group")
    xml_add_child(title_group, "article-title", title)
  }

  # Authors
  if (length(authors) > 0) {
    contrib_group <- xml_add_child(article_meta, "contrib-group")

    for (author in authors) {
      contrib_attrs <- list("contrib-type" = "author")
      if (author$corresponding) {
        contrib_attrs$"corresp" <- "yes"
      }

      contrib <- do.call(xml_add_child, c(list(.x = contrib_group, .value = "contrib"), contrib_attrs))

      if (!is.null(author$orcid) && has_content(author$orcid)) {
        xml_add_child(contrib, "contrib-id", author$orcid, "contrib-id-type" = "orcid")
      }

      name <- xml_add_child(contrib, "name")
      if (has_content(author$family)) {
        xml_add_child(name, "surname", author$family)
      }
      if (has_content(author$given)) {
        xml_add_child(name, "given-names", author$given)
      }

      # Add affiliation references
      if (!is.null(author$affiliation_ref) && has_content(author$affiliation_ref)) {
        aff_refs <- strsplit(author$affiliation_ref, "\\s+")[[1]]
        for (aff_ref in aff_refs) {
          aff_ref_clean <- gsub("^#", "", aff_ref)
          xml_add_child(contrib, "xref", "", "ref-type" = "aff", "rid" = aff_ref_clean)
        }
      }
    }
  }

  # Affiliations
  if (length(affiliations) > 0) {
    for (aff in affiliations) {
      aff_node <- xml_add_child(article_meta, "aff", aff$text, "id" = aff$id)

      if (!is.null(aff$country_code) && has_content(aff$country_code)) {
        xml_set_attr(aff_node, "country", aff$country_code)
      }
    }
  }

  # Publication date
  if (!is.null(year)) {
    pub_date_node <- xml_add_child(article_meta, "pub-date", "pub-type" = "epub")
    if (!is.null(day)) xml_add_child(pub_date_node, "day", day)
    if (!is.null(month)) xml_add_child(pub_date_node, "month", month)
    xml_add_child(pub_date_node, "year", year)
  }

  # Volume and issue
  if (!is.null(volume) && has_content(volume)) {
    xml_add_child(article_meta, "volume", volume)
  }
  if (!is.null(issue) && has_content(issue)) {
    xml_add_child(article_meta, "issue", issue)
  }

  # Page numbers
  if (has_content(page_first)) {
    xml_add_child(article_meta, "fpage", page_first)
  }
  if (has_content(page_last)) {
    xml_add_child(article_meta, "lpage", page_last)
  }

  # Copyright
  if (has_content(copyright_statement)) {
    permissions <- xml_add_child(article_meta, "permissions")
    xml_add_child(permissions, "copyright-statement", copyright_statement)
  }

  # Abstract
  if (!is.null(abstract_text) && has_content(abstract_text)) {
    abstract_node <- xml_add_child(article_meta, "abstract")
    # Split by double newlines to preserve paragraphs
    paras <- strsplit(abstract_text, "\n\n")[[1]]
    for (para in paras) {
      if (has_content(trimws(para))) {
        xml_add_child(abstract_node, "p", trimws(para))
      }
    }
  }

  # Keywords
  if (length(keywords) > 0) {
    kwd_group <- xml_add_child(article_meta, "kwd-group")
    for (kw in keywords) {
      xml_add_child(kwd_group, "kwd", kw)
    }
  }

  # Body content
  if (length(body_sections) > 0) {
    body <- xml_add_child(jats_doc, "body")

    for (section in body_sections) {
      sec_attrs <- list()
      if (!is.null(section$id) && has_content(section$id)) {
        sec_attrs$id <- section$id
      }

      sec <- do.call(xml_add_child, c(list(.x = body, .value = "sec"), sec_attrs))

      if (has_content(section$title)) {
        xml_add_child(sec, "title", section$title)
      }

      for (para_text in section$paragraphs) {
        xml_add_child(sec, "p", para_text)
      }
    }
  }

  # Back matter (references)
  if (length(references) > 0) {
    back <- xml_add_child(jats_doc, "back")
    ref_list <- xml_add_child(back, "ref-list")

    for (ref in references) {
      ref_node <- xml_add_child(ref_list, "ref", "id" = ref$id)

      element_citation <- xml_add_child(ref_node, "element-citation")
      if (!is.null(ref$type) && has_content(ref$type)) {
        xml_set_attr(element_citation, "publication-type", ref$type)
      }

      # Authors
      if (length(ref$authors) > 0) {
        person_group <- xml_add_child(element_citation, "person-group", "person-group-type" = "author")
        for (author_name in ref$authors) {
          xml_add_child(person_group, "name")
          # Note: Could parse author_name further if needed
        }
      }

      if (has_content(ref$title)) {
        xml_add_child(element_citation, "article-title", ref$title)
      }

      if (has_content(ref$journal)) {
        xml_add_child(element_citation, "source", ref$journal)
      }

      if (has_content(ref$year)) {
        xml_add_child(element_citation, "year", ref$year)
      }

      if (has_content(ref$volume)) {
        xml_add_child(element_citation, "volume", ref$volume)
      }

      if (has_content(ref$pages)) {
        pages <- strsplit(ref$pages, "-")[[1]]
        if (length(pages) > 0) xml_add_child(element_citation, "fpage", pages[1])
        if (length(pages) > 1) xml_add_child(element_citation, "lpage", pages[2])
      }
    }
  }

  # Write to file
  cat(sprintf("Writing JATS XML file: %s\n", output_file))
  write_xml(jats_doc, output_file, encoding = "UTF-8")

  cat("Conversion complete!\n")

  return(invisible(jats_doc))
}

# Main function
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 2) {
    cat("Usage: Rscript wiley_to_jats.R <input_wiley.xml> <output_jats.xml>\n")
    cat("Example: Rscript wiley_to_jats.R article_wiley.xml article_jats.xml\n")
    quit(status = 1)
  }

  input_file <- args[1]
  output_file <- args[2]

  if (!file.exists(input_file)) {
    cat(sprintf("Error: Input file '%s' not found\n", input_file))
    quit(status = 1)
  }

  tryCatch({
    convert_wiley_to_jats(input_file, output_file)
    cat(sprintf("\nSuccess! JATS XML file created: %s\n", output_file))
  }, error = function(e) {
    cat(sprintf("Error during conversion: %s\n", e$message))
    traceback()
    quit(status = 1)
  })
}

# Run main if script is executed directly
if (sys.nframe() == 0) {
  main()
}
