#!/usr/bin/env Rscript
# Elsevier XML to JATS XML Converter (local-name version)
# Converts Elsevier journal article XML files to JATS XML 1.1 format
# Uses local-name() to handle complex namespace structures

library(xml2)
library(stringr)

# Helper function for local-name XPath
# Basic: selects any descendant whose local name matches x.
use_local <- function(x) {
  paste0(".//*[local-name()='", x, "']")
}

# Variant: descendant whose local name matches x AND has a required attribute.
# e.g. use_local_attr("cross-ref", "refid") -> .//*[local-name()='cross-ref'][@refid]
use_local_attr <- function(x, attr) {
  paste0(".//*[local-name()='", x, "'][@", attr, "]")
}

# Variant: direct child (single step, not deep) whose local name matches x.
# e.g. use_local_child("section") -> ./*[local-name()='section']
use_local_child <- function(x) {
  paste0("./*[local-name()='", x, "']")
}

# Variant: OR of multiple element names, matched at any depth.
# e.g. use_local_or(c("simple-para", "para")) ->
#   .//*[local-name()='simple-para' or local-name()='para']
use_local_or <- function(xs) {
  predicates <- paste0("local-name()='", xs, "'", collapse = " or ")
  paste0(".//*[", predicates, "]")
}

# Variant: chained path of element names, each matched by local-name.
# Every step is a direct child of the previous.
# e.g. use_local_path(c("contribution", "title", "maintitle")) ->
#   .//*[local-name()='contribution']/*[local-name()='title']/*[local-name()='maintitle']
use_local_path <- function(xs) {
  paste0(
    ".//*[local-name()='", xs[1], "']",
    if (length(xs) > 1)
      paste0("/*[local-name()='", xs[-1], "']", collapse = "")
    else ""
  )
}

# Variant: deep path — every step uses a descendant axis (//*).
# Useful when intermediate nodes may appear at varying depths.
# e.g. use_local_deep_path(c("host", "title", "maintitle")) ->
#   .//*[local-name()='host']//*[local-name()='title']//*[local-name()='maintitle']
use_local_deep_path <- function(xs) {
  paste0(".//*[local-name()='",
         paste0(xs, collapse = "']//*[local-name()='"),
         "']")
}

# helper function to safely check if a value is not empty
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

# Parse Elsevier authors
parse_elsevier_authors <- function(doc) {
  author_list <- list()

  for (auth in xml_find_all(doc, use_local("ce:author"))) {
    author_id <- xml_attr(auth, "ce:author id")
    orcid <- xml_attr(auth, "ce:orcid")

    # Get person name
    given <- safe_text(xml_find_first(auth, use_local("ce:given-name")))
    family <- safe_text(xml_find_first(auth, use_local("ce:surname")))

    # Get affiliation references from cross-refs
    cross_refs <- xml_find_all(auth, use_local_attr("ce:cross-ref", "ce:refid"))
    affiliation_refs <- sapply(cross_refs, function(x) xml_attr(x, "ce:refid"))

    # Check if corresponding author
    corresponding <- FALSE
    corresp_attr <- xml_attr(auth, "corresponding")
    if (!is.null(corresp_attr) && has_content(corresp_attr)) {
      corresponding <- grepl("yes|true", corresp_attr, ignore.case = TRUE)
    }

    author_list[[length(author_list) + 1]] <- list(
      id = author_id,
      given = given,
      surname = family,
      orcid = orcid,
      affiliation_refs = affiliation_refs,
      corresponding = corresponding
    )
  }

  return(author_list)
}

# Parse Elsevier affiliations
parse_elsevier_affiliations <- function(doc) {
  aff_list <- list()

  for (aff in xml_find_all(doc, use_local("affiliation"))) {
    aff_id <- xml_attr(aff, "id")

    # Get text label if present
    label <- safe_text(xml_find_first(aff, use_local("label")))

    # Get text content
    textfn <- safe_text(xml_find_first(aff, use_local("textfn")))

    # If textfn is empty, try getting all text
    if (!has_content(textfn)) {
      textfn <- safe_text(aff)
    }

    # Get country if present
    country <- safe_text(xml_find_first(aff, use_local("country")))

    aff_list[[length(aff_list) + 1]] <- list(
      id = aff_id,
      label = label,
      text = textfn,
      country = country
    )
  }

  return(aff_list)
}

# Parse Elsevier abstract
parse_elsevier_abstract <- function(doc) {
  # Get the author abstract (not graphical)
  abstract_nodes <- xml_find_all(doc, use_local("ce:abstract"))

  # Try to find author abstract
  abstract_node <- NULL
  for (node in abstract_nodes) {
    class_attr <- xml_attr(node, "class")
    if (!is.null(class_attr) && class_attr == "author") {
      abstract_node <- node
      break
    }
  }

  # If no author abstract, use first abstract
  if (is.null(abstract_node) && length(abstract_nodes) > 0) {
    abstract_node <- abstract_nodes[[1]]
  }

  if (is.null(abstract_node)) {
    return(NULL)
  }

  # Get all abstract sections
  abstract_secs <- xml_find_all(abstract_node, use_local("ce:abstract-sec"))
  abstract_parts <- list()

  if (length(abstract_secs) == 0) {
    # No sections, just get paragraphs directly
    paragraphs <- xml_find_all(abstract_node, use_local_or(c("ce:simple-para", "ce:para")))
    para_texts <- sapply(paragraphs, safe_text)
    para_texts <- para_texts[has_content(para_texts)]

    if (length(para_texts) > 0) {
      abstract_parts[[1]] <- list(
        role = NULL,
        title = NULL,
        paragraphs = para_texts
      )
    }
  } else {
    for (sec in abstract_secs) {
      role <- xml_attr(sec, "role")

      # Get section title if present
      title <- safe_text(xml_find_first(sec, use_local("ce:section-title")))

      # Get all paragraphs
      paragraphs <- xml_find_all(sec, use_local_or(c("ce:simple-para", "ce:para")))
      para_texts <- sapply(paragraphs, safe_text)
      para_texts <- para_texts[has_content(para_texts)]

      if (length(para_texts) > 0) {
        abstract_parts[[length(abstract_parts) + 1]] <- list(
          role = role,
          title = title,
          paragraphs = para_texts
        )
      }
    }
  }

  return(abstract_parts)
}

# Parse Elsevier keywords
parse_elsevier_keywords <- function(doc) {
  # Keywords are typically in keyword/text
  keywords <- xml_find_all(doc, use_local_path(c("ce:keyword", "ce:text")))
  keyword_list <- sapply(keywords, safe_text)
  return(keyword_list[has_content(keyword_list)])
}

# Parse Elsevier body sections
parse_elsevier_body <- function(doc) {
  # Find body sections - look for sections element, then section children
  sections_node <- xml_find_first(doc, use_local("ce:sections"))

  if (is.na(sections_node)) {
    return(list())
  }

  # Get direct section children
  sections <- xml_find_all(sections_node, use_local_child("ce:section"))

  section_list <- list()

  for (section in sections) {
    section_id <- xml_attr(section, "id")
    role <- xml_attr(section, "role")

    # Get section title
    title_node <- xml_find_first(section, use_local_child("ce:section-title"))
    title <- if (!is.na(title_node)) safe_text(title_node) else ""

    # Get paragraphs (direct and nested)
    paragraphs <- xml_find_all(section, use_local("ce:para"))
    para_texts <- sapply(paragraphs, function(p) {
      safe_text(p, trim = TRUE)
    })
    para_texts <- para_texts[has_content(para_texts)]

    section_list[[length(section_list) + 1]] <- list(
      id = section_id,
      role = role,
      title = title,
      paragraphs = para_texts
    )
  }

  return(section_list)
}

# Parse Elsevier references
parse_elsevier_references <- function(doc) {
  refs <- xml_find_all(doc, use_local("ce:bib-reference"))

  ref_list <- list()

  for (ref in refs) {
    ref_id <- xml_attr(ref, "id")

    # Get label
    label <- safe_text(xml_find_first(ref, use_local_child("ce:label")))

    # Get structured reference
    sb_ref <- xml_find_first(ref, use_local_child("sb:reference"))

    if (!is.na(sb_ref)) {
      # Extract authors
      authors_nodes <- xml_find_all(sb_ref, use_local("sb:author"))
      authors <- sapply(authors_nodes, function(a) {
        given <- safe_text(xml_find_first(a, use_local("ce:given-name")))
        surname <- safe_text(xml_find_first(a, use_local("ce:surname")))
        if (has_content(given) && has_content(surname)) {
          paste(surname, given, sep = " ")
        } else if (has_content(surname)) {
          surname
        } else {
          ""
        }
      })
      authors <- authors[has_content(authors)]

      # Check for et al
      et_al <- length(xml_find_all(sb_ref, use_local("et-al"))) > 0

      # Get title
      article_title <- safe_text(xml_find_first(sb_ref, use_local_path(c("sb:contribution", "sb:title", "sb:maintitle"))))

      # Get journal/source
      journal_title <- safe_text(xml_find_first(sb_ref, use_local_deep_path(c("sb:host", "sb:title", "sb:maintitle"))))

      # Get year
      pub_year <- safe_text(xml_find_first(sb_ref, use_local("sb:date")))

      # Get volume
      volume <- safe_text(xml_find_first(sb_ref, use_local("sb:volume-nr")))

      # Get pages
      page_first <- safe_text(xml_find_first(sb_ref, use_local("sb:first-page")))
      page_last <- safe_text(xml_find_first(sb_ref, use_local("sb:last-page")))

      ref_list[[length(ref_list) + 1]] <- list(
        id = ref_id,
        label = label,
        authors = authors,
        et_al = et_al,
        title = article_title,
        journal = journal_title,
        year = pub_year,
        volume = volume,
        pages = if (has_content(page_first)) paste(page_first, page_last, sep = "-") else ""
      )
    } else {
      # Fallback to source text
      source_text <- safe_text(xml_find_first(ref, use_local("ce:source-text")))
      ref_list[[length(ref_list) + 1]] <- list(
        id = ref_id,
        label = label,
        source_text = source_text
      )
    }
  }

  return(ref_list)
}

# Convert Elsevier XML to JATS XML
convert_elsevier_to_jats <- function(elsevier_file, output_file) {
  cat(sprintf("Reading Elsevier XML file: %s\n", elsevier_file))

  # Read and parse Elsevier XML
  doc <- read_xml(elsevier_file)
  doc <- xml_find_first(doc, use_local("xocs:doc"))

  # Extract metadata using local-name
  meta <- xml_find_first(doc, use_local("xocs:meta"))

  doi <- safe_text(xml_find_first(meta, use_local("xocs:doi")))
  pii <- safe_text(xml_find_first(doc, use_local("xocs:pii-formatted")))

  # Get journal information
  journal_title <- safe_text(xml_find_first(meta, use_local("xocs:srctitle")))

  # Get ISSN
  issn_primary <- safe_text(xml_find_first(meta, use_local("xocs:issn-primary-formatted")))

  # Get publisher (typically Elsevier)
  publisher <- "Elsevier"

  # Get volume and issue
  volume <- safe_text(xml_find_first(meta, use_local("xocs:vol-first")))
  issue <- safe_text(xml_find_first(meta, use_local("xocs:iss-first")))

  # Get page numbers
  article_number <- safe_text(xml_find_first(meta, use_local("xocs:article-number")))
  page_first <- safe_text(xml_find_first(meta, use_local("xocs:first-fp")))
  page_last <- safe_text(xml_find_first(meta, use_local("xocs:last-lp")))

  # Alternative page numbers if first-fp not found
  if (!has_content(page_first)) {
    page_first <- safe_text(xml_find_first(meta, use_local("xocs:first-page")))
  }
  if (!has_content(page_last)) {
    page_last <- safe_text(xml_find_first(meta, use_local("xocs:last-page")))
  }

  # Get publication date
  cover_date <- safe_text(xml_find_first(meta, use_local("xocs:cover-date-start")))
  year <- safe_text(xml_find_first(meta, use_local("xocs:cover-date-year")))

  month <- NULL
  day <- NULL

  if (has_content(cover_date)) {
    date_parts <- strsplit(cover_date, "-")[[1]]
    if (length(date_parts) >= 2) month <- date_parts[2]
    if (length(date_parts) >= 3) day <- date_parts[3]
  }

  # Get copyright
  copyright_statement <- safe_text(xml_find_first(meta, use_local("xocs:copyright-line")))

  # Parse content
  content <- xml_find_first(doc, use_local("xocs:serial-item"))

  # Get title
  title <- safe_text(xml_find_first(content, use_local("ce:title")))

  authors <- parse_elsevier_authors(content) # authors
  affiliations <- parse_elsevier_affiliations(content)# author affiliations

  abstract_parts <- parse_elsevier_abstract(content) # abstract sections

  keywords <- parse_elsevier_keywords(content) # keywords

  body_sections <- parse_elsevier_body(content) # body sections
  references <- parse_elsevier_references(content) # references

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

  if (has_content(issn_primary)) {
    xml_add_child(journal_meta, "issn", issn_primary, "pub-type" = "ppub")
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

  # PII
  if (has_content(pii)) {
    xml_add_child(article_meta, "article-id", pii, "pub-id-type" = "pii")
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
      if (has_content(author$surname)) {
        xml_add_child(name, "surname", author$surname)
      }
      if (has_content(author$given)) {
        xml_add_child(name, "given-names", author$given)
      }

      # Add affiliation references
      if (!is.null(author$affiliation_refs) && length(author$affiliation_refs) > 0) {
        for (aff_ref in author$affiliation_refs) {
          xml_add_child(contrib, "xref", "", "ref-type" = "aff", "rid" = aff_ref)
        }
      }
    }
  }

  # Affiliations
  if (length(affiliations) > 0) {
    for (aff in affiliations) {
      # Build affiliation node
      if (!is.null(aff$id) && has_content(aff$id)) {
        aff_node <- xml_add_child(article_meta, "aff", "id" = aff$id)
      } else {
        aff_node <- xml_add_child(article_meta, "aff")
      }

      # Add label if present
      if (!is.null(aff$label) && has_content(aff$label)) {
        xml_add_child(aff_node, "label", aff$label)
      }

      # Add text content
      if (!is.null(aff$text) && has_content(aff$text)) {
        # Add as text node after any label
        xml_add_child(aff_node, "text", aff$text)
      }

      # Add country attribute
      if (!is.null(aff$country) && has_content(aff$country)) {
        xml_set_attr(aff_node, "country", aff$country)
      }
    }
  }

  # Publication date
  if (has_content(year)) {
    pub_date_node <- xml_add_child(article_meta, "pub-date", "pub-type" = "epub")
    if (!is.null(day) && has_content(day)) xml_add_child(pub_date_node, "day", day)
    if (!is.null(month) && has_content(month)) xml_add_child(pub_date_node, "month", month)
    xml_add_child(pub_date_node, "year", year)
  }

  # Volume and issue
  if (has_content(volume)) {
    xml_add_child(article_meta, "volume", volume)
  }
  if (has_content(issue)) {
    xml_add_child(article_meta, "issue", issue)
  }

  # Page numbers or article number
  if (has_content(page_first)) {
    xml_add_child(article_meta, "fpage", page_first)
  }
  if (has_content(page_last)) {
    xml_add_child(article_meta, "lpage", page_last)
  }
  if (has_content(article_number) && !has_content(page_first)) {
    xml_add_child(article_meta, "elocation-id", article_number)
  }

  # Copyright
  if (has_content(copyright_statement)) {
    permissions <- xml_add_child(article_meta, "permissions")
    xml_add_child(permissions, "copyright-statement", copyright_statement)
  }

  # Abstract
  if (!is.null(abstract_parts) && length(abstract_parts) > 0) {
    abstract_node <- xml_add_child(article_meta, "abstract")

    for (abs_part in abstract_parts) {
      if (!is.null(abs_part$title) && has_content(abs_part$title)) {
        xml_add_child(abstract_node, "title", abs_part$title)
      }

      for (para in abs_part$paragraphs) {
        if (has_content(trimws(para))) {
          xml_add_child(abstract_node, "p", trimws(para))
        }
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
      if (!is.null(section$role) && has_content(section$role)) {
        sec_attrs$"sec-type" <- section$role
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

      # Add label if present
      if (!is.null(ref$label) && has_content(ref$label)) {
        xml_add_child(ref_node, "label", ref$label)
      }

      # Check if structured reference exists
      if (!is.null(ref$authors) && length(ref$authors) > 0) {
        element_citation <- xml_add_child(ref_node, "element-citation", "publication-type" = "journal")

        # Authors
        person_group <- xml_add_child(element_citation, "person-group", "person-group-type" = "author")
        for (author_name in ref$authors) {
          name_node <- xml_add_child(person_group, "name")
          # Note: author_name is already formatted as "Surname Given"
          xml_add_child(name_node, "surname", author_name)
        }

        if (!is.null(ref$et_al) && ref$et_al) {
          xml_add_child(person_group, "etal")
        }

        if (!is.null(ref$title) && has_content(ref$title)) {
          xml_add_child(element_citation, "article-title", ref$title)
        }

        if (!is.null(ref$journal) && has_content(ref$journal)) {
          xml_add_child(element_citation, "source", ref$journal)
        }

        if (!is.null(ref$year) && has_content(ref$year)) {
          xml_add_child(element_citation, "year", ref$year)
        }

        if (!is.null(ref$volume) && has_content(ref$volume)) {
          xml_add_child(element_citation, "volume", ref$volume)
        }

        if (!is.null(ref$pages) && has_content(ref$pages)) {
          pages <- strsplit(ref$pages, "-")[[1]]
          if (length(pages) > 0 && has_content(pages[1])) {
            xml_add_child(element_citation, "fpage", pages[1])
          }
          if (length(pages) > 1 && has_content(pages[2])) {
            xml_add_child(element_citation, "lpage", pages[2])
          }
        }
      } else if (!is.null(ref$source_text) && has_content(ref$source_text)) {
        # Use mixed-citation for unstructured references
        xml_add_child(ref_node, "mixed-citation", ref$source_text)
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
    cat("Usage: Rscript elsevier_to_jats.R <input_elsevier.xml> <output_jats.xml>\n")
    cat("Example: Rscript elsevier_to_jats.R article_elsevier.xml article_jats.xml\n")
    quit(status = 1)
  }

  input_file <- args[1]
  output_file <- args[2]

  if (!file.exists(input_file)) {
    cat(sprintf("Error: Input file '%s' not found\n", input_file))
    quit(status = 1)
  }

  tryCatch({
    convert_elsevier_to_jats(input_file, output_file)
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
