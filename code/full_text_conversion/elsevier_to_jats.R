#!/usr/bin/env Rscript
# Elsevier XML to JATS Converter - strips namespace prefixes to work around malformed XML

library(xml2)

has_content <- function(x) {
  !is.null(x) && length(x) > 0 && !all(is.na(x)) && any(nzchar(trimws(as.character(x))))
}

safe_text <- function(node) {
  if (length(node) == 0 || all(is.na(node))) return("")
  txt <- xml_text(node, trim = TRUE)
  if (is.na(txt)) return("")
  txt
}

convert_elsevier_to_jats <- function(input_file, output_file) {
  
  cat(sprintf("Reading: %s\n", input_file))
  
  # Read and fix XML by removing namespace prefixes
  lines <- readLines(input_file, warn = FALSE)
  xml_str <- paste(lines, collapse = "\n")
  
  # Strip ce: and xocs: prefixes (workaround for missing namespace declarations)
  xml_str <- gsub("<ce:", "<", xml_str)
  xml_str <- gsub("</ce:", "</", xml_str)
  xml_str <- gsub("<xocs:", "<xocs_", xml_str)
  xml_str <- gsub("</xocs:", "</xocs_", xml_str)
  
  doc <- read_xml(xml_str)
  
  # Extract metadata
  doi <- safe_text(xml_find_first(doc, "//xocs_doi | //doi"))
  title <- safe_text(xml_find_first(doc, "//head/title | //title[1]"))
  journal <- safe_text(xml_find_first(doc, "//xocs_srctitle"))
  volume <- safe_text(xml_find_first(doc, "//xocs_vol-first"))
  issue <- safe_text(xml_find_first(doc, "//xocs_iss-first"))
  fpage <- safe_text(xml_find_first(doc, "//xocs_first-page"))
  lpage <- safe_text(xml_find_first(doc, "//xocs_last-page"))
  pub_year <- safe_text(xml_find_first(doc, "//xocs_cover-date-year"))
  copyright_text <- safe_text(xml_find_first(doc, "//xocs_copyright-line | //copyright"))
  issn <- safe_text(xml_find_first(doc, "//xocs_issn-primary-formatted"))
  
  # Authors
  authors <- list()
  for (auth in xml_find_all(doc, "//author")) {
    given <- safe_text(xml_find_first(auth, ".//given-name"))
    family <- safe_text(xml_find_first(auth, ".//surname"))
    
    aff_rids <- character(0)
    for (xref in xml_find_all(auth, ".//cross-ref[@refid]")) {
      rid <- xml_attr(xref, "refid")
      if (has_content(rid) && grepl("^aff", rid)) aff_rids <- c(aff_rids, rid)
    }
    
    if (has_content(family)) {
      authors[[length(authors) + 1]] <- list(given = given, family = family, aff_rids = unique(aff_rids))
    }
  }
  
  # Affiliations
  affs <- list()
  for (aff in xml_find_all(doc, "//affiliation")) {
    aff_id <- xml_attr(aff, "id")
    text_content <- safe_text(aff)
    if (has_content(text_content)) {
      affs[[length(affs) + 1]] <- list(id = aff_id, text = text_content)
    }
  }
  
  # Abstract
  abstract <- character(0)
  abs_node <- xml_find_first(doc, "//abstract | //abstract-sec")
  if (!is.na(abs_node)) {
    para_nodes <- xml_find_all(abs_node, ".//simple-para | .//para")
    if (length(para_nodes) > 0) {
      abstract <- vapply(para_nodes, safe_text, character(1))
      abstract <- abstract[has_content(abstract)]
    }
  }
  
  # Keywords
  keywords <- character(0)
  kw_nodes <- xml_find_all(doc, "//keyword")
  if (length(kw_nodes) > 0) {
    keywords <- vapply(kw_nodes, safe_text, character(1))
    keywords <- keywords[has_content(keywords)]
  }
  
  # Body sections
  body_secs <- list()
  for (sec in xml_find_all(doc, "//section")) {
    sec_id <- xml_attr(sec, "id")
    sec_title <- safe_text(xml_find_first(sec, "./section-title"))
    
    para_nodes <- xml_find_all(sec, "./para | ./simple-para")
    paras <- character(0)
    if (length(para_nodes) > 0) {
      paras <- vapply(para_nodes, safe_text, character(1))
      paras <- paras[has_content(paras)]
    }
    
    if (has_content(sec_title) || length(paras) > 0) {
      body_secs[[length(body_secs) + 1]] <- list(id = sec_id, title = sec_title, paragraphs = paras)
    }
  }
  
  # References
  refs <- list()
  for (ref in xml_find_all(doc, "//bib-reference")) {
    ref_id <- xml_attr(ref, "id")
    ref_text <- safe_text(ref)
    if (has_content(ref_text)) {
      refs[[length(refs) + 1]] <- list(id = ref_id, text = ref_text)
    }
  }
  
  cat(sprintf("Extracted: DOI=%s, Title=%s..., Authors=%d, Sections=%d, Refs=%d\n",
              ifelse(has_content(doi), doi, "N/A"),
              ifelse(has_content(title), substr(title, 1, 50), "N/A"),
              length(authors), length(body_secs), length(refs)))
  
  # Build JATS
  article <- xml_new_root("article", "article-type" = "research-article", "dtd-version" = "1.1", 
                          "xml:lang" = "en", "xmlns:xlink" = "http://www.w3.org/1999/xlink")
  
  front <- xml_add_child(article, "front")
  journal_meta <- xml_add_child(front, "journal-meta")
  
  if (has_content(journal)) {
    jtg <- xml_add_child(journal_meta, "journal-title-group")
    xml_add_child(jtg, "journal-title", journal)
  }
  if (has_content(issn)) xml_add_child(journal_meta, "issn", issn, "pub-type" = "ppub")
  
  pub <- xml_add_child(journal_meta, "publisher")
  xml_add_child(pub, "publisher-name", "Elsevier")
  
  article_meta <- xml_add_child(front, "article-meta")
  if (has_content(doi)) xml_add_child(article_meta, "article-id", doi, "pub-id-type" = "doi")
  
  if (has_content(title)) {
    tg <- xml_add_child(article_meta, "title-group")
    xml_add_child(tg, "article-title", title)
  }
  
  if (length(authors) > 0) {
    cg <- xml_add_child(article_meta, "contrib-group")
    for (auth in authors) {
      c <- xml_add_child(cg, "contrib", "contrib-type" = "author")
      n <- xml_add_child(c, "name")
      xml_add_child(n, "surname", auth$family)
      if (has_content(auth$given)) xml_add_child(n, "given-names", auth$given)
      for (rid in auth$aff_rids) {
        xml_add_child(c, "xref", "", "ref-type" = "aff", "rid" = rid)
      }
    }
  }
  
  for (aff in affs) {
    if (has_content(aff$id)) {
      xml_add_child(article_meta, "aff", aff$text, "id" = aff$id)
    } else {
      xml_add_child(article_meta, "aff", aff$text)
    }
  }
  
  if (has_content(pub_year)) {
    pd <- xml_add_child(article_meta, "pub-date", "pub-type" = "epub")
    xml_add_child(pd, "year", pub_year)
  }
  
  if (has_content(volume)) xml_add_child(article_meta, "volume", volume)
  if (has_content(issue)) xml_add_child(article_meta, "issue", issue)
  if (has_content(fpage)) xml_add_child(article_meta, "fpage", fpage)
  if (has_content(lpage)) xml_add_child(article_meta, "lpage", lpage)
  
  if (has_content(copyright_text)) {
    perm <- xml_add_child(article_meta, "permissions")
    xml_add_child(perm, "copyright-statement", copyright_text)
  }
  
  if (length(abstract) > 0) {
    abs <- xml_add_child(article_meta, "abstract")
    for (p in abstract) xml_add_child(abs, "p", p)
  }
  
  if (length(keywords) > 0) {
    kg <- xml_add_child(article_meta, "kwd-group")
    for (kw in keywords) xml_add_child(kg, "kwd", kw)
  }
  
  if (length(body_secs) > 0) {
    body <- xml_add_child(article, "body")
    for (sec in body_secs) {
      s <- if (has_content(sec$id)) xml_add_child(body, "sec", "id" = sec$id) else xml_add_child(body, "sec")
      if (has_content(sec$title)) xml_add_child(s, "title", sec$title)
      for (p in sec$paragraphs) xml_add_child(s, "p", p)
    }
  }
  
  if (length(refs) > 0) {
    back <- xml_add_child(article, "back")
    rl <- xml_add_child(back, "ref-list")
    for (ref in refs) {
      r <- if (has_content(ref$id)) xml_add_child(rl, "ref", "id" = ref$id) else xml_add_child(rl, "ref")
      ec <- xml_add_child(r, "element-citation")
      xml_add_child(ec, "mixed-citation", ref$text)
    }
  }
  
  cat(sprintf("Writing: %s\n", output_file))
  write_xml(article, output_file, encoding = "UTF-8")
  cat("Done!\n")
  
  invisible(article)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Usage: Rscript elsevier_to_jats.R input.xml output.xml")
if (!file.exists(args[1])) stop(sprintf("File not found: %s", args[1]))

tryCatch(convert_elsevier_to_jats(args[1], args[2]), error = function(e) {
  cat(sprintf("Error: %s\n", e$message))
  quit(status = 1)
})
