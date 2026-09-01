

library(httr)
library(jsonlite)


# Function to sort a character vector by string length in descending order
str_length_sort <- function(vec) {
  sorted_desc <- vec[order(nchar(vec), decreasing = TRUE)]
  return(sorted_desc)
}


# Function to get all descendants of a given EFO (or other ontology) term from the GWAS Catalog API
# retrieves term name (e.g. "Alzheimer's disease") for all descendants of a given term
# doesn't retrieve the term IDs (e.g. EFO_0000249)

# returns them in all lower case, and trimmed of whitespace
get_descendants <- function(url){

  terms <- c()

  repeat {
    res <- GET(url)
    stop_for_status(res)
    data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

    terms <- c(terms, data$`_embedded`$terms$label)

    # check if there is a next page
    if (!is.null(data$`_links`$`next`$href)) {
      url <- data$`_links`$`next`$href
    } else {
      break
    }
  }

  terms = unlist(terms)
  terms = stringr::str_trim(tolower(terms))
  terms = unique(terms)
  terms = str_length_sort(terms)

  print("Number of terms collected:")
  print(length(terms))

  print("\n Some example terms")
  print(terms[1:5])

  return(terms)


}

# Function to convert a character vector into a grep pattern
# that deals with comma separated values
vec_to_grep_pattern <- function(vec){

  # remove any NULL or NA values from the vector
  vec <- vec[!is.na(vec) & vec != ""]

  # remove any leading or trailing whitespace from the vector
  vec <- stringr::str_squish(vec)
  vec <- vec[nzchar(vec)]

  # remove any duplicate values from the vector
  vec <- unique(vec)

  # sort the vector by string length in descending order
  vec <- vec[order(stringr::str_length(vec), decreasing = T)]

  stopifnot(length(vec) > 0)

  vec <- stringr::str_escape(vec)

  vec <- paste0("(?<=^|, )", vec)
  vec <- paste0(vec, "(?=,|$)")

  if(length(vec) > 1){
    vec <- paste0(vec, collapse = "|")
  }

  return(vec)

}
