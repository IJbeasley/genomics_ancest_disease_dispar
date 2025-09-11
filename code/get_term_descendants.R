

library(httr)
library(jsonlite)

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

  print("Number of terms collected:")
  print(length(terms))

  print("\n Some example terms")
  print(terms[1:5])

  return(terms)


}
