#' Search by Orphacode
#'
#' @inheritParams get_icd10
#' @param code Orphacode to search for. Must be numeric
#'
#' @return A data frame containing
#' @export
#'
#' @examples
#' \dontrun{
#' search_code(code = 16,
#'             lang = "EN",
#'             apiKey = "Apikey here")
#' }
#'
search_code <- function(code,
                        lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                        apiKey,
                        output = c("df", "list", "json"),
                        verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)
  stopifnot(is.numeric(code))

  req <- build_req(lang,
                   params = paste0("/orphacode/", code, "/Name"),
                   apiKey)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}


#' Search by name
#'
#' @inheritParams get_icd10
#' @param name Character. The name to search for.
#' @param exact Logical. If true, only exact matches are returned. Otherwise an approximate, fuzzy search is made.
#'
#' @return A data frame containing all matches for the searched name. Columns are:
#' - Preferred term
#' - ORPHAcode
#' - Date
#' @export
#'
#' @examples
#' \dontrun{
#' search_name(name = "Marfan syndrome",
#'             exact = FALSE,
#'             lang = "EN",
#'             apiKey = "Apikey here")
#' }

search_name <- function(name,
                        exact = FALSE,
                        lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                        apiKey,
                        output = c("df", "list", "json"),
                        verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)
  name <- gsub(" ", "%20", as.character(name))
  param <- ifelse(exact, "FindbyName", "ApproximateName")

  req <- build_req(lang,
                   params = paste0("/", param, "/", name),
                   apiKey)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}

#' Search by synonym
#'
#' @inheritParams get_icd10
#' @param synonym The synonym to search for
#' @param exact Logical. If true, only exact matches are returned. Otherwise an approximate, fuzzy search is made.
#'
#' @return A data frame containing all matches for the searched synonym. Columns are:
#' - Preferred term
#' - ORPHAcode
#' - Date
#' @export
#'
#' @examples
#' \dontrun{
#' search_synonym(synonym = "MFS",
#'                exact = FALSE,
#'                lang = "EN",
#'                apiKey = "Apikey here")
#' }
#'

search_synonym <- function(synonym,
                           exact = FALSE,
                           lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                           apiKey,
                           output = c("df", "list", "json"),
                           verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)
  synonym <- gsub(" ", "%20", as.character(synonym))
  param <- ifelse(exact, "FindbyName", "ApproximateName")

  req <- build_req(lang,
                   params = paste0("/", param, "/", synonym, "/Synonym"),
                   apiKey)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}

