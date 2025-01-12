
#' Get information for an Orphacode
#'
#' Information can be a definition, status or typology for an Orphacode
#'
#' @inheritParams get_icd10
#' @param code Orphacode to search for. Must be numeric
#' @param type The type of information to return. Possible values are "Status",
#' "Definition", "Typology"
#'
#' @return A data frame containing the ORPHAcode, the type and the date.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get status for a code
#' get_info(code = 16,
#'          type = "Status",
#'          lang = "EN",
#'          apiKey = "Apikey here")
#'
#' # Get definition for a code
#' get_info(code = 16,
#'          type = "Definition",
#'          lang = "EN",
#'          apiKey = "Apikey here")
#' }
#'
get_info <- function(code,
                     type = c("Definition", "Status", "Typology",
                              "Classification", "ClassificationLevel"),
                     lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                     apiKey,
                     output = c("df", "list", "json"),
                     verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)
  type <- match.arg(type)
  stopifnot(is.numeric(code))

  req <- build_req(lang,
                     params = paste0("/orphacode/", code, "/", type),
                     apiKey)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}
