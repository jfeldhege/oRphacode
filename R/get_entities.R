#' Title
#'
#' @inheritParams get_icd10
#'
#' @return A data frame containing all entities in the chosen language.
#' @export
#'
#' @examples
#' \dontrun{
#' get_entities(lang = "EN",
#'              apiKey = "Apikey here")
#' }
get_entities <- function(lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                         apiKey,
                         output = c("df", "list", "json"),
                         verbosity = 0) {

  lang <- toupper(lang)
  output <- match.arg(output)

  req <- build_req(lang,
                     params = NULL,
                     apiKey)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}
