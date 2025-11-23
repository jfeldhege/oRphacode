#' Get all Orphanet clinical entities
#'
#' Retrieves all clinical entities from the 'ORPHAcode' API.
#'
#' @inheritParams get_icd10
#'
#' @return A data frame containing all entities in the chosen language.
#' @export
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' get_entities(lang = "EN",
#'              api_key = "jfeldhege")
#'
get_entities <- function(lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                         api_key,
                         output = c("df", "list", "json"),
                         verbosity = 0) {

  lang <- toupper(lang)
  output <- match.arg(output)

  req <- build_req(lang,
                   params = NULL,
                   api_key)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}
