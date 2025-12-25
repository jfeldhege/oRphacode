#' Get all Orphanet classifications
#'
#' Retrieves all clinical classifications from the 'ORPHAcode' API.
#'
#' @inheritParams get_icd10
#'
#' @returns A data frame containing all classifications in the chosen language.
#' @export
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' get_classifications(lang = "EN",
#'                    api_key = "jfeldhege")
#'
get_classifications <- function(lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                                api_key,
                                output = c("df", "list", "json"),
                                verbosity = 0) {

  lang <- toupper(lang)
  output <- match.arg(output)
  stopifnot(is.character(api_key))

  req <- build_req(lang,
                   params = "Classification",
                   api_key)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}



#' Get classification for a specified ORPHAcode
#'
#' @inheritParams get_icd10
#'
#' @returns A data frame, list or json containing the classification and datetime for a specified ORPHAcode.
#' @export
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#'
#' # Get classification for blue cone monochromatism (ORPHAcode 16)
#' get_classification(code = 16,
#'                    lang = "EN",
#'                    api_key = "jfeldhege/oRphacode")
#'
get_classification <- function(orpha_code,
                               lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                               api_key,
                               output = c("df", "list", "json"),
                               verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)
  stopifnot(is.numeric(orpha_code))
  stopifnot(is.character(api_key))

  req <- build_req(lang,
                   params = paste0("/orphacode/", orpha_code, "/Classification"),
                   api_key)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}

#' Get classification level for a specified ORPHAcode
#'
#' @inheritParams get_icd10
#'
#' @returns A data frame, list or json containing the classification level and datetime for a specified ORPHAcode.
#' @export
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#'
#' # Get classification for blue cone monochromatism (ORPHAcode 16)
#' get_classification_level(code = 16,
#'                          lang = "EN",
#'                          api_key = "jfeldhege/oRphacode")
#'
get_classification_level <- function(orpha_code,
                                     lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                                     api_key,
                                     output = c("df", "list", "json"),
                                     verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)
  stopifnot(is.numeric(orpha_code))
  stopifnot(is.character(api_key))

  req <- build_req(lang,
                   params = paste0("/orphacode/", orpha_code, "/ClassificationLevel"),
                   api_key)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}
