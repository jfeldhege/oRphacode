
#' Get information for an Orphacode
#'
#' Information can be a definition, status or typology for an Orphacode
#'
#' @inheritParams get_icd10
#' @param code Orphacode to search for. Must be numeric
#' @param type The type of information to return. Possible values are "Status",
#' "Definition", "Typology"
#'
#' @return A data frame, list or json containing the ORPHAcode, the type and the date.
#' @export
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Get status for a code
#' get_info(code = 16,
#'          type = "Status",
#'          lang = "EN",
#'          api_key = "jfeldhege/oRphacode")
#'
#' # Get definition for a code
#' get_info(code = 16,
#'          type = "Definition",
#'          lang = "EN",
#'          api_key = "jfeldhege/oRphacode")
#'
#'
get_info <- function(code,
                     type = c("Definition", "Status", "Typology",
                              "Classification", "ClassificationLevel"),
                     lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                     api_key,
                     output = c("df", "list", "json"),
                     verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)
  type <- match.arg(type)
  stopifnot(is.numeric(code))

  req <- build_req(lang,
                   params = paste0("/orphacode/", code, "/", type),
                   api_key)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}





#' Get status for a specified Orphacode
#'
#' Returns the status (active/inactive)
#'
#' @inheritParams get_icd10
#' @param code Orphacode to search for. Must be numeric
#'
#' @returns A data frame, list or json containing the status and date for a specified ORPHAcode.
#' @export
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Get status for blue cone monochromatism (ORPHAcode 16)
#' get_status(code = 16,
#'          lang = "EN",
#'          api_key = "jfeldhege/oRphacode")
#'
get_status <- function(code,
                       lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                       api_key,
                       output = c("df", "list", "json"),
                       verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)
  stopifnot(is.numeric(code))

  req <- build_req(lang,
                   params = paste0("/orphacode/", code, "/Status"),
                   api_key)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}



#' Get definition for a specified Orphacode
#'
#'
#'
#' @inheritParams get_icd10
#' @param code Orphacode to search for. Must be numeric
#'
#' @returns A data frame, list or json containing the definition and date for a specified ORPHAcode.
#' @export
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Get definition for blue cone monochromatism (ORPHAcode 16)
#' get_definition(code = 16,
#'          lang = "EN",
#'          api_key = "jfeldhege/oRphacode")
#'
get_definition <- function(code,
                           lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                           api_key,
                           output = c("df", "list", "json"),
                           verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)
  stopifnot(is.numeric(code))

  req <- build_req(lang,
                   params = paste0("/orphacode/", code, "/Definition"),
                   api_key)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}


#' Get typology for a specified Orphacode
#'
#' Get the type for a specified code
#'
#' @inheritParams get_icd10
#' @param code Orphacode to search for. Must be numeric
#'
#' @returns A data frame, list or json containing the typology and date for a specified ORPHAcode.
#' @export
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Get type for blue cone monochromatism (ORPHAcode 16)
#' get_typology(code = 16,
#'          lang = "EN",
#'          api_key = "jfeldhege/oRphacode")
#'
#' # Get type for rare bone diseases (ORPHAcode 93419)
#' get_typology(code = 93419,
#'          lang = "EN",
#'          api_key = "jfeldhege/oRphacode")
#'
get_typology <- function(code,
                         lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                         api_key,
                         output = c("df", "list", "json"),
                         verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)
  stopifnot(is.numeric(code))
  stopifnot(is.character(api_key))

  req <- build_req(lang,
                   params = paste0("/orphacode/", code, "/Typology"),
                   api_key)

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}
