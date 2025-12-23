
#' Find association of ORPHAcode and ICD-10 code
#'
#' @param ORPHAcode The ORPHAcode
#' @param icd10code The ICD-10 code to look for
#' @param lang The language in which entities are to be returned. Possible
#' values are "CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT".
#' @param api_key The API key to use for authentication.
#' @param output The format of the output. Must be "df" for a data frame,
#'  "list" for a list or "json" for a raw json string.
#' @param verbosity How much information to print
#'
#' @return A data frame
#' @export
#'
#' @seealso [get_icd11()]
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#'
#' get_icd10(ORPHAcode = 15,
#'           lang = "EN",
#'           api_key = "Apikey here")
#'
#' get_icd10(icd10code = "Q77.4",
#'           lang = "EN",
#'           api_key = "Apikey here")
#'
get_icd10 <- function(ORPHAcode = NULL,
                      icd10code = NULL,
                      lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                      api_key,
                      output = c("df", "list", "json"),
                      verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)

  if (is.null(ORPHAcode) & is.null(icd10code)) {

    stop("One of ORPHAcode or icd10code must be specified.")

  } else if (!is.null(ORPHAcode)) {

    stopifnot(is.numeric(ORPHAcode))

    req <- build_req(lang,
                     params = paste0("/orphacode/", ORPHAcode, "/ICD10"),
                     api_key)

  } else if (!is.null(icd10code)) {

    stopifnot(is.character(icd10code))

    req <- build_req(lang,
                     params = paste0("/ICD10/", icd10code),
                     api_key)
  } else {

    stop("Only one of ORPHAcode or icd10code must be specified.")
  }

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}




#' Find association of ORPHAcode and ICD-11 code
#'
#' @inheritParams get_icd10
#' @param icd11code The ICD-11 code to look for
#'
#' @return Depending on the output format chosen, a data frame, list or raw json string.
#' @export
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#'
#' # Get ICD-11 code for Achondroplasia (ORPHAcode = 15)
#' get_icd11(ORPHAcode = 15,
#'           lang = "EN",
#'           api_key = "jfeldhege/oRphacode")
#'
#' # Search orphacodes for ICD-11 code LD24.00 for Achondroplasia
#' get_icd11(icd11code = "LD24.00",
#'           lang = "EN",
#'           api_key = "jfeldhege/oRphacode")
#'
get_icd11 <- function(ORPHAcode = NULL,
                      icd11code = NULL,
                      lang = c("CS", "DE", "EN", "ES", "FR", "IT", "NL", "PL", "PT"),
                      api_key,
                      output = c("df", "list", "json"),
                      verbosity = 0) {

  lang <- toupper(match.arg(lang))
  output <- match.arg(output)

  if (is.null(ORPHAcode) & is.null(icd11code)) {

    stop("One of ORPHAcode or icd11code must be specified.")
  } else if (!is.null(ORPHAcode)) {

    stopifnot(is.numeric(ORPHAcode))

    req <- build_req(lang,
                     params = paste0("/orphacode/", ORPHAcode, "/ICD11"),
                     api_key)
  } else if (!is.null(icd11code)) {

    stopifnot(is.character(icd11code))

    req <- build_req(lang,
                     params = paste0("/ICD11/", icd11code),
                     api_key)
  } else {
    stop("Only one of ORPHAcode or icd11code must be specified.")
  }

  resp <- httr2::req_perform(req, verbosity = verbosity)

  convert_body(resp$body, output = output)
}



