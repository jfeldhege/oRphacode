
#' Builds request to the API
#'
#' @param lang Language
#' @param params parameters of the request
#' @param apiKey API key
#'
#' @return An HTTP request: an S3 list with class httr2_request.
#' @noRd

build_req <- function(lang,
                        params,
                        apiKey){

  # httr2::request(paste0("https://api.orphacode.org/",
  #                       lang,
  #                       "/ClinicalEntity",
  #                       params
  # ))
  httr2::request("https://api.orphacode.org/") |>
    httr2::req_url_path_append(lang)  |>
    httr2::req_url_path_append("ClinicalEntity") |>
    httr2::req_url_path_append(params) |>
    httr2::req_headers("Accept" = "application/json")|>
    httr2::req_headers("apiKey" = apiKey)
}

#' Convert response body to data frame
#'
#' @param resp_body Response body
#' @param output Output type. Must be "df", "list", or "json"
#'
#' @return a data frame
#' @noRd

convert_body <- function(resp_body,
                         output = c("df", "list", "json")) {

  resp_body <- rawToChar(resp_body)

  if (output == "json") {
    resp_body
  } else if (output == "df") {
    as.data.frame(jsonlite::fromJSON(resp_body))
  } else if (output == "list") {
    jsonlite::fromJSON(resp_body)
  }
}

