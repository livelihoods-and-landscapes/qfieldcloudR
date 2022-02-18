#' QFieldCloud login
#'
#' @param username QFieldCloud email
#' @param password QFieldCloud password
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#'
#' @return list object which records the status of the request and a token if
#'   login was successful.
#' @export

qfieldcloud_login <- function(username, password, endpoint) {

  credentials <- list(
    email = username,
    password = password
  )

  # todo handle empty / faulty endpoint
  login_url <- paste0("https://", endpoint, "/api/v1/auth/login/")

  httr::handle_reset(login_url)

  login_status <- tryCatch(
    error = function(cnd) {
      login_status <- list(
        status = "fail - no response from server. Check endpoint URL is correct.",
        token = NULL
      )
    },
    {
      token <- httr::POST(
        url = login_url,
        body = credentials,
        encode = "json"
      )

      status_code <- token$status_code

      if (status_code < 399) {
        login_status <- list(
          status = "success",
          token = httr::content(token, as = "parsed")$token
        )
      } else {
        login_status <- list(
          status = "fail",
          token = NULL
        )
      }

      login_status
    }
  )

  login_status
}
