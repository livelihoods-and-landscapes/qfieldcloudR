#' Get QFieldCloud users
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#'
#' @return data.frame with two columns storing usernames and full names.
#' @export
#'

get_qfieldcloud_users <- function(token, endpoint) {

  url <- paste0("https://", endpoint, "/api/v1/users/")

  users <- tryCatch(
    error = function(cnd) {
      projects <- "Failed to get users - check endpoint and that login was successful."
    },
    {
      users <- httr::GET(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token))
      )

      users_parsed <- httr::content(users, as = "parsed")

      username <- c()
      full_name <- c()

      for (i in users_parsed) {
        username <- c(username, i$username)
        full_name <- c(full_name, i$full_name)
      }

      users <- data.frame(username = username, full_name = full_name)

      users
    }
  )

  users
}
