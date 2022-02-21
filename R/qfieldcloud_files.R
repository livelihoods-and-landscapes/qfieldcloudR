#' Get QFieldCloud files in project (GeoPackages only)
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#' @param project_id project id corresponding to project to download file from
#'
#' @return data.frame with two columns storing file name and last modified data
#' @export
#'

get_qfieldcloud_files <- function(token, endpoint, project_id) {

  url <- paste0("https://", endpoint, "/api/v1/files/", project_id, "/")

  files <- tryCatch(
    error = function(cnd) {
      files <- "Failed to get project's files."
    },
    {
      files_response <- httr::GET(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token))
      )

      files_parsed <- httr::content(files_response, as = "parsed")

      f_name <- c()
      f_last_modified <- c()

      for (i in files_parsed){
        f_name <- c(f_name, i$name)
        f_last_modified <- c(f_last_modified, i$last_modified)
      }

      files <- data.frame(name = f_name, last_modified = f_last_modified)

      files
    }
  )

  files
}
