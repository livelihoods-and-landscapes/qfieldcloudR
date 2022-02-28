#' Get QFieldCloud files in project
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

#' Get QFieldCloud file
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#' @param project_id project id corresponding to project to download file from
#' @param filename filename of file to download
#'
#' @return dataframe with filename and path to temporary location where file is downloaded
#' @export
#'

get_qfieldcloud_file <- function(token, endpoint, project_id, filename) {

  url <- url <- paste0("https://", endpoint, "/api/v1/files/", project_id, "/", filename, "/")

  file_path_df <- tryCatch(
    error = function(cnd) {
      file_path_df <- "Failed to download file"
    },
    {
      # need to use followlocation = FALSE to get redirect url
      file_data <- httr::with_config(httr::config(followlocation = FALSE), httr::GET(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token))
      ))

      # location <- file_data$headers$location
      #
      # file_data <- httr::GET(
      #   url = location
      # )

      ext <- xfun::file_ext(filename)
      fname <- xfun::sans_ext(filename)

      f_data <- httr::content(file_data, as = "raw")

      tmp_dir <- tempdir()
      tmp_file <- paste0(tmp_dir, "/", filename)

      writeBin(f_data, tmp_file)

      file_path_df <- data.frame(filename = filename, tmp_file = tmp_file)
    }
  )

  file_path_df
}
