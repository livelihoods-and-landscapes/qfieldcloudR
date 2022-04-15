#' Get QFieldCloud projects for authenticated user
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#'
#' @return data.frame with three columns storing project names, project ids, and user roles
#' @export
#'

get_qfieldcloud_projects <- function(token, endpoint) {

  # todo handle query params to get community projects
  url <- paste0("https://", endpoint, "/api/v1/projects?include-public=false")

  projects <- tryCatch(
    error = function(cnd) {
      projects <- "Failed to get user's projects - check endpoint and that login was successful."
    },
    {

      projects_response <- httr::GET(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token))
      )

      projects_parsed <- httr::content(projects_response, as = "parsed")

      names <- c()
      id <- c()
      user_role <- c()

      for (i in projects_parsed) {
        names <- c(names, i$name)
        id <- c(id, i$id)
        user_role <- c(user_role, i$user_role)
      }

      projects <- data.frame(name = names, id = id, user_role = user_role)

      projects
    }

  )

  projects
}


#' Get a QFieldCloud project
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#' @param project_id QFieldCloud project id
#'
#' @return data.frame with project information
#' @export
#'

get_qfieldcloud_project <- function(token, endpoint, project_id) {
  url <- paste0("https://", endpoint, "/api/v1/projects/", project_id, "/")

  project <- tryCatch(
    error = function(cnd) {
      project = "Failed to get project information."
    },
    {
      project_response <- httr::GET(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token))
      )

      project_parsed <- httr::content(project_response, as = "parsed")

      project <- data.frame(project_parsed)
    }
  )

  project
}


#' Create a QFieldCloud project
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#' @param project_name QFieldCloud project name
#'
#' @return string indicating project creation success or failure
#' @export
#'

post_qfieldcloud_project <- function(token, endpoint, project_name) {
  url <- paste0("https://", endpoint, "/api/v1/projects/")

  create_project_status <- tryCatch(
    error = function(cnd) {
      project = "Failed to create project."
    },
    {
      content <- list(
        name = project_name
      )

      project_response <- httr::POST(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token)),
        body = content
      )

      status_code <- project_response$status_code

      if (status_code < 399) {
        create_project_status <- "success"
      } else {
        create_project_status <- "Failed to add collaborator."
      }

      create_project_status

    }
  )

  create_project_status
}
