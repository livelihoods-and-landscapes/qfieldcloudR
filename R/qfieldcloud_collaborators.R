#' Get collaborators in a QFieldCloud project
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#' @param project_id QFieldCloud project id
#'
#' @return data.frame with two columns storing project names and project ids
#' @export
#'

get_qfieldcloud_collaborators <- function(token, endpoint, project_id) {

  url <- paste0("https://", endpoint, "/api/v1/collaborators/", project_id, "/")

  collaborators <- tryCatch(
    error = function(cnd) {
      collaborators <- "Failed to get collaborators."
    },
    {
      collaborators_response <- httr::GET(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token))
      )

      collaborators_parsed <- httr::content(collaborators_response, as = "parsed")

      collaborator <- c()
      role <- c()

      for (i in collaborators_parsed) {
        collaborator <- c(collaborator, i$collaborator)
        role <- c(role, i$role)
      }

      collaborators <- data.frame(collaborator = collaborator, role = role)

      collaborators
    }
  )

  collaborators
}

#' Get the role of a collaborator in a QFieldCloud project
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#' @param project_id QFieldCloud project id
#' @param username QFieldCloud username
#'
#' @return data.frame with two columns storing username and role
#' @export
#'

get_qfieldcloud_collaborator <- function(token, endpoint, project_id, username) {

  url <- paste0("https://", endpoint, "/api/v1/collaborators/", project_id, "/", username, "/")

  collaborator <- tryCatch(
    error = function(cnd) {
      collaborator <- "Failed to get collaborator."
    },
    {
      collaborator_response <- httr::GET(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token))
      )

      collaborator_parsed <- httr::content(collaborator_response, as = "parsed")

      collaborator <- data.frame(collaborator_parsed)

      collaborator
    }
  )

  collaborator
}

#' Add a collaborator to a QFieldCloud project
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#' @param project_id QFieldCloud project id
#' @param collaborator QFieldCloud username to add
#' @param role QFieldCloud role to assign the user within the project
#'
#' @return string indicating success or failure
#' @export
#'

add_qfieldcloud_collaborator <- function(token, endpoint, project_id, collaborator, role) {

  url <- paste0("https://", endpoint, "/api/v1/collaborators/", project_id, "/")

  add_collaborator_status <- tryCatch(
    error = function(cnd) {
      add_collaborator_status <- "Failed to add collaborator."
    },
    {
      content <- list(
        collaborator = collaborator,
        role = role
      )

      add_response <- httr::POST(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token)),
        body = content
      )

      status_code <- add_response$status_code

      if (status_code < 399) {
        add_collaborator_status <- "success"
      } else {
        add_collaborator_status <- "Failed to add collaborator."
      }

      add_collaborator_status
    }
  )

  add_collaborator_status
}

#' Update a collaborator to a QFieldCloud project
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#' @param project_id QFieldCloud project id
#' @param username QFieldCloud username to add
#' @param role QFieldCloud role to assign the user within the project
#'
#' @return string indicating success or failure
#' @export
#'

update_qfieldcloud_collaborator <- function(token, endpoint, project_id, username, role) {

  url <- paste0("https://", endpoint, "/api/v1/collaborators/", project_id, "/", username, "/")

  update_collaborator_status <- tryCatch(
    error = function(cnd) {
      update_collaborator_status <- "Failed to update collaborator."
    },
    {
      content <- list(
        role = role
      )

      update_response <- httr::PUT(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token)),
        body = content
      )

      status_code <- update_response$status_code

      if (status_code < 399) {
        update_collaborator_status <- "success"
      } else {
        update_collaborator_status <- "Failed to update collaborator."
      }

      update_collaborator_status
    }
  )

  update_collaborator_status
}

#' Delete a collaborator to a QFieldCloud project
#'
#' @param token session token
#' @param endpoint QFieldCloud app url (omit https:// and trailing /)
#' @param project_id QFieldCloud project id
#' @param username QFieldCloud username to delete
#'
#' @return string indicating success or failure
#' @export
#'

delete_qfieldcloud_collaborator <- function(token, endpoint, project_id, username) {

  url <- paste0("https://", endpoint, "/api/v1/collaborators/", project_id, "/", username, "/")

  delete_collaborator_status <- tryCatch(
    error = function(cnd) {
      delete_collaborator_status <- "Failed to delete collaborator."
    },
    {

      delete_response <- httr::DELETE(
        url = url,
        httr::add_headers(Authorization = paste0("token ", token))
      )

      print(delete_response)

      status_code <- delete_response$status_code

      if (status_code < 399) {
        delete_collaborator_status <- "success"
      } else {
        delete_collaborator_status <- "Failed to delete collaborator."
      }

      delete_collaborator_status
    }
  )

  delete_collaborator_status
}

