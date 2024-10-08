% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chatlogr.R
\name{get_user_chat}
\alias{get_user_chat}
\title{Get and view chat history of a user}
\usage{
get_user_chat(chatdata, user_id = NULL, random_user = FALSE)
}
\arguments{
\item{chatdata}{Chat data list object from parse_users_chat_data or chat history dataframe.}

\item{user_id}{A user ID in character. Default is NULL.}

\item{random_user}{A logical value. If TRUE, a random user will be selected. Default is FALSE.}
}
\value{
Chat history of a user in a tibble.
}
\description{
Get and view chat history of a user
}
\examples{
# read and process raw data
csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
chatdata <- parse_users_chat_data(csv_file, idcol = "user_id")

# pass in chatdata list object
get_user_chat(chatdata, user_id = 1)

# pass in chat history dataframe
get_user_chat(chatdata$df_success, user_id = 1)
}
