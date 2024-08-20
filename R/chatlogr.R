get_matching_col_names <- function(dat, patterns = c("chathistory")) {
    pattern <- unique(patterns)
    matched <- c()
    for (p in patterns) {
        matched <- c(matched, grep(p, colnames(dat), value = TRUE))
    }
    return(unique(sort(matched)))
}


get_available_df_columns <- function(dat, cols = c("chathistory00", "chathistory01", "chathistory02")) {
    cols <- unique(cols)
    updated_cols <- intersect(cols, colnames(dat))
    return(dat[, updated_cols])
}



check_if_file_exist <- function() {
    return(file.exists(file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")))
}


#' Read csv file to get matching data columns
#'
#' @param csv_file Path to csv file in character.
#' @param idcol ID column in character.
#' @param chat_col_patterns Patterns to match chat columns in character. Defaults to "chathistory".
#' @param chat_cols Chat columns in character. Defaults to "".
#'
#' @importFrom readr read_csv
#'
#' @return Dataframe/tibble with matching columns.
#' @export
#'
#' @examples
#' csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
#' dat <- read_relevant_data_columns(csv_file, "vs", "mpg", "cyl")
read_relevant_data_columns <- function(csv_file,
                                       idcol,
                                       chat_col_patterns = c("chathistory"),
                                       chat_cols = "") {
    if (length(idcol) > 1) {
        stop("idcol must be a single column name")
    }
    dat <- readr::read_csv(csv_file, show_col_types = FALSE)
    chat_cols1 <- c()
    if (chat_col_patterns[1] != "") {
        chat_cols1 <- get_matching_col_names(dat, chat_col_patterns)
    }
    chat_cols2 <- c()
    if (chat_cols[1] != "") {
        chat_cols2 <- chat_cols
    }
    unique_chat_cols <- sort(unique(c(chat_cols1, chat_cols2)))
    l <- unique_chat_cols[unique_chat_cols != ""]
    dat <- get_available_df_columns(dat, c(idcol, unique_chat_cols))
    return(dat)
}







#' Parse a single user's raw chat data
#'
#' @param user_chat_data Raw chat data for a single user. Must only include chat data columns.
#' @param user_id User ID in character.
#' @param join_str Join string in character. Defaults to '\"\"'.
#' @param verbose Logical. Defaults to TRUE.
#'
#' @importFrom yyjsonr read_json_str validate_json_str
#' @importFrom tibble tibble
#'
#' @return JSON data as list.
#' @export
#'
#' @examples
#' csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
#' user_chat_data <- read_relevant_data_columns(csv_file, "user_id", "chathistory")[1, ]
#' user_chatdata <- parse_user_chat_data(user_chat_data, "vs", join_str = '\"\"')
parse_user_chat_data <- function(user_chat_data, user_id, join_str = '\"\"', verbose = TRUE) {

    # remove columns with NA
    user_chat_data <- user_chat_data[, colSums(!is.na(user_chat_data)) > 0]
    user_data_list <- as.list(user_chat_data)
    n_items <- length(user_data_list)
    names(user_data_list) <- NULL
    if (verbose) {
        message(paste0(user_id, " has ", n_items, " cells"))
    }

    if (length(user_data_list) == 1) {
        json_obj <- list()
        if (yyjsonr::validate_json_str(user_data_list[[1]])) {
            json_obj0 <- yyjsonr::read_json_str(user_data_list[[1]])
            if (is.data.frame(json_obj0)) {  # old format with just chat history (no parameters)
                json_obj$messages <- tibble::tibble(json_obj0)
                json_obj$status <- "success"
            } else {  # new format with chat history and other parameters
                json_obj <- yyjsonr::read_json_str(json_obj0)
                json_obj$messages <- tibble::tibble(json_obj$messages)
                json_obj$status <- "success"
            }
        } else {
            json_obj <- list(status = "error: json validation error")
            json_obj$messages <- tibble::tibble(
                role = NA,
                content = user_data_list[[1]],
                createdAt = NA,
                id = NA)
        }
    } else {
        # concatenate all cells
        user_data_string <- paste0(user_data_list, collapse = "")
        user_data_string <- gsub(join_str, "", user_data_string)
        if (yyjsonr::validate_json_str(user_data_string)) {
            json_obj <- yyjsonr::read_json_str(yyjsonr::read_json_str(user_data_string))
            json_obj$status <- "success"
        } else {  # validation error
            json_obj <- list(status = "error: json validation error")
            json_obj$messages <- tibble::tibble(
                role = NA,
                content = user_data_string,
                createdAt = NA,
                id = NA)
        }
    }
    json_obj$user_id <- user_id
    if (verbose) {
        message(paste0("- status: ", json_obj$status))
    }

    return(json_obj)
}



check_unique_ids <- function(dat, idcol) {
    if (nrow(unique(dat[, idcol])) != nrow(dat)) {
        return(FALSE)
    }
    return(TRUE)
}

remove_na_chat_columns <- function(dat) {
    n_na <- colSums(is.na(dat))
    chat_cols <- names(n_na[n_na < nrow(dat)])
    return(dat[, chat_cols])
}

ensure_chat_columns_are_char <- function(dat, idcol) {
    temp <- dat[, setdiff(colnames(dat), idcol)]
    if (!all(sapply(temp, class) == "character")) {
        return(FALSE)
    }
    return(TRUE)
}


#' Parse all user data
#'
#' @param csv_file Path to csv file in character.
#' @param idcol Name of column that is the user ID column in character. Must contain unique values.
#' @param chat_col_patterns Patterns to match chat columns in character. Defaults to "chathistory".
#' @param chat_cols Chat columns in character. Defaults "".
#' @param join_str Join string in character. Defaults to '\"\"'.
#' @param nrows Number of rows to process. Defaults to Inf. Use fewer rows to debug the function/parser.
#' @param verbose Whether to print processing status. Defaults to FALSE.
#'
#' @importFrom dplyr bind_rows filter arrange all_of everything select
#' @importFrom stringr str_count
#' @importFrom tibble tibble
#'
#' @return A list with the following keys:
#' \describe{
#'   \item{info}{A vector indicating total number of rows processed, number of success/error/warning parses.}
#'   \item{info_df}{A dataframe/tibble with info on each user's chat data, where the status column indicates parsing status and mobile column indicates mobile (1) or not (0).}
#'   \item{json_list}{A list of JSON data for each user. Contains full data and can be a very large list.}
#'   \item{df_success}{A dataframe/tibble with successful parses. Contains only chat messages and not other data.}
#'   \item{df_fail}{A dataframe/tibble with failed parses. Contains only chat messages and not other data.}
#' }
#'
#' @export
#'
#' @examples
#' csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
#' chatdata <- parse_users_chat_data(csv_file, "user_id")
parse_users_chat_data <- function(csv_file,
                       idcol = "ResponseId",
                       chat_col_patterns = c("chathistory"),
                       chat_cols = "",
                       join_str = '\"\"',
                       nrows = Inf,
                       verbose = FALSE) {

    dat <- read_relevant_data_columns(csv_file, idcol, chat_col_patterns, chat_cols)

    if (!check_unique_ids(dat, idcol)) {
        stop("idcol must contain unique values")
    }

    dat <- remove_na_chat_columns(dat)

    if (!ensure_chat_columns_are_char(dat, idcol)) {
        stop("All chat data columns must be character")
    }

    if (nrows != Inf) {
        dat <- dat[sample(1:nrow(dat), nrows), ]
        message(paste0("Sampling ", nrow(dat), " rows..."))
    }
    message(paste0("Processing ", nrow(dat), " rows..."))

    json_objects <- list()
    dataframe_list <- list()
    info <- list(success = list(), errors = list(), warnings = list())

    for (i in 1:nrow(dat)) {
        user_id <- dat[i, idcol, drop = TRUE]
        chat_cols <- setdiff(colnames(dat), idcol)  # remove idcol
        user_chat_data <- dat[i, chat_cols]

        tryCatch({
            id <- status <- role <- NULL
            # parse user data; convert from string to json
            json_obj <- parse_user_chat_data(user_chat_data, user_id, join_str, verbose)
            mobile <- NA
            if (!is.null(json_obj$userAgentInfo$mobile)) {
                mobile <- ifelse(json_obj$userAgentInfo$mobile, 1, 0)
            }
            temp_info <- data.frame(json_obj$user_id,
                                    json_obj$status,
                                    mobile)
            colnames(temp_info) <- c(idcol, "status", "mobile")
            json_objects[[user_id]] <- json_obj

            if (grepl("error", json_obj$status)) {
                info$errors[[user_id]] <- temp_info
            } else if (grepl("success", json_obj$status)) {
                info$success[[user_id]] <- temp_info
            }

            # convert to dataframe
            dataframe_list[[user_id]] <- tibble::tibble(json_obj$messages)
            dataframe_list[[user_id]]$n_words <- stringr::str_count(dataframe_list[[user_id]]$content, "\\w+")
            dataframe_list[[user_id]][, idcol] <- user_id
            dataframe_list[[user_id]] <- dplyr::arrange(dataframe_list[[user_id]], id)
        }, error = function(e) {
            temp_info <- dplyr::tibble(user_id, content = "error: fail to parse json string")
            colnames(temp_info) <- c(idcol, "status")
            info$errors[[user_id]] <<- temp_info
        }, warning = function(w) {
            temp_info <- dplyr::tibble(user_id, content = "warning: fail to parse json string")
            colnames(temp_info) <- c(idcol, "status")
            info$warnings[[user_id]] <<- temp_info
        })

    }

    info_summary <- unlist(lapply(info, length))
    info_summary <- c(total = nrow(dat), info_summary)
    df_summary <- tibble::tibble(dplyr::arrange(dplyr::bind_rows(lapply(info, dplyr::bind_rows)), status))
    df0 <- dplyr::select(dplyr::bind_rows(dataframe_list), dplyr::all_of(idcol), dplyr::everything())
    message("Parsing summary/status")
    print(info_summary)

    return(list(info = info_summary,
                info_df = df_summary,
                json_list = json_objects,
                df_success = dplyr::filter(df0, !is.na(role)),
                df_fail = dplyr::filter(df0, is.na(role))
                )
           )

}





#' Write chat data to json file
#'
#' @param chatdata Chat data from parse_users_chat_data.
#' @param output_file Output file name. Must end with '.json'.
#'
#' @importFrom yyjsonr write_json_file
#'
#' @return TRUE if successful.
#' @export
#'
#' @examples
#' \dontrun{
#' write_to_json(chatdata, "chatdata.json")
#' }
write_to_json <- function(chatdata, output_file) {
    if (!grepl(".json$", output_file)) {
        stop("output_file must end with '.json'")
    }
    message(paste0("Writing all chat data to ", output_file))
    yyjsonr::write_json_file(chatdata, output_file)
    return(TRUE)
}



#' Write chat history to csv file
#'
#' Write only chat history to csv file. Does not include other information contained in the full chat data. To save the full chat data, use `write_to_json()`.
#'
#' @param chatdata Chat data from parse_users_chat_data.
#' @param output_file Output file name. Must end with '.csv'.
#'
#' @importFrom dplyr bind_rows
#' @importFrom readr write_csv
#'
#' @return TRUE if successful.
#' @export
#'
#' @examples
#' \dontrun{
#' write_to_csv(chatdata, "chatdata.csv")
#' }
write_to_csv <- function(chatdata, output_file) {
    if (!grepl(".csv$", output_file)) {
        stop("output_file must end with '.csv'")
    }
    message(paste0("Writing only chat history to ", output_file))
    df0 <- dplyr::bind_rows(chatdata$df_fail, chatdata$df_success)
    readr::write_csv(df0, output_file)
    return(TRUE)
}



#' Get ids whose data have been parsed sucessfully
#'
#' @param chatdata Chat data from parse_users_chat_data.
#'
#' @return A vector of user ids.
#' @export
#'
#' @examples
#' csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
#' chatdata <- parse_users_chat_data(csv_file, "user_id")
#' get_success_ids(chatdata)
get_success_ids <- function(chatdata) {
    return(unique(chatdata$df_success[[1]]))
}



#' Get ids whose data have failed to parse
#'
#' @param chatdata Chat data from parse_users_chat_data.
#'
#' @return A vector of user ids.
#' @export
#'
#' @examples
#' csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
#' chatdata <- parse_users_chat_data(csv_file, "user_id", nrows = 5)
#' get_failed_ids(chatdata)
get_failed_ids <- function(chatdata) {
    return(unique(chatdata$df_fail[[1]]))
}







#' Get and view chat history of a user
#'
#' @param chatdata Chat data from parse_users_chat_data.
#' @param user_id A user ID in character. Default is NULL.
#' @param random_user A logical value. If TRUE, a random user will be selected. Default is FALSE.
#'
#' @importFrom crayon red green
#'
#' @return Chat history of a user in a tibble.
#' @export
#'
#' @examples
#' csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
#' chatdata <- parse_users_chat_data(csv_file, "user_id")
#' get_user_chat(chatdata, user_id = 1)
get_user_chat <- function(chatdata, user_id = NULL, random_user = FALSE) {
    if (is.null(user_id) && !random_user) {
        stop("Please provide a user_id or set random_user = TRUE")
    } else if (random_user) {
        user_id <- sample(get_success_ids(chatdata), 1)
    }

    if (is.null(chatdata$json_list[[user_id]])) {
        stop(paste0("User ", user_id, " not found"))
    }

    success <- chatdata$json_list[[user_id]]$status
    if (success == "success") {
        chathistory <- chatdata$df_success
    } else {
        chathistory <- chatdata$df_fail
    }

    idcol <- colnames(chathistory)[1]
    chathistory <- chathistory[chathistory[[idcol]] == user_id, ]

    if (nrow(chathistory) == 0) {
        message(paste0("No chat history for user ", user_id))
        return(chathistory)
    }

    message(paste0("Chat history for user ", user_id, "\n--------------------"))
    for (i in 1:nrow(chathistory)) {
        role <- chathistory[i, ]$role
        role <- paste0(role, " (", chathistory[i,]$n_words, " words)")
        content <- chathistory[i,]$content
        if (grepl("system", role)) {
            cat(crayon::red(paste0(role, ": ", content, "\n\n")))
        } else if (grepl("ai", role) | grepl("assistant", role)) {
            cat(crayon::green(paste0(role, ": ", content, "\n\n")))
        } else {
            cat(paste0(role, ": ", content, "\n\n"))
        }
    }
    cat("--------------------\n")
    return(chathistory)
}
