library(testthat)
library(chatlogr)
library(tibble)
library(readr)


test_that("chatlogr functions work", {

    dat <- tibble(x = 1:5, xy = 1:5, y = 1:5, z = 1:5)

    expect_length(get_matching_col_names(dat, c("x")), 2)
    expect_length(get_matching_col_names(dat, c("x", "x")), 2)
    expect_length(get_matching_col_names(dat, c("x", "y")), 3)
    expect_length(get_matching_col_names(dat, c("x", "y", "z")), 4)
    expect_length(get_matching_col_names(dat, "z"), 1)
    expect_length(get_matching_col_names(dat, c("z", "z", "x", "x")), 3)

    expect_s3_class(get_available_df_columns(dat, c("x")), "data.frame")
    expect_length(get_available_df_columns(dat, c("x")), 1)
    expect_length(get_available_df_columns(dat, c("x", "xy")), 2)
    expect_length(get_available_df_columns(dat, c("x", "y")), 2)
    expect_length(get_available_df_columns(dat, c("x", "y", "z")), 3)
    expect_length(get_available_df_columns(dat, c("x", "y", "z", "z")), 3)

})


test_that("works with actual data", {
    skip_if_not(check_if_file_exist())

    csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
    expect_s3_class(read_relevant_data_columns(csv_file, idcol = "vs", chat_col_patterns = "mpg", chat_cols = "cyl"), "data.frame")
    expect_length(read_relevant_data_columns(csv_file, idcol = "hp", chat_col_patterns = "mpg", chat_cols = "mpg"), 2)
    expect_length(read_relevant_data_columns(csv_file, idcol = "hp", chat_col_patterns = "mpg", chat_cols = "cyl"), 3)
    expect_length(read_relevant_data_columns(csv_file, idcol = "hp", chat_col_patterns = c("mpg"), chat_cols = c("cyl", "gear")), 4)
    expect_length(read_relevant_data_columns(csv_file, idcol = "hp", chat_col_patterns = c("mpg", "x"), chat_cols = c("cyl", "gear", "x")), 4)
    expect_length(read_relevant_data_columns(csv_file, idcol = "hp", chat_col_patterns = c("mpg", "x", "vs"), chat_cols = c("cyl", "gear", "x")), 5)
    expect_length(read_relevant_data_columns(csv_file, idcol = "hp", chat_col_patterns = c("mpg", "x", "vs"), chat_cols = c("cyl", "gear", "x", "gear")), 5)
    expect_length(read_relevant_data_columns(csv_file, idcol = "hp", chat_col_patterns = c("mpg", "x", "vs", "mpg"), chat_cols = c("cyl", "gear", "x", "gear")), 5)
    expect_error(read_relevant_data_columns(csv_file, idcol = c("a", "b", "c"), chat_col_patterns = "mpg", chat_cols = "cyl"))

    user_chat_data <- read_relevant_data_columns(csv_file, idcol = "vs", chat_col_patterns = "mpg", chat_cols = "cyl")[1, ]
    expect_type(parse_user_chat_data(user_chat_data, "vs", '\"\"'), "list")
    expect_match(parse_user_chat_data(user_chat_data, "vs", '\"\"')$status, "error")

    csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
    x <- read_csv(csv_file)
    expect_s3_class(read_relevant_data_columns(dat = x, idcol = "vs", chat_col_patterns = "mpg", chat_cols = "cyl"), "data.frame")
    expect_length(read_relevant_data_columns(dat = x, idcol = "hp", chat_col_patterns = "mpg", chat_cols = "mpg"), 2)
    expect_length(read_relevant_data_columns(dat = x, idcol = "hp", chat_col_patterns = "mpg", chat_cols = "cyl"), 3)
    expect_length(read_relevant_data_columns(dat = x, idcol = "hp", chat_col_patterns = c("mpg"), chat_cols = c("cyl", "gear")), 4)
    expect_length(read_relevant_data_columns(dat = x, idcol = "hp", chat_col_patterns = c("mpg", "x"), chat_cols = c("cyl", "gear", "x")), 4)
    expect_length(read_relevant_data_columns(dat = x, idcol = "hp", chat_col_patterns = c("mpg", "x", "vs"), chat_cols = c("cyl", "gear", "x")), 5)
    expect_length(read_relevant_data_columns(dat = x, idcol = "hp", chat_col_patterns = c("mpg", "x", "vs"), chat_cols = c("cyl", "gear", "x", "gear")), 5)
    expect_length(read_relevant_data_columns(dat = x, idcol = "hp", chat_col_patterns = c("mpg", "x", "vs", "mpg"), chat_cols = c("cyl", "gear", "x", "gear")), 5)
    expect_error(read_relevant_data_columns(dat = x, idcol = c("a", "b", "c"), chat_col_patterns = "mpg", chat_cols = "cyl"))

    dat <- read_relevant_data_columns(csv_file, idcol = "ResponseId", chat_col_patterns = "chathistory")

    # single single data
    user_chat_data <- dat[sample(1:100, 1), ]
    user_chat_data <- user_chat_data[-1]
    obj <- parse_user_chat_data(user_chat_data, "test_id")
    expect_s3_class(obj$messages, "data.frame")
    expect_true(grepl("error", obj$status) | grepl("success", obj$status))
    expect_true(obj$user_id == "test_id")
    expect_true(all(c("messages", "status", "user_id") %in% names(obj)))

    # multiple users' data
    obj <- parse_users_chat_data(csv_file, idcol = "user_id", nrows = 2)
    expect_type(obj, "list")
    expect_type(obj$info, 'integer')
    expect_s3_class(obj$info_df, "data.frame")
    expect_type(obj$json_list, "list")
    expect_s3_class(obj$df_success, "data.frame")
    expect_s3_class(obj$df_fail, "data.frame")

    expect_true(length(unique(obj$df_success$user_id)) + length(unique(obj$df_fail$user_id)) ==
                    length(unique(obj$info_df$user_id)))

    expect_true(length(unique(obj$df_success$user_id)) == obj$info['success'])
    expect_true(length(unique(obj$df_fail$user_id)) == obj$info['total'] - obj$info['success'])

    # view chat data
    chatdata <- parse_users_chat_data(csv_file, idcol = 'user_id', nrows = sample(5:20, 1), verbose = F)

    expect_true(length(get_success_ids(chatdata)) >= 0)
    expect_true(length(get_failed_ids(chatdata)) >= 0)
    expect_error(get_user_chat(chatdata, "dsfdsfa"))

    expect_error(write_to_json(mtcars, "mtcars"))
    expect_error(write_to_csv(mtcars, "mtcars"))

    # id column must be unique
    csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
    expect_error(parse_users_chat_data(csv_file, idcol = "vs"))
    expect_no_error(parse_users_chat_data(csv_file, idcol = "user_id"))

    # chat columns must be character
    expect_error(parse_users_chat_data(csv_file, idcol = "user_id", chat_cols = c("gear")))

    # NA columns don't throw error
    csv_file <- file.path(system.file("extdata", package = "chatlogr"), "qualtrics01.csv")
    expect_no_error(parse_users_chat_data(csv_file, idcol = "ResponseId", nrows = 5))

    # only accept input type
    csv_file <- file.path(system.file("extdata", package = "chatlogr"), "mtcars.csv")
    expect_error(parse_users_chat_data(csv_file, mtcars))
    expect_no_error(parse_users_chat_data(dat = read_csv(csv_file), idcol = "user_id"))

    # view chat data
    csv_file <- file.path(system.file("extdata", package = "chatlogr"), "qualtrics01.csv")
    chatdata <- parse_users_chat_data(csv_file, idcol = 'ResponseId', verbose = F)
    chathistory <- chatdata$df_success
    expect_no_error(get_user_chat(chathistory, "R_6CTNF27UPvt8oKX"))

    # get df
    expect_no_error(get_df(chatdata))

})
