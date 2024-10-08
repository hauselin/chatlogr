
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chatlogr

<!-- badges: start -->

[![R-CMD-check](https://github.com/hauselin/chatlogr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hauselin/chatlogr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`chatlogr` helps your parse data from chatbot data. The main function is
[`parse_users_chat_data()`](https://hauselin.github.io/chatlogr/reference/parse_users_chat_data.html)
which parses chat history data from a CSV file. See
[documentation](https://hauselin.github.io/chatlogr/) for details.

## Installation

Install the development version of `chatlogr`. You might need to install
the `remotes` package first.

``` r
install.packages("remotes")  # if remotes package not already installed
remotes::install_github("hauselin/chatlogr")
```

## Usage

``` r
library(chatlogr)  # load library

# parse data
chatdata <- parse_users_chat_data(
    csv_file = "data.csv",  # data file (or provide a dataframe via dat parameter)
    idcol = "ResponseId",  # unique id column in data
    chat_col_patterns = c("chathistory"),  # columns containing chat history
    nrows = 10  # number of rows to parse (fewer for debugging/testing)
    )
# Sampling 10 rows...                                                                      
# Processing 10 rows...
# Parsing summary/status
#    total  success   errors warnings 
#       10        7        3        0 

# more info about parsing status
chatdata$info
chatdata$info_df

# get successfully parsed chat messages/history
chatdata$df_success

# get rows that cannot be parsed
chatdata$df_fail

# get full json object
# contains all data (including chat data like thumbs up/down, highlights, user-agent info)
chatdata$json

# get long-form chat history (success and failed) for all users
get_df(chatdata)

# get successfully parsed userids
get_success_ids(chatdata)

# get failed userids
get_failed_ids(chatdata)

# get/view chat history for a specific a userid
get_user_chat(chatdata, "R_13GYa78PPEossy5")  # accepts chatdata from parse_users_chat_data
get_user_chat(chatdata$df_success, "R_13GYa78PPEossy5")  # accepts a dataframe
# prints chat history for that userid
# returns a tibble dataframe for that userid
```

## Parse with `parse_users_chat_data()`

The `parse_users_chat_data()` function has several parameters that can
be adjusted to suit the specific format of the chat data.

``` r
parse_users_chat_data(
    csv_file,  # path to CSV file
    idcol = "your_unique_ID_column", 
    chat_col_patterns = c("chathistory"), # get columns with names containing this pattern
    chat_cols = c("chathistory00", "chathistory01"),  # specify exact column names to parse
    nrows = 10  # rows to read (for debugging/testing initially)
)
```

You can read/parse a csv file or a processed dataframe that is already
in R.

``` r
chatdata <- parse_users_chat_data(csv_file = "file.csv")
chatdata <- parse_users_chat_data(dat = some_data_frame)
```

If you run into issues, try parsing the raw CSV file (instead of a
processed data/file) with the `csv_file` parameter.

``` r
csv_file <- "qualtrics_data.csv"
# see doc for default parameter values
chatdata <- parse_users_chat_data(csv_file) 
# returns a list
# Processing 150 rows...                                                                   
# Parsing summary/status
#    total  success   errors warnings 
#      150      122       28        0 

names(chatdata)
# keys in the list
# [1] "info"       "info_df"    "json_list"  "df_success" "df_fail"   

# summary of parsing results
chatdata$info
# total  success   errors warnings 
#   150      122       28        0 

chatdata$info_df
# tibble dataframe with status column indicating parsing success or failure
# mobile column indicates mobile (1) or not ()
# A tibble: 150 × 3
#   ResponseId                     status                       mobile
#   <chr>                          <chr>                         <dbl>
# 1 "Response ID"                  error: json validation error      0
# 2 "{\"ImportId\":\"_recordId\"}" error: json validation error      0
# ...

# large list object containing parsed JSON data
chatdata$json_list  

# tibble dataframe with successfully parsed chat history
chatdata$df_success  
# A tibble: 1,103 × 6
#    ResponseId        role      content                            createdAt    id n_words
#    <chr>             <chr>     <chr>                                  <dbl> <int>   <int>
#  1 R_6CTNF27UPvt8oKX system    "You are an engaging and persuasi…      0        0     344
#  2 R_6CTNF27UPvt8oKX user      "The issue of 'Role of federal go…      0.1      1      95

# tibble dataframe with failed parsing
chatdata$df_fail 
# # A tibble: 28 × 6
#    ResponseId                     role  content                   createdAt    id n_words
#    <chr>                          <chr> <chr>                         <dbl> <int>   <int>
#  1 "Response ID"                  NA    "Chat history will be sa…        NA    NA     315
#  2 "{\"ImportId\":\"_recordId\"}" NA    "{\"ImportId\":\"QID84_T…        NA    NA      10
```

## Saving and reading data

``` r
# chatdata is the list returned by parse_users_chat_data()
# save the FULL chatdata object (with extra parameters) to JSON file
write_to_json(chatdata, "chatdata.json")

# read the JSON file back into R
chatdata <- read_json_file("chatdata.json")

# chatdata is the list returned by parse_users_chat_data()
# saves ONLY CHAT MESSAGES/HISTORY to CSV file
write_to_csv(chatdata, "chathistory.csv")
```
