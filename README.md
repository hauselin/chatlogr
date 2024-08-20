
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
# datafile is "data.csv"
# unique id column is ResponseId
# chat history columns contain "chathistory"
# parse 10 (random) rows
chatdata <- parse_users_chat_data(
    "data.csv", 
    idcol = "ResponseId", 
    chat_col_patterns = c("chathistory"), 
    nrows = 10
    )
# Sampling 10 rows...                                                                      
# Processing 10 rows...
# Parsing summary/status
#    total  success   errors warnings 
#       10        7        3        0 

# get successfully parsed userids
get_success_ids(chatdata)

# get failed userids
get_failed_ids(chatdata)

# get chat history for a specific user
get_user_chat(chatdata, "R_7PQRDkwCKlhLGhP")
# prints chat history for user with id "R_7PQRDkwCKlhLGhP"
# returns a tibble dataframe for this user
```

## Parse with `parse_users_chat_data()`

Parse a raw CSV file downloaded from Qualtrics with the
`parse_users_chat_data()` function.

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
#  3 R_6CTNF27UPvt8oKX assistant "It’s great that you’re thinking …      1.99     2     477
#  4 R_6CTNF27UPvt8oKX user      "thank you for explaining"             42.9      3       4

# tibble dataframe with failed parsing
chatdata$df_fail 
# # A tibble: 28 × 6
#    ResponseId                     role  content                   createdAt    id n_words
#    <chr>                          <chr> <chr>                         <dbl> <int>   <int>
#  1 "Response ID"                  NA    "Chat history will be sa…        NA    NA     315
#  2 "{\"ImportId\":\"_recordId\"}" NA    "{\"ImportId\":\"QID84_T…        NA    NA      10
#  3 "R_6Oxfp8hPdB4oOJQ"            NA    ""                               NA    NA       0
```

### Parsing parameters

The `parse_users_chat_data()` function has several parameters that can
be adjusted to suit the specific format of the chat data.

``` r

parse_users_chat_data(
    csv_file,  # path to CSV file
    idcol = "your_unique_ID_column", 
    chat_col_patterns = c("chathistory"), # get columns with names containing this pattern
    chat_cols = c("chathistory00", "chathistory01"),  # specify exact column names to parse
    nrows = 10  # no. of rows to read (for debugging)
)
```

## Saving data

``` r
# save the full chatdata object to a JSON file
# chatdata is the list object returned by parse_users_chat_data()
# save the FULL chatdata object to a JSON file
write_to_json(chatdata, "chatdata.json")

# save the successfully parsed chat history to a CSV file
# chatdata is the list object returned by parse_users_chat_data()
# saves only the chat history data to a CSV file
write_to_csv(chatdata, "chathistory.csv")
```