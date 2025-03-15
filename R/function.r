
#' Send query to mmex database and get the result
#'
#' @param sql_query  SQL query
#' @param db_path db file path
#'
#' @return tibble from SQL query
query_db <- function(sql_query, db_path){
    on.exit(RSQLite::dbDisconnect(con))

    con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
    df <- RSQLite::dbGetQuery(con, sql_query) |>
        tibble::as_tibble() |>
        janitor::clean_names()
    df
}


#' List all tables in mmex db.
#'
#' @param db_path db file path
#'
#' @return character vector
#' @export
#'
list_table <- function(db_path){
    on.exit(RSQLite::dbDisconnect(con))

    con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
    RSQLite::dbListTables(con)
}


#' Get a specific table from MMEX db using `query_db()`
#'
#' To find the list of tables, use `list_table(db_path)`
#' The name assumes that pipe is used 'table_name' |> read_table_from(db_path)
#'
#' @param tbl_name table within mmex db
#' @param db_path path to mmex db file
#'
#' @export
#'
#' @return tibble
read_table_from <- function(tbl_name, db_path){
    if(!file.exists(db_path)){
        msg <- stringr::str_glue("Specified database does not exist.:\n {db_path}")
        stop(msg)
    }
    stringr::str_glue("select * from {tbl_name}") |> query_db(db_path)
}

#' Read category as a df
#'
#' This will run recursive query to get multi-level categories
#'
#' @param db_path path to mmex db file
#'
#' @returns tibble with category information
#' @export
read_category_from <- function(db_path){
    # https://github.com/moneymanagerex/general-reports/tree/master/packages/Category/OneCategoryList
    "
    WITH RECURSIVE categories(categid, categname, parentid) AS
    (SELECT a.categid, a.categname, a.parentid FROM category_v1 a WHERE parentid = '-1'
    UNION ALL
    SELECT c.categid, r.categname || ':' || c.categname, c.parentid
    FROM categories r, category_v1 c
    WHERE r.categid = c.parentid)
    SELECT categid, categname FROM categories ORDER by categname;
    " -> query_category

    if(!file.exists(db_path)){
        msg <- stringr::str_glue("Specified database does not exist.:\n {db_path}")
        stop(msg)
    }
    df <- query_category |> query_db(db_path)
    df

}


#' Check whether the format is valid with only 0 or 1 ':'
#'
#' Should be either 'category' or 'category:subcategory'
#'
#' @param string_vec string of categories
#'
#' @returns logical
is_category_valid <- function(string_vec){
    all(str_count(string_vec, ':') %in% c(0, 1))
}


#' Extracts category
#'
#' @param string_vec string of categories
#'
#' @returns string vector of category. Should not have any NA
get_category <- function(string_vec){
    if(!is_category_valid(string_vec)){
        stop("Category should have at most 2 levels")
    }
    string_vec |> str_split(':') |> map_chr(1)
}


#' Extracts subcategory
#'
#' @param string_vec string of categories
#'
#' @returns string vector of subcategory. Returns NA_character_ if missing
get_subcategory <- function(string_vec){
    if(!is_category_valid(string_vec)){
        stop("Category should have at most 2 levels")
    }

    if_null_to_na <- \(x){
        if(is.null(x)){NA_character_} else {x}
    }

    result <- string_vec |>
        str_split(':') |>
        map(2) |>
        # need to convert NULL -> NA
        map_chr(if_null_to_na)
    result
}


#' Read *selected* tables from MMEX database and combine it as a tibble.
#'
#' Note that this does not join all tables in the db.
#' It only joins
#' - checkingaccount
#' - accountlist
#' - category
#' - payee
#' Also this does *not* process for my preference, which will be done 'read_mmex_db'
#'
#' @param db_path path to MMEX database
#'
#' @return tibble
#' @export
build_df <- function(db_path){

    if(!file.exists(db_path)){
        msg <- stringr::str_glue("Specified database does not exist.:\n {db_path}")
        stop(msg)
    }

    # read data
    df_tran <- "checkingaccount_v1" |> read_table_from(db_path)
    df_account <- "accountlist_v1" |> read_table_from(db_path)
    df_category <- read_category_from(db_path)
    df_payee <- "payee_v1" |> read_table_from(db_path)

    df_account1 <-
        df_account |>
        dplyr::select(accountid, accountname, accounttype, initialbal)

    df_account_name <-
        df_account1 |>
        dplyr::select(accountid, accountname) |>
        dplyr::rename(toaccountid = accountid) |>
        dplyr::rename(toaccountname = accountname)

    # combine
    df <-
        df_tran |>
        dplyr::mutate(transdate = lubridate::ymd_hms(transdate) |> lubridate::as_date()) |>
        dplyr::left_join(df_account1, by = "accountid") |>
        dplyr::left_join(df_account_name, by = 'toaccountid') |>
        dplyr::left_join(df_payee |> dplyr::select(payeeid, payeename), by = "payeeid") |>
        dplyr::left_join(df_category, by = "categid")
    df

}


#' Read selected tables from MMEX database and combine it as a tibble.
#'
#' Note that this does not join all tables in the db.
#' It only joins
#' - checkingaccount
#' - accountlist
#' - category
#' - payee
#'
#' @param db_path path to MMEX database
#'
#' @return tibble
#' @export
read_mmex_db <- function(db_path){

    if(!file.exists(db_path)){
        msg <- stringr::str_glue("Specified database does not exist.:\n {db_path}")
        stop(msg)
    }

    # build
    df <- build_df(db_path)

    # process
    dfm <-
        df |>
        # process for my usual analysis
        # change signs for "Deposit" to +
        dplyr::mutate(transamount = ifelse(transcode == "Deposit", transamount, -transamount)) |>
        # match the format that I used in my analysis scripts
        dplyr::rename(cat_name = categname) |>
        # assume that cat_name has only format like: cat, cat:subcat
        # I use only 1 or 2 level categories
        dplyr::mutate(categname = cat_name |> get_category()) |>
        dplyr::mutate(subcategname = cat_name |> get_subcategory())
    dfm
}

