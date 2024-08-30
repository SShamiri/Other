library(tidyverse)

path_file1 <- "C:\\Users\\ss3898\\OneDrive - Corporate Network\\Documents\\Search and match models\\02_data\\6354004_vacancy_industry.xlsx" # has Index
path_file2 <- "C:\\Users\\ss3898\\Downloads\\6291007.xlsx" # has Index

path_file3 <- "C:\\Users\\ss3898\\Downloads\\MRM1.xlsx" # no Index
path_file4 <- "C:\\Users\\ss3898\\Downloads\\EQ02.xlsx" # no Index

# solving for different sheets 
# Not all abs files has Index sheet
file <- path_file1

sheets <- readxl::excel_sheets(path = file)

if('Index' %in% sheets) {
  sheets <- sheets[!sheets %in% c("Inquiries", "Enquiries", "Contents", "Explanatory notes")]
  sheets <- sheets[!grepl("Table ", sheets, ignore.case = T)]
  # get table title
  tbl_title <- readxl::read_excel(file,
                                  range = "Index!B6:B6",
                                  col_names = "tbl_title"
  )
  # get data
  data_sheets <- purrr::map(
    .x = sheets[!sheets %in% 'Index'],
    .f = readxl::read_excel,
    path = file,
    trim_ws = TRUE,
    .name_repair = "minimal"
  )
  
  data_sheets <- map2(data_sheets, tbl_title, ~cbind(.x, tbl_title = .y))
  # sheet_names <- rep("data", length(data_sheets))
  # names(data_sheets) <- sheet_names
  #add title to data
  
  # data_sheets <- data_sheets %>%
  #   purrr::set_names(paste0(
  #     tools::file_path_sans_ext(basename(file)), "=",
  #     sheets[!sheets %in% 'Index'], "=",
  #     tbl_title
  #   ))
  
}

if('Contents' %in% sheets) {
  sheets <- sheets[!sheets %in%  c("Index", "Inquiries", "Enquiries", "Contents", "Explanatory notes")]
  # get table title
  tbl_title <- purrr::map(
    .x = sheets,
    .f = readxl::read_excel,
    path = file,
    trim_ws = TRUE,
    .name_repair = "minimal",
    range = "A4:A4",
    col_names = "tbl_title"
  )
  # get data
  data_sheets <- purrr::map(
    .x = sheets,
    .f = readxl::read_excel,
    path = file,
    trim_ws = TRUE,
    .name_repair = "minimal",
    skip = 4
  )
  data_sheets <- map2(data_sheets, tbl_title, ~cbind(.x, tbl_title = .y))
  
}

# tidy data
df <- data_sheets[[1]]
colnames(df)[1] <- "X__1"
if (df[9, 1] != "Series ID") {
  stop(
    "The data frame appears not to be formatted as we expect",
    " from an ABS time series. There should be 9 rows of metadata",
    " after the column names (eg. 'Series ID').",
    " Please check the spreadsheet."
  )
}
df

cols <- 2:ncol(df)-1
new_col_names <- paste(colnames(df)[cols], df[9, cols], sep = "_")

colnames(df)[2:ncol(df)] <- c(new_col_names,"tbl_name")

# metadata
df %>%
  tidyr::pivot_longer(cols = !one_of("X__1"), names_to = "series") %>%
  filter(
    !is.na(X__1),
    # This filtering is necessary for cases where the ABS adds notes
    # to the bottom of the data for some reason
    !stringi::stri_detect_fixed(X__1, "Trend Break"),
    !stringi::stri_detect_fixed(X__1, "see")
  ) %>%
  dplyr::group_by(series) %>%
  dplyr::mutate(
    series_type = value[X__1 == "Series Type"],
    data_type = value[X__1 == "Data Type"],
    collection_month = value[X__1 == "Collection Month"],
    frequency = value[X__1 == "Frequency"],
    series_id = value[X__1 == "Series ID"],
    unit = value[X__1 == "Unit"]
  ) %>%
  dplyr::filter(dplyr::row_number() >= 10) %>%
  dplyr::ungroup() |> 
  dplyr::rename(date = X__1) %>%
  dplyr::mutate(
    date = suppressWarnings(as.numeric(date)),
    date = as.Date(date, origin = "1899-12-30"),
    unit = as.character(unit),
    value = as.numeric(value)
  ) %>%
  dplyr::filter(!is.na(date))





####### function ---------------------------
clean_abs_raw <- function(file, metadata = TRUE) {
  # Error catching
  if (is.null(file)) {
    stop("You must specify file path.")
  }
  # if (!is.logical(metadata) || length(metadata) != 1L || is.na(metadata)) {
  #   stop("`metadata` argument must be either TRUE or FALSE")
  # }
  #filenames <- list.files(path = file, pattern = "\\.(xls|xlsx)$")
  
  # if (length(filenames) == 0) {
  #   stop(paste0("Could not find any .xls or .xlsx files in path: '", file, "'"))
  # }
  file_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
  
  # first extract table titles from the time series spreadsheet index
  tbl_title <-unlist(purrr::map(file, get_tbl_title))
  # then read in the data
  sheets <- purrr::map2(file, tbl_title,
                        .f = get_sheets
  )
  # remove one 'layer' of the list,
  # so that each sheet is its own element in the list
  sheets <- unlist(sheets, recursive = FALSE)
  # tidy the sheets
  sheet <- tidy_abs_list(sheets, metadata = metadata)
  
 return( sheet) 
  
}

## 
get_tbl_title <- function(file) {
  tbl_title <- readxl::read_excel(file,
                                   range = "Index!B6:B6",
                                   col_names = "tbl_title"
  )
  
  tbl_title <- as.character(tbl_title)
  
  return(tbl_title)
}

get_sheets <- function(file,
                        tbl_title = NULL
                               ) {
  
  sheets <- readxl::excel_sheets(path = file)
  sheets <- sheets[!sheets %in% c("Index", "Inquiries", 
                                  "Enquiries", "Contents"
                                  )]
  sheets <- sheets[!grepl("Table ", sheets, ignore.case = T)]
  
  if (length(sheets) < 1) {
    stop(sprintf(
      "The Excel workbook %s appears to have no data sheets.",
      file
    ))
  }
  
  data_sheets <- purrr::map(
    .x = sheets,
    .f = readxl::read_excel,
    path = file,
    trim_ws = TRUE,
    .name_repair = "minimal"
  )
  
  data_sheets <- data_sheets %>%
    purrr::set_names(paste0(
      tools::file_path_sans_ext(basename(file)), "=",
      sheets, "=",
      tbl_title
    ))
  
  data_sheets
}

tidy_abs_list <- function(list_of_dfs, metadata = TRUE) {
  table_no <- sheet_no <- table_title <- NULL
  
  if (!"list" %in% class(list_of_dfs)) {
    if ("data.frame" %in% class(list_of_dfs)) {
      stop(
        "abs_tidy_list() works with lists of data frames,",
        " not individual data frames. Use abs_tidy() to tidy an",
        " individual data frame that is not contained in a list."
      )
    }
    stop("The object is not a list of data frames.")
  }
  
  message("Tidying data from imported ABS spreadsheets")
  
  # apply abs_tidy to each element of list_of_dfs and combine into 1 df
  # with new column "sheet_id"
  x <- purrr::map(list_of_dfs,
                  tidy_abs,
                  metadata = metadata
  )
  
  sheet_ids <- names(list_of_dfs)
  
  list_of_split_sheet_ids <- stringi::stri_split_fixed(sheet_ids, "=", 3)
  
  for (i in seq_along(x)) {
    x[[i]]$table_no <- list_of_split_sheet_ids[[i]][1]
    x[[i]]$sheet_no <- list_of_split_sheet_ids[[i]][2]
    x[[i]]$table_title <- list_of_split_sheet_ids[[i]][3]
  }
  
  x <- bind_rows(x)
  
  x <- select(x, table_no, sheet_no, table_title, everything())
  
  x
}

tidy_abs <- function(df, metadata = TRUE) {
  unit <- series <- value <- X__1 <- series_id <- NULL
  
  if (!"data.frame" %in% class(df)) {
    stop("Object does not appear to be a data frame; it cannot be tidied.")
  }
  
  # return an error if the df has <= 1 column
  if (ncol(df) <= 1) {
    stop(
      "The data frame appears to have fewer than 2 columns.",
      " This is unexpected from an ABS time series.",
      " Please check the spreadsheet."
    )
  }
  
  if (!is.logical(metadata)) {
    stop("`metadata` argument to tidy_abs() must be either `TRUE` or `FALSE`.")
  }
  
  # newer versions of tibble() and readxl() want to either leave the first
  # colname blank or coerce it to something other than X__1
  # so we set it manually
  colnames(df)[1] <- "X__1"
  
  # return an error if the sheet is not formatted as we expect
  # from an ABS time series
  
  if (df[9, 1] != "Series ID") {
    stop(
      "The data frame appears not to be formatted as we expect",
      " from an ABS time series. There should be 9 rows of metadata",
      " after the column names (eg. 'Series ID').",
      " Please check the spreadsheet."
    )
  }
  
  # a bunch of columns have duplicate names which causes problems; temporarily
  # append the series ID to the colname so they're unique
  
  # new_col_names <- purrr::map_chr(
  #   .x = 2:ncol(df),
  #   .f = ~ paste0(colnames(df)[.], "_", df[9, .])
  # )
  
  cols <- 2:ncol(df)
  new_col_names <- paste(colnames(df)[cols], df[9, cols], sep = "_")
  
  colnames(df)[2:ncol(df)] <- new_col_names
  
  if (isTRUE(metadata)) {
    df <- df %>%
      tidyr::pivot_longer(cols = !one_of("X__1"), names_to = "series") %>%
      filter(
        !is.na(X__1),
        # This filtering is necessary for cases where the ABS adds notes
        # to the bottom of the data for some reason
        !stringi::stri_detect_fixed(X__1, "Trend Break"),
        !stringi::stri_detect_fixed(X__1, "see")
      ) %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(
        series_type = value[X__1 == "Series Type"],
        data_type = value[X__1 == "Data Type"],
        collection_month = value[X__1 == "Collection Month"],
        frequency = value[X__1 == "Frequency"],
        series_id = value[X__1 == "Series ID"],
        unit = value[X__1 == "Unit"]
      ) %>%
      dplyr::filter(dplyr::row_number() >= 10) %>%
      dplyr::ungroup()
    
    df <- df %>%
      dplyr::rename(date = X__1) %>%
      dplyr::mutate(
        date = suppressWarnings(as.numeric(date)),
        date = as.Date(date, origin = "1899-12-30"),
        unit = as.character(unit),
        value = as.numeric(value)
      ) %>%
      dplyr::filter(!is.na(date))
    
    
    df <- df %>%
      # now remove appended series ID from the end of 'series'
      dplyr::mutate(series = stringi::stri_replace_all_regex(series,
                                                             pattern = "_[^_]+$",
                                                             replacement = ""
      ))
  }
  
  if (metadata == FALSE) {
    colnames(df) <- df[9, ]
    df <- df[-c(1:9), ]
    colnames(df)[1] <- "date"
    
    df <- df %>%
      tidyr::gather(
        key = series_id,
        value = value,
        -date
      ) %>%
      dplyr::mutate(
        date = as.Date(as.numeric(date), origin = "1899-12-30"),
        value = as.numeric(value)
      )
  }
  
  df
}