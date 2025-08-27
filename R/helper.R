##########################################################
# Project: Some utility scripts for my Quarto books
# Author: Peter Baumgartner
# August 27, 2025
#
# CONTENT:
## - my_save_data_file: save data file
## - my_get_dir_files
## - my_pkgs_dl: compare package downloads
## - my_df_recoded: recode score values of RWB datasets
## - my_rwb_rec: call RWB score columns to recode their values
##########################################################



library(glossary)

glossary::glossary_path("../glossary-pb/glossary.yml")

################################################################
# my_save_data_file: Save data file for the specified chapter
# Version 1.0: June 8, 2025
# Purpose:
# If folder not exists, create it and save object as .rds file
# Author: Peter Baumgartner
# chapter_folder = character: folder inside "data" folder
#                  example "chap05"
# object = data to save
# file_name = character: example "xy_object.rds"
# I have used the function in my notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################

my_save_data_file <- function(chapter_folder, object, file_name){
    data_folder <- base::paste0(here::here(), "/data/")
    if (!base::file.exists(data_folder))
    {base::dir.create(data_folder)}

    chap_folder <-
        base::paste0(
            here::here(),
            paste0("/data/", chapter_folder, "/")
        )
    if (!base::file.exists(chap_folder))
    {base::dir.create(chap_folder)}

    base::saveRDS(object = object,
                  file = paste0(chap_folder, "/", file_name))
}

################################################################
# my_pkgs_dl: Get number of downloads from RStudio CRAN Mirror
# Version 1.0: June 8, 2025
# Purpose:
# Compare popularity of different packages
# Author: Peter Baumgartner
# pkgs = character vector of package names
# period = "last-day" "last-week", "last-month"
# days: period days = 1, 7, 30
# returns: tibble with packages sorted by download figures
# I have used the function in my notes on "Statistics with R"
# # See: https://bookdown.org/pbaumgartner/swr-harris/
################################################################
my_pkgs_dl <-  function(pkgs, period = "last-week", days = 7) {
  dl_pkgs <- cranlogs::cran_downloads(when = period, packages = pkgs)

  start_date = base::min(dl_pkgs$date)
  end_date = base::max(dl_pkgs$date)

  dl_pkgs |>
    dplyr::group_by(package) |>
    dplyr::summarize(average = trunc(sum(count) / days)) |>
    dplyr::arrange(desc(average)) |>
    dplyr::mutate(
      from = start_date,
      to = end_date
    )
}


##### my_get_dir_files    #######################################
# my_get_dir_files: Get all files  from a directory
# August 27, 2025
# Purpose:
# Load all files of a specified path
#         with a specified  file extension into memory
# Author: Peter Baumgartner
# path = character string, starting from here::here()
# pattern = regex expression, for instance "\\.rds$"
################################################################

my_get_dir_files <-  function(path, pattern){
  get_file_names <- list.files(
    path = path,
    pattern = pattern,
    full.names = TRUE
  )
  for (i in seq_along(get_file_names)) {
    file_name_with_ext <- basename(get_file_names[i])   # remove path to get base name
    file_name <- gsub(pattern, "", file_name_with_ext) # remove the .rds extension
    assign(                                              # assign dataset to name in environment
      file_name,
      readRDS(paste0(here::here(), "/", get_file_names[i])),
      envir = parent.frame()
    )
  }
}



################################################################
# my_df_recoded: recode score values of RWB datasets
# August 27, 2025
# Purpose:
# Internal function, called by  my_rwb_rec()
# Clean score values of a specified  column
# Author: Peter Baumgartner
# df = data frame as R object
# col_name: name of score column to clean
################################################################

my_df_recoded <- function(df, col_name) {
  df |>
    dplyr::mutate(
      "{{col_name}}" := dplyr::if_else({{col_name}} < 100, {{col_name}} * 100, {{col_name}}),
      "{{col_name}}" := dplyr::if_else({{col_name}} < 1000, {{col_name}} * 10, {{col_name}}),
      "{{col_name}}" := dplyr::if_else({{col_name}} > 10000, round({{col_name}} / 10), {{col_name}}),
      "{{col_name}}" := {{col_name}} / 100
    )
}



################################################################
# my_rwb_rec: specify score column to clean
#     and load dataset into memory
# August 27, 2025
# Purpose:
# Specify column names and call my_df_recoded
#     with  df and  column name
# Author: Peter Baumgartner
# df = data frame as R object
# col_name: name of score column to clean as R object
################################################################

my_rwb_rec <- function(df) {
  for (i in seq_along(df)) {
    df1 <- df |>
      my_df_recoded(score) |>
      my_df_recoded(score_n_1) |>
      dplyr::mutate(score_evolution = score - score_n_1)
    if ("political_context" %in% names(df1)) {
      df1 <- df1 |>
        my_df_recoded(political_context) |>
        my_df_recoded(economic_context) |>
        my_df_recoded(legal_context) |>
        my_df_recoded(social_context) |>
        my_df_recoded(safety)
    }
    file_name = paste0("rwb", df1$year_n[1])
    assign(file_name, df1, envir = globalenv())
  }
}



## END

