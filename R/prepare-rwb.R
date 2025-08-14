
library(dplyr, warn.conflicts = FALSE)
base::source(paste0(here::here(), "/R/prepare-m49.R"))

rwb_new <- function(df) {
    df |>
        janitor::clean_names() |>
        rename_with(function(x){gsub("_context","", x)}) |>
        left_join(m49_recoded(m49), "iso") |>
        select(-c(country_fr, country_es:country_fa)) |>
        relocate(year = year_n) |>
        df_recoded_new(score) |>
        df_recoded_new(political) |>
        df_recoded_new(economic) |>
        df_recoded_new(legal) |>
        df_recoded_new(social) |>
        df_recoded_new(safety)
}


df_recoded_new <- function(df, col_name) {
    df |>
        mutate(
            "{{col_name}}" := if_else({{col_name}} < 100, {{col_name}} * 100, {{col_name}}),
            "{{col_name}}" := if_else({{col_name}} < 1000, {{col_name}} * 10, {{col_name}}),
            "{{col_name}}" := if_else({{col_name}} > 10000, round({{col_name}} / 10), {{col_name}}),
            "{{col_name}}" := {{col_name}} / 100,
            "{{col_name}}_situation" := case_when(
                {{col_name}} >= 85 ~ "1. Good",
                {{col_name}} >= 70 & {{col_name}} <= 84.99 ~ "2. Rather Good",
                {{col_name}} >= 55 & {{col_name}} <= 69.99 ~ "3. Problematic",
                {{col_name}} >= 40 & {{col_name}} <= 54.99 ~ "4. Difficult",
                {{col_name}} <= 39.99 ~ "5. Very Serious"
            )
        ) |>
        relocate(last_col(), .after = {{col_name}})
}

rwb_old <- function(df) {
    df |>
        janitor::clean_names() |>
        rename_with(function(x){gsub("_context","", x)}) |>
        left_join(m49_recoded(m49), "iso") |>
        select(-c(fr_country, es_country:fa_country)) |>
        relocate(year = year_n) |>
        df_recoded_old(score_n) |>
        rename(country_en = en_country)
}

df_recoded_old <- function(df, col_name) {
    df |>
        mutate(
            "{{col_name}}" := if_else({{col_name}} < 100, {{col_name}} * 100, {{col_name}}),
            "{{col_name}}" := if_else({{col_name}} < 1000, {{col_name}} * 10, {{col_name}}),
            "{{col_name}}" := if_else({{col_name}} > 10000, round({{col_name}} / 10), {{col_name}}),
            "{{col_name}}" := {{col_name}} / 100,
            "{{col_name}}_situation" := case_when(
                {{col_name}} >= 85 ~ "1. Good",
                {{col_name}} >= 75 & {{col_name}} <= 84.99 ~ "2. Rather Good",
                {{col_name}} >= 65 & {{col_name}} <= 74.99 ~ "3. Problematic",
                {{col_name}} >= 45 & {{col_name}} <= 64.99 ~ "4. Difficult",
                {{col_name}} <= 44.99 ~ "5. Very Serious"
            )
        ) |>
        relocate(last_col(), .after = {{col_name}})
}
