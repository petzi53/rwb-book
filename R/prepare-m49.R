

m49_recoded <- function(df) {
    df |>
        janitor::clean_names() |>
        dplyr::filter(country_or_area != "Antarctica") |>
        dplyr::mutate(intermediate_region_name =
                          base::ifelse(is.na(intermediate_region_name),
                                       sub_region_name, intermediate_region_name)
        ) |>
        dplyr::select(
            iso_alpha3_code,
            region_name,
            sub_region_name,
            intermediate_region_name
        ) |>
        dplyr::rename(
            iso = iso_alpha3_code,
        ) |>
        dplyr::rename_with(function(x) {gsub("_name","",x)})
}
