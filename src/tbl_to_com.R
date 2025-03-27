tbl_to_comm <- function(df, site, species, cover) {
  
  names <- df %>% dplyr::distinct(!!dplyr::enquo(site))
  na_col <- colnames(names)[1]
  df <- df %>%
    dplyr::select(!!dplyr::enquo(site),
                  !!dplyr::enquo(species),
                  !!dplyr::enquo(cover) ) %>%
    tidyr::pivot_wider(names_from = !!dplyr::enquo(species),
                       values_from = !!dplyr::enquo(cover))
  
  df <- names %>%
    dplyr::left_join(df, by = na_col[1]) %>%
    dplyr::select(-1) %>%
    dplyr::mutate_all(list(~tidyr::replace_na(., 0))) %>%
    as.data.frame()
  row.names(df) <- purrr::as_vector(names)
  return(df)
}