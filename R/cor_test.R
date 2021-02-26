#' Recode
#'
#' @param data a dataframe
#' @param cols vector or quos(). column(s) that need to be recoded
#'
#' @return
#' @export
#'
#' @examples
#'

cor_test =  function(data, cols) {
  data = data %>% dplyr::select(!!!cols)

  cor_test_df = data %>%
    dplyr::mutate(dplyr::across(tidyr::everything(),as.numeric)) %>%
    psych::corr.test()

  cor_df = as.data.frame(cor_test_df$r) %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::if_else(. == 1, NA_real_, .))) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3)))

  sig_df = tidyr::as_tibble(cor_test_df$p) %>%
    dplyr::mutate(dplyr::across(tidyr::everything(), ~
                           dplyr::case_when(. <= 0.001 ~ '***',
                                            . <= 0.01 & . > 0.001 ~ '**',
                                            . < 0.05 & . > 0.01 ~ '*',
                                            TRUE ~ '')))

  for (i in c(1:ncol(sig_df))) {
    c_vec = stringr::str_c(cor_df[[i + 1]], sig_df[[i]])
    cor_df[[i + 1]] = c_vec
  }

  cor_df = cor_df %>% tibble::column_to_rownames()

  # printing warning meesage, non essential block
  coreced_name = NULL
  coreced_name = data %>% dplyr::select(!is.numeric) %>% names(.)
  if (length(coreced_name) != 0) {
    warning_message = paste(paste(coreced_name, collapse = ', '),'were coreced into numeric')
    warning(warning_message)
  }
  return(cor_df)
}
