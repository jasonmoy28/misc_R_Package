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
  data = data %>% select(!!!cols)

  cor_test_df = data %>%
    mutate(across(everything(),as.numeric)) %>%
    psych::corr.test()

  cor_df = as.data.frame(cor_test_df$r) %>%
    rownames_to_column() %>%
    mutate(across(where(is.numeric), ~ if_else(. == 1, NA_real_, .))) %>%
    mutate(across(where(is.numeric), ~ round(., 3)))

  sig_df = as_tibble(cor_test_df$p) %>%
    mutate(across(everything(), ~
                    case_when(. <= 0.001 ~ '***',
                              . <= 0.01 & . > 0.001 ~ '**',
                              . < 0.05 & . > 0.01 ~ '*',
                              TRUE ~ '')))

  for (i in c(1:ncol(sig_df))) {
    c_vec = str_c(cor_df[[i + 1]], sig_df[[i]])
    cor_df[[i + 1]] = c_vec
  }

  cor_df = cor_df %>% column_to_rownames()

  # printing warning meesage, non essential block
  coreced_name = NULL
  coreced_name = data %>% select(!is.numeric) %>% names(.)
  if (length(coreced_name) != 0) {
    warning_message = paste(paste(coreced_name, collapse = ', '),'were coreced into numeric')
    warning(warning_message)
  }
  return(cor_df)
}
