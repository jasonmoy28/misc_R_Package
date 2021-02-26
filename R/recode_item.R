#' Recode
#'
#' @param data a dataframe
#' @param cols vector or quos(). column(s) that need to be recoded
#' @param code_from vector. the order must match with vector for code_to
#' @param code_to vector. the order must match with vector for code_from
#'
#' @return
#' @export
#'
#' @examples
#'
recode_item <- function(data,
                        cols,
                        reverse_code = F,
                        code_as_NA = NULL,
                        code_from = NULL,
                        code_to = NULL) {

  data = data %>%
    select(!!!cols) %>%
    mutate(across(!!!cols, as.numeric)) %>%

  if (reverse_code == T) {
    return_df = data %>%
      mutate(across(!!!cols, ~ if_else(. %in% code_as_NA, NA_real_, .))) %>%
      mutate(across(!!!cols, ~ psych::reverse.code(-1,.)))
  } else if (!is.null(code_from)){
    # need to try to code it in a more generalizable way
    return_df = data %>%
      mutate(across(!!!cols, ~ case_when(
        . == code_from[1] ~ code_to[1],
        . == code_from[2] ~ code_to[2],
        . == code_from[3] ~ code_to[3],
        . == code_from[4] ~ code_to[4],
        . == code_from[5] ~ code_to[5],
        . == code_from[6] ~ code_to[6],
        . == code_from[7] ~ code_to[7],
        . == code_from[8] ~ code_to[8],
        . == code_from[9] ~ code_to[9]
      )))
  } else{
    return_df = data %>%
      mutate(across(!!!cols, ~ if_else(. %in% code_as_NA, NA_real_, .)))
  }
  return_df = return_df %>% data.table::as.data.table() %>% as_tibble()
  return(return_df)
}
