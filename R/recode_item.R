#' Recode
#'
#' @param data a dataframe
#' @param cols vector or quos(). column(s) that need to be recoded
#' @param code_from vector. the order must match with vector for code_to
#' @param code_to vector. the order must match with vector for code_from
#' @param reverse_code logical. Default as F. If T, it will use psych::reverse_code
#' @param code_as_NA vector of values should be coded as NA in the columns. Default as NULL.
#'
#' @return
#' @export
#'
#' @examples
#'
recode_item <- function(data,
                        cols,
                        code_from = NULL,
                        code_to = NULL,
                        reverse_code = F,
                        code_as_NA = NULL) {

  data = data %>%
    dplyr::mutate(dplyr::across(!!!cols, as.numeric))

  if (reverse_code == T) {
    return_df = data %>%
      dplyr::mutate(dplyr::across(!!!cols, ~ dplyr::if_else(. %in% code_as_NA, NA_real_, .))) %>%
      dplyr::mutate(dplyr::across(!!!cols, ~ psych::reverse.code(-1,.)))
  } else if (!is.null(code_from)){
    # need to try to code it in a more generalizable way
    return_df = data %>%
      dplyr::mutate(dplyr::across(!!!cols, ~ dplyr::case_when(
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
      dplyr::mutate(dplyr::across(!!!cols, ~ dplyr::if_else(. %in% code_as_NA, NA_real_, .)))
  }
  return_df = return_df %>% data.table::as.data.table() %>% tibble::as_tibble()
  return(return_df)
}
