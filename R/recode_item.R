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
recode_item <- function(data,cols,code_from, code_to) {
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
  return(return_df)
}
