#' Listwise deletion
#'
#' Perform listwise deletion (the entire rows is disregarded if the row has one NA value)
#' @param data dataframe
#' @param cols vector or quos(). column(s) that need to be centered
#'
#' @return
#' return a dataframe with listwise deletion
#' @export
#'
#' @examples
#'
#'

listwise_deletion = function(data, cols) {
  return_df = data %>%
    dplyr::filter(dplyr::across(!!!cols, ~ !is.na(.)))
  return(return_df)
}
