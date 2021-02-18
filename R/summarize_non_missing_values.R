#' Count number of non-missing / non-NA values
#'
#' Center all columns with respect to the grand mean
#'
#' @param data dataframe
#' @param cols vector or quos(). column(s) that need to be centered
#' @param group character. pass to group_by
#'
#' @return
#' return a dataframe with the number of non-NA values
#' @export
#'
#' @examples
#' summarize_non_missing_values(data = df1, cols = quos(Q1:Q10), group = 'Country')
#'
summarize_non_missing_values = function(data, cols, group = NULL,print = F) {
  if (!is.null(group)) {
    return_df = data %>%
      group_by(!!!group) %>%
      summarize(across(!!!cols, ~ sum(!is.na(.))))
  } else {
    return_df = data %>%
      summarize(across(!!!cols, ~ sum(!is.na(.)))) %>%
      gather()
  }

  if (print) {
    print(return_df)
    return(data)
  } else {
    return(return_df)
  }
}
