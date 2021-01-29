#' Center with respect to grand mean
#'
#' Center all columns with respect to the grand mean
#' @param data dataframe
#' @param cols vector or quos(). column(s) that need to be centered
#'
#' @return
#' return a dataframe with the columns centered (replace existing columns)
#' @export
#'
#' @examples
#' center_group_mean(data = df1, cols = quos('IV1:IV5'))
#'
center_grand_mean = function(data, cols) {
  return_df = data %>%
    mutate(across(!!!cols, function(x) { (x - mean(x,na.rm = T))}))
  return(return_df)
}
