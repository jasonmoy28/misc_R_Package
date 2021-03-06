#' Center with respect to group mean
#'
#' Center all columns with respect to the group mean
#' @param data dataframe
#' @param cols vector or quos(). column(s) that need to be centered
#' @param group the grouping variable. If you need to pass multiple group variables, try to use quos(). Passing multiple group variables is not tested.
#'
#'
#' @return
#' return a dataframe with the columns centered (replace existing columns)
#' @export
#'
#' @examples
#'

center_group_mean = function(data, cols, group){
  return_df = data %>%
    dplyr::group_by(dplyr::across(!!!group)) %>%
    dplyr::mutate(dplyr::across(!!!cols, function(x) { (x - mean(x,na.rm = T))})) %>%
    dplyr::ungroup()
  return(return_df)
}
