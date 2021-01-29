#' Center with respect to group mean
#'
#' Center all columns with respect to the group mean
#' @param dataframe dataframe
#' @param cols vector or quos(). column(s) that need to be centered
#' @param group the grouping variable. If you need to pass multiple group variables, try to use quos(). Passing multiple group variables is not tested.
#'
#'
#' @return
#' return a dataframe with the columns centered (replace existing columns)
#' @export
#'
#' @examples
#' center_group_mean(data = df1, cols = quos('IV1:IV5'), group = 'Country')

center_group_mean = function(data, cols, group){
  return_df = data %>%
    group_by(across(!!!group)) %>%
    mutate(across(!!!cols, function(x) { (x - mean(x,na.rm = T))})) %>%
    ungroup()
  return(return_df)
}
