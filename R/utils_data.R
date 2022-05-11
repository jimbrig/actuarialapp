#' Pull Unique Values from a dataframe
#'
#' @param df Dataframe
#' @param var Quoted named of variable
#'
#' @return Character vector of unique, sorted values from specified column
#' @export
pull_unique <- function(df, var){
  df[[var]] %>%
    unique() %>%
    sort()
}
