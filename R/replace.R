#' Title
#'
#' @param df_final
#' @param number_table1
#'
#' @return
#' @export
#'
#' @examples
replace <- function(df_final,number_table1){
  for(i in 1:nrow(df_final)){
    l <- lapply(strsplit(as.character(df_final$movie_ids[i]), ","), as.numeric);
    vec <- unlist(l)
    new_vec <- list()
    for(x in vec){
      index <- which(number_table1$Item_Name == x)
      value <- number_table1[index,as.integer(3)]
      new_vec <- c(new_vec, value)
    }
    str <- ""
    str <- paste(str, toString(new_vec))
    df_final$movie_ids[i] <- str
  }
  return (df_final)
}
