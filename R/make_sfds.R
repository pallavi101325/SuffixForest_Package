library(r2r)
library(dplyr)
#' Title
#'
#' @param df_sample
#'
#' @return
#' @export
#'
#' @examples
make_sfds <- function(df_sample){
  all_SFDs <- hashmap()
  min_value <- min(df_sample$year)
  max_value <- max(df_sample$year)
  for(key in min_value : max_value){
    item_list <- data.frame()
    item_list <- filter(df_sample , year == as.integer(key))
    sfd_x <- hashmap()
    df_len <- nrow(item_list)
    if(df_len > 0){
      for(i in 1:nrow(df_sample)) {
        x <- as.character(item_list[i,1])
        l <- lapply(strsplit(as.character(item_list[i,3]) , ","),as.numeric);
        vec <- unlist(l)

        if (!is.na(x)) {
          sfd_x[[x]] <- l
        }
      }
      if(length(sfd_x) > 0){
        all_SFDs[[key]] <- sfd_x
      }
    }
  }
  return (all_SFDs)
}
