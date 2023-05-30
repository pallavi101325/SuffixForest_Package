library(r2r)
library(magrittr)
library(dplyr)

#' Title
#'
#' @param df_final
#' @param min_support
#'
#' @return
#' @export
#'
#' @examples
form_number_table <- function(df_final,min_support){
  list_of_itemlists <- list()

  for(i in 1:nrow(df_final)){
    x <- lapply(strsplit(as.character(df_final[i,3]) , ","),as.numeric);
    vec <- unlist(x)
    list_of_itemlists <- append(list_of_itemlists , vec)
  }

  frequency_map <- hashmap()
  my_vector <- c()
  for(vector in list_of_itemlists){
    for (element in vector) {
      if (element %in% keys(frequency_map)) {
        frequency_map[[element]] <- frequency_map[[element]] + as.integer(1)
      } else {
        frequency_map[[element]] <- as.integer(1)
      }
    }

  }
  var <- as.integer(0)
  df_temp <- data.frame()
  first <- TRUE
  for(key in keys(frequency_map)){
    if(frequency_map[[key]] >= min_support){
      if(first){
        df_temp <- data.frame(Item_Name = c(key),Item_frequency = c(frequency_map[[key]]))
        first <- FALSE
      }
      else{
        df_temp <- df_temp %>% add_row(Item_Name = key ,Item_frequency = frequency_map[[key]])
      }
    }

  }

  freq_vec <- df_temp$Item_frequency
  typeof(freq_vec)
  number_table <- df_temp[order(freq_vec),]
  for(i in 1:nrow(number_table)){
    number_table$Item_Number[i] <- as.integer(i)
  }

  rownames(number_table) <- NULL
  return (number_table)
}
