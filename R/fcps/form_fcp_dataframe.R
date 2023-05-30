#' Title
#'
#' @param fcps
#'
#' @return
#' @export
#'
#' @examples
form_fcp_dataframe <- function(fcps){
  Df_main <- data.frame()
  #c <- as.integer(20)
  f <- TRUE
  df_final <- data.frame()
  item_list_vec <- c()
  supp_vec <- c()
  for(x in keys(fcps)){
    #if(c <= as.integer(0)){break}
    l1 <- ""
    for(p in x[["itemset"]]){
      l1 <- paste(l1, toString(p))
      l1 <- paste(l1,",")
    }
    modified_string <- substr(l1, 1, nchar(l1) - 1)
    item_list_vec <- c(item_list_vec , modified_string)
    Df_Temp <- data.frame()
    first <- TRUE
    #count <- as.integer(50)

    for(p in keys(x[["object"]])){
      key_text <- ""
      value_text <- ""
      # if(count <= as.integer(0)){break;}
      key_text <- paste(key_text,p)
      value_text <- paste(value_text,toString(x[["object"]][[p]]))

      if(first){
        Df_Temp <- data.frame(Object_keys = c(key_text), Object_values = c(value_text))
        first <- FALSE
      }
      else{
        Df_Temp <- Df_Temp %>% add_row(Object_keys = key_text , Object_values = value_text)
      }
      #count <- count - as.integer(1)
    }


    df_grouped <- data.frame()
    df_grouped <- Df_Temp %>% group_by(Object_values) %>% summarise(Object_keyss = paste(Object_keys, collapse = ','))
    #print(df_grouped)
    ob_string <- ""
    supp <- as.integer(0)
    for(i in 1:nrow(df_grouped)){

      ob_string <- paste(ob_string, toString(df_grouped$Object_keyss[i]))
      ob_string <- paste(ob_string, "= [")
      ob_string <- paste(ob_string , toString(df_grouped$Object_values[i]))
      ob_string <- paste(ob_string, "]")
      ob_string <- paste(ob_string , ",")

      l <- lapply(strsplit(as.character(df_grouped$Object_values[i]) , ","),as.numeric);
      vec <- unlist(l)
      supp <- supp + as.integer(length(vec))
    }

    supp_vec <- c(supp_vec , supp)
    #print(ob_string)
    if(f){
      df_final <- data.frame(Object_List = c(ob_string))
      f <- FALSE
    }
    else{
      df_final <- df_final %>% add_row(Object_List = ob_string)
    }
    #c <- c - as.integer(1)
  }
  df_final$Itemset <- item_list_vec
  df_final$Support <- supp_vec
  df_final <- df_final[, c("Itemset","Support","Object_List")]
  return (df_final)
}
