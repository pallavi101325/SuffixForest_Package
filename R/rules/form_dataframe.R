#' Title
#'
#' @param df
#' @param rules
#'
#' @return
#' @export
#'
#' @examples
form_dataframe <- function(df,rules){

  first <- TRUE
  c <- as.integer(30)
  for(r in keys(rules)){
    if(c <= as.integer(0)){ break}
    l1 <- list()
    for(i in r[["antecedent"]][[1]]){
      l1 <- append(l1,i)
    }
    ante <- toString(l1)
    l2 <- list()
    for(i in r[["consequent"]][[1]]){
      l2 <- append(l2,i)
    }
    ante <- toString(l1)
    conse <- toString(l2)
    con <- r[["confidence"]]
    lyft <- r[["lift"]]
    str <- ""
    for(p in keys(r[["object"]])){
      str <- paste(str, toString(p))
      str <- paste(str,"=")
      str <- paste(str, toString(r[["object"]][[p]]))
      str <- paste(str, "\n")
    }

    if(first){
      df <- data.frame(Antecedent = c(ante), Consequent = c(conse), Confidence = c(con) , Lift = c(lyft), ObjectList = c(str))
      first <- FALSE
    }
    else{
      df <-  df %>% add_row(Antecedent = ante, Consequent = conse, Confidence = con ,Lift = lyft, ObjectList = str)
    }


  }
  return(df)
}
