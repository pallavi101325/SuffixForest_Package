
find_support_count <- function(consequent , fcps){

  max_count <- as.integer(0)
  for(pattern in keys(fcps)){
    sup_count <- get_support_count(pattern)
    if (sup_count > max_count & is_subset(pattern[["itemset"]] , consequent)){
      max_count <- sup_count
    }
  }
  return (max_count)
}


get_support_count <-function(object){
  count <- as.double(0)
  for(k in keys(object)){
    count <- count + as.double(length(object[[k]][[1]]))
  }
  return (count)
}


#' Title
#'
#' @param GEN
#' @param FCP
#' @param AR_E
#' @param AR_SB
#' @param AR_PB
#' @param dataset_size
#'
#' @return
#' @export
#'
#' @examples
generate_rules <- function(GEN, FCP , AR_E , AR_SB , AR_PB , dataset_size){

  for(g in keys(GEN)){
    for(f in keys(FCP)){
      if(identical( g[["FCP"]][["itemset"]] , f[["itemset"]])){

        if(!identical(g[["itemset"]] , f[["itemset"]])){
          rule <- hashmap()
          rule[["antecedent"]] <- g[["itemset"]]
          rule[["consequent"]] <- setdiff(f[["itemset"]] , g[["itemset"]])
          rule[["lift"]] <- (get_support_count(f[["object"]])*dataset_size)/(get_support_count(g[["object"]])*find_support_count( rule[["consequent"]] , FCP))
          rule[["object"]] <- f[["object"]]
          rule[["confidence"]] <- as.integer(1.0)
          insert(AR_E,rule)
        }
      }
      else{
        if(is_subset(f[["itemset"]],g[["FCP"]][["itemset"]])){

          confidence <- as.double(get_support_count(f[["object"]]))/as.double(get_support_count(g[["FCP"]][["object"]]))
          rule <- hashmap()
          rule[["antecedent"]] <- g[["itemset"]]
          rule[["consequent"]] <- setdiff(f[["itemset"]] , g[["itemset"]])
          rule[["lift"]] <- (get_support_count(f[["object"]])*dataset_size)/(get_support_count(g[["object"]])*find_support_count( rule[["consequent"]] , FCP))
          rule[["object"]] <- f[["object"]]
          rule[["confidence"]] <- as.double(confidence)
          insert(AR_SB,rule)

        }
      }
    }
  }
  for(f1 in keys(FCP)){
    for(f2 in keys(FCP)){
      if((length(f1[["itemset"]])>0) & (length(f2[["itemset"]])>0) & (length(f1[["itemset"]]) < length(f2[["itemset"]])) & (is_subset(f2[["itemset"]][[1]],f1[["itemset"]]))){

        confidence <- as.double(get_support_count(f2[["object"]]))/as.double(get_support_count(f1[["object"]]))
        rule <- hashmap()
        rule[["antecedent"]] <- f1[["itemset"]]
        rule[["consequent"]] <- setdiff(f2[["itemset"]],f1[["itemset"]])
        rule[["lift"]] <- get_support_count(f2[["object"]])*dataset_size/(get_support_count(f1[["object"]])*find_support_count( rule[["consequent"]], FCP))
        rule[["object"]] <- f2[["object"]]
        rule[["confidence"]] <- as.double(confidence)
        insert(AR_PB,rule)
      }
    }
  }
}
