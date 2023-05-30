


library(r2r)

is_object_subset2 <- function(p1,p2){
  for(key in keys(p1)){
    if(has_key(p2,key)){
      for(val in p1[[key]]){{
        if(!val %in% p2[[key]]){
          return (FALSE)
        }
      }

      }
    }
    else{

      return (FALSE)
    }
  }
  return (TRUE)
}



is_object_subset <- function(p1,p2){
  for(key in keys(p1)){
    if(has_key(p2,key)){
      if(identical(p1[[key]],p2[[key]]) == FALSE){
        return (FALSE)
      }
    }
    else{
      return (FALSE)
    }
  }
  return (TRUE)
}

has_same_object <- function(p1,p2){
  return (is_object_subset(p1,p2) & is_object_subset(p2,p1))
}



is_closed <- function(P1,pattern_list){
  #print("closed")
  for(pat in keys(pattern_list)){

    if((length(pat[["itemset"]]) > length(P1[["itemset"]])) && (has_same_object(pat[["object"]],P1[["object"]])) ){

      if(all(P1[["itemset"]][[1]] %in% pat[["itemset"]][[1]])){
        return (FALSE)

      }
    }
  }
  return (TRUE)
}



find_unique_patterns <- function(all_patterns){
  unique_patterns <- hashset()
  for(pat in keys(all_patterns)){
    dupe_found <- FALSE
    for(upat in keys(unique_patterns)){
      if((setequal(pat[["itemset"]],upat[["itemset"]])==TRUE) & (has_same_object(pat[["object"]],upat[["object"]])==TRUE)){
        dupe_found <- TRUE
        break
      }

    }
    if(dupe_found == FALSE){
      insert(unique_patterns,pat)
    }
  }



  return (unique_patterns)
}


form_patterns <- function(h_node){
  pattern_list <- hashset()
  #print("hi")
  if(length(h_node[["children"]]) > 0){
    for(child in keys(h_node[["children"]])){
      partial_patterns <- hashset()
      partial_patterns <- form_patterns(child)
      for(pattern in keys(partial_patterns)){
        if(!h_node[["item"]] %in% pattern[["itemset"]][[1]]){
          if(length(pattern[["itemset"]][[1]]) == 0){
            #print("hi")
            pattern[["itemset"]] <- list(h_node[["item"]])
          }
          else{
            #pattern[["itemset"]][[1]] <- append(pattern[["itemset"]][[1]] , h_node[["item"]])
            pattern[["itemset"]][[length(pattern[["itemset"]])+1]] <-  h_node[["item"]]
          }
        }
        insert(pattern_list,pattern)
      }
    }
  }
  if(length(h_node[["leaf"]]) > 0){
    pp <- hashmap()
    #print("hi")
    #print("hnode")
    #print(h_node[["item"]])
    ll <- list(h_node[["item"]])
    pp[["itemset"]] <- ll
    pp[["object"]] <- hashmap()
    hh <- h_node[["leaf"]]
    pp[["object"]] <- hh



    insert(pattern_list,pp)

  }
  return (pattern_list)
}




get_all_patterns <- function(h_tree){
  #print("gap")
  all_patterns <- hashset()
  for(key in keys(h_tree)){

    #print("gap")
    some_patterns <- hashset()
    h_map <- h_tree[[key]]
    some_patterns <- form_patterns(h_map)
    for(pattern in keys(some_patterns)){
      insert(all_patterns,pattern)
    }
  }

  return (find_unique_patterns(all_patterns))
}


#' Title
#'
#' @param h_tree
#'
#' @return
#' @export
#'
#' @examples
get_closed_patterns <- function(h_tree){
  all_patterns<- hashset()
  all_patterns <- get_all_patterns(h_tree)
  FCP <- hashset()
  for(pattern in keys(all_patterns)){
    if(is_closed(pattern,all_patterns)){
      insert(FCP,pattern)
    }
  }
  return (FCP)
}
