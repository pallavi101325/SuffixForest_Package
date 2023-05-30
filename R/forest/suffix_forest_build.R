library(r2r)
library(rjson)

build <- function(suffix) {

  h_node <- hashmap()
  h_node[["item"]] <- suffix[[1]]

  h_node[["children"]] <- hashset()
  h_node[["leaf"]] <- hashmap()

  if(length(suffix) == as.integer(2)) {
    x <- names(suffix)[2] #**
    y <- list(suffix[[2]])
    insert(h_node[["leaf"]],x,y)
  }
  else{
    len <- length(suffix)
    insert(h_node[["children"]],build(suffix[2:len]))
  }

  return(h_node)
}



match <- function(h_node, suffix) {


  if (length(suffix) == as.integer(2)){
    if(length(h_node[["leaf"]]) == as.integer(0)){
      h_node[["leaf"]] <- hashmap()

      for(n in names(suffix)){

        if(nchar(n)>0){
          insert(h_node[["leaf"]],n,list(suffix[[n]])) #**  list(suffix[2][[n]])
        }
      }
    }
    else {
      tree_leaf <- h_node[["leaf"]]

      state <- names(suffix)[2]
      type <- suffix[[state]]

      if (state %in% keys(tree_leaf)){
        x <- tree_leaf[[state]]

        if (type %in% tree_leaf[[state]]){
          print("Warning: Same row already exists in forest !")
        }
        else{
          #tree_leaf[[state]][[1]] <- append(tree_leaf[[state]][[1]] ,type)
          tree_leaf[[state]][[length(tree_leaf[[state]])+1]] <- type
        }
      }
      else{
        if(nchar(state) > 0){
          tree_leaf[[state]] <- list(type)
        }
      }

      h_node[["leaf"]] <- tree_leaf
    }
  }
  else{
    found <- FALSE
    if(length(h_node[["children"]]) > 0){
      for (i in keys(h_node[["children"]])){
        child <- i
        if (child[["item"]] == suffix[[2]]){
          t <- length(suffix)
          match(child, suffix[2:t])
          found <- TRUE
          break
          return
        }
      }
    }
    if(found == FALSE){
      len <- length(suffix)
      temp <- build(suffix[2:len])
      insert(h_node[["children"]],temp)
    }
  }

  # return(h_node)

}

get_suffix <- function(arr, idx, state, type) {
  suffix <- list()

  for(i in idx:length(arr)){
    if(!(arr[i] %in% suffix)){
      #suffix <- append(suffix, arr[i])
      suffix[[length(suffix) + 1]] <- arr[i]
    }
  }

  suffix[[state]] <- list(type)
  return (suffix)
}


get_all_suffix <- function(arr, state, type){
  all_suffix <- hashset()

  for (i in 1 : length(arr)){
    suffix <- get_suffix(arr, i, state, type)
    insert(all_suffix,suffix)
  }

  return (all_suffix)
}





#' Title
#'
#' @param SFD_List
#'
#' @return
#' @export
#'
#' @examples
suffix_forest_build <- function(SFD_List){
  h_tree <- hashmap()

  for(k in keys(SFD_List)){
    SFD <- SFD_List[[k]]
    for(state in keys(SFD)) {
      arr <- SFD[[state]]
      arr1 <- unlist(arr)
      arr1 <- arr1[!is.na(arr1)]
      #print("arr1 : ")
      #print(arr1)
      suffixes <- hashset()
      if(length(arr1) > 0){
        suffixes <- get_all_suffix(arr1,state,k)
      }
      #print("len : ")
      #print(length(suffixes))
      for(i in keys(suffixes)){
        suffix <- i
        if(has_key(h_tree, suffix[[1]])) {
          match(h_tree[[suffix[[1]]]], suffix)
          #insert(h_tree, suffix[[1]], res)
        }
        else {
          #print("hi")
          h_tree[[suffix[[1]]]] <- build(suffix)
        }
      }
    }
  }
  return(h_tree)
}
