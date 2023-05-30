
merge_leaf <- function(P1,leaf){
  for(state in keys(leaf)){
    if(!has_key(P1[["object"]] , state)){
      P1[["object"]][[state]] <- leaf[[state]]
    }
    else{
      for(type in leaf[[state]][[1]]){
        if(!type %in% P1[["object"]][[state]][[1]]){
          # P1[["object"]][[state]][[1]] <- append(P1[["object"]][[state]][[1]],type)
          P1[["object"]][[state]][[length(P1[["object"]][[state]])+1]] <- type
        }
      }
    }
  }

}


intersection <- function(p1,p2){

  l <- intersect(p1[["itemset"]],p2[["itemset"]])
  bno <- hashmap()
  new_object <- hashmap()
  new_object <- p1[["object"]]
  other_object <- hashmap()
  other_object <- p2[["object"]]

  for(key in keys(other_object)){
    if(has_key(bno, key)){
      for(val in other_object[[key]]){
        if(!val %in% bno[[key]]){
          bno[[key]][[length(bno[[key]])+1]] <- val
        }
      }

    }
    else{
      bno[[key]] <- other_object[[key]]
    }

  }
  for(key in keys(new_object)){
    if(has_key(bno, key)){
      for(val in new_object[[key]]){
        if(!val %in% bno[[key]]){
          bno[[key]][[length(bno[[key]])+1]] <- val
        }
      }

    }
    else{
      bno[[key]] <- new_object[[key]]
    }

  }


  new_pat <- hashmap()
  new_pat[["itemset"]] <- l
  new_pat[["object"]] <- hashmap()
  new_pat[["object"]] <- bno
  return (new_pat)
}



get_fcps <- function(FCP){

  #new_may_exist <- TRUE
  #while(new_may_exist == TRUE){
  NFCP <- hashset()
  modify_patterns <- hashset()
  for(pat1 in keys(FCP)){
    for(pat2 in keys(FCP)){
      if(!identical(pat1[["itemset"]],pat2[["itemset"]])){
        pat <- intersection(pat1,pat2)

        if(length(pat[["itemset"]]) > 0 ){

          found_in_fcp <- FALSE
          for(i in keys(FCP)){
            if(identical(i[["itemset"]],pat[["itemset"]])){

              found_in_fcp <- TRUE
              if(!is_object_subset2(pat[["object"]],i[["object"]])){

                key1 <- "idx"
                value1 <- i

                key2 <- "leaf"
                value2 <- pat[["object"]]
                object <- hashmap()
                # Build up key value pairs
                object[[ key1 ]] <- value1
                object[[ key2 ]] <- value2
                insert(modify_patterns,object)
              }
              break
            }
          }
          if(found_in_fcp == FALSE){
            found_in_nfcp <- FALSE
            for(i in keys(NFCP)){
              if(identical(i[["itemset"]] , pat[["itemset"]])){

                merge_leaf(i,pat[["object"]])
                found_in_nfcp <- TRUE
                break
              }
            }
            if(!found_in_nfcp){
              #NFCP <- append(NFCP,pat)
              insert(NFCP,pat)
            }
          }
        }
      }
    }
  }

  # if((length(NFCP) + length(modify_patterns)) == 0){
  # new_may_exist <- FALSE
  #}



  for(patch in keys(modify_patterns)){

    merge_leaf(patch[["idx"]],patch[["leaf"]])
    delete(modify_patterns,patch)
  }
  for(pat in keys(NFCP)){
    insert(FCP,pat)
    delete(NFCP,pat)
  }

  return (FCP)
}
