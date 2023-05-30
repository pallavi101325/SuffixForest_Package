func <- function(h_map){
  print(paste("item -> ",h_map[["item"]]))
  print("leaf => ")
  for(leaf in keys(h_map[["leaf"]])){
    print(paste(leaf,"->",h_map[["leaf"]][[leaf]]))
  }
  print(paste(" \n childrens of ",h_map[["item"]]))
  if(length(h_map[["children"]]) > 0){
    for(child in keys(h_map[["children"]])){
      func(child)
    }
  }

}
#c <- as.integer(0)
for(k in keys(forest)){
  #c <- c+1
  print(paste("k -> ", k))
  #if(c == as.integer(9)){
  #   break;
  #}
  h_map <- forest[[k]]
  func(h_map)
}
