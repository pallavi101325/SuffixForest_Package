

#' Title
#'
#' @param data1
#' @param min_supp
#'
#' @return
#' @export
#'
#' @examples
output_numberTable <- function(data1,path,min_supp,min_conf){
  print("hi")
  df_sample <- read.csv(data1)
  number_table1 <- data.frame()
  source("form_number_table.R")
  number_table1 <- form_number_table(df_sample, min_supp)

  outputfile <- paste0(path, "/output/", "numberTable_minSupp=" , min_supp,".csv")
  outputfile <- gsub("\\s", "",   outputfile )
  print(outputfile)
  write.csv(number_table1, file = outputfile )
  source("replace.R")
  df_sample1 <- data.frame()
  df_sample1<- replace(df_sample , number_table1)
  source("make_sfds.R")
  SFDS <- hashmap()
  SFDS <- make_sfds(df_sample1)
  source("forest/suffix_forest_build.R")
  forest <- hashmap()
  forest <- suffix_forest_build(SFDS)
  source("fcps/get_closed_patterns.R")
  FCP <- hashset()
  FCP <- get_closed_patterns(forest)
  source("fcps/get_fcps.R")
  h2 <- hashset()
  h2 <- get_fcps(FCP)
  source("fcps/form_fcp_dataframe.R")
  fcp_dataframe <- data.frame()
  fcp_dataframe <- form_fcp_dataframe(h2)
  output_fcp <- paste(path, "/output/", "Triclusters_minSupp=" , min_supp,".csv")
  output_fcp <- gsub("\\s", "",   output_fcp )
  write.csv(fcp_dataframe, file = output_fcp)
  source("generators/get_generators.R")
  gen <- hashset()
  gen <- get_generators(h2)
  ########################################
  dataset_size <- as.integer(0)
  for(k in keys(SFDS)){
    dataset_size <- dataset_size + length(SFDS[[k]])
  }
  ########################################
  AR_E_1 <- hashset()
  AR_SB_1 <- hashset()
  AR_PB_1<- hashset()
  source("rules/generate_rules.R")
  generate_rules(gen,h2,AR_E_1,AR_SB_1,AR_PB_1,dataset_size)
  print(length(AR_E_1))
  print(length(AR_SB_1))
  print(length(AR_PB_1))

  source("rules/form_dataframe.R")
  df_AR_E <- data.frame()
  df_AR_E <- form_dataframe(df_AR_E,AR_E_1)
  df_AR_SB <- data.frame()
  df_AR_SB <- form_dataframe(df_AR_SB,AR_SB_1)
  df_AR_PB <- data.frame()
  df_AR_PB <- form_dataframe(df_AR_PB,AR_PB_1)

  AR_E <- filter(df_AR_E , Confidence >= as.integer(min_conf))
  AR_SB <- filter(df_AR_SB , Confidence >= as.integer(min_conf))
  AR_PB <- filter(df_AR_PB , Confidence >= as.integer(min_conf))
  output_ar_e <- paste(path , "/output/", "AR_E_minSupp=" , min_supp,"min_conf=",min_conf,".csv")
  output_ar_e <- gsub("\\s", "",   output_ar_e  )
  output_ar_sb <- paste(path, "/output/", "AR_SB_minSupp=" , min_supp,"min_conf=",min_conf,".csv")
  output_ar_sb <- gsub("\\s", "",   output_ar_sb  )
  output_ar_pb <- paste(path, "/output/", "AR_PB_minSupp=" , min_supp,"min_conf=",min_conf,".csv")
  output_ar_pb <- gsub("\\s", "",   output_ar_pb  )
  write.csv(AR_E, file = output_ar_e)
  write.csv(AR_SB, file = output_ar_sb)
  write.csv(AR_PB, file = output_ar_pb)
  close(df_sample)

}
