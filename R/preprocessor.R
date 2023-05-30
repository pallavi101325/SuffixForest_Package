library(dplyr)
library(r2r)


pre_processor <- function(data_1, data_2, min_support_count){
  df <- read.csv(data_1 , stringsAsFactors = FALSE)
  movie <- hashmap()
  for (i in 1:nrow(df)) {
    temp <- as.integer(df$Movie_ID[i])
    movie[[temp]] <- list(df$Name[i] , df$Year[i])
  }

  df_rating <- read.csv(data_2,stringsAsFactors = FALSE)

  # df_rating$Movie_ID <- as.factor(df_rating$Movie_ID)
  # df1 <- transform(df_rating,item_Frequency=ave(seq(nrow(df_rating)),Movie_ID,FUN=length))
  # df2 <- subset(df1,item_Frequency > min_support_count)
  # df2$item_Frequency <- NULL

  # df_temp <- df_rating[1:250000, ]
  df_temp2 <- filter(df_rating,Rating == as.integer(5))
  df_rating_2 <- df_temp2

  temp_year_vec <- c()
  for(i in 1:nrow(df_rating_2)){
    temp2 <- as.integer(df_rating_2$Movie_ID[i])
    temp_year_vec[i] <- movie[[temp2]][[2]]
  }

  df_rating_2$Year <- temp_year_vec

  df_grouped <- df_rating_2 %>%
    group_by(User_ID, Year) %>%
    summarise(Movie_IDs = paste(Movie_ID, collapse = ','))

  percent <- as.integer(50)
  n <- round(percent/as.integer(100) * nrow(df_grouped))
  set.seed(42)
  rows_to_delete <- df_grouped[sample(nrow(df_grouped), n, replace = FALSE), ]
  df_grouped <- df_grouped[!row.names(df_grouped) %in% row.names(rows_to_delete), ]

  return (df_grouped)
}
