## Cleanup production company
# Get rid of all the movie genres
for(i in names(df_genre)[-1]) {
  df_merge$production_company <- gsub(pattern=i, replacement="",
                                      x=df_merge$production_company)
}

# Foreign is missing!!!
df_merge$production_company <- gsub(pattern="Foreign", replacement="",
                                    x=df_merge$production_company)


df_company <- data.frame(stringr::str_split_fixed(df_merge$production_company,
                                                  pattern=",", n=Inf),
                         stringsAsFactors = FALSE)

# df_company$X1 <- df_merge$tmdb_id
# company <- df_company$X2[!is.na(df_company$X2)]
# company <- company[company != ""]
# 
# length(df_genre)
# names(df_genre)[2:20] <- company[1:19]
# df_genre[2] <- NULL


