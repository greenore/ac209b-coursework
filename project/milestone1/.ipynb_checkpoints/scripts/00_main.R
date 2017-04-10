#!/usr/bin/Rscript
# Purpose:         Milestone 1: EDA Analysis
# Date:            2017-04-03
# Author:          tim.hagmann@gmail.com
# Machine:         SAN-NB0044 | Intel i7-3540M @ 3.00GHz | 16.00 GB RAM
# R Version:       Microsoft R Open 3.3.2 -- "Sincere Pumpkin Patch"
#
# Notes:           Parallelisation requires the "RevoUtilsMath" package (if
#                  necessary, copy it manually into packrat). On Windows install 
#                  RTools in order to build packages.
################################################################################

## Options
options(scipen=10)
update_package <- FALSE
options(java.parameters="-Xmx6g")

## Init files (always execute, eta: 10s)
source("scripts/01_init.R")                   # Helper functions to load packages
source("scripts/02_packages.R")               # Load all necessary packages
source("scripts/03_functions.R")              # Load project specific functions

## Data preparation
df_genre <- read_csv("data/genres_10k_new.txt")
df_companies <- read_csv("data/companies_10k.txt")
df_info <- read_tsv("data/tmdb_movie_info.txt")

x <- names(df_companies)

## Merge
df_merge <- merge(df_info, df_genre, by="tmdb_id", all=TRUE)

## First look at the data
table(is.na(df_genre$Adventure))
table(is.na(df_merge$Adventure))
# 1979 missings

table(is.na(df_info$title))
table(is.na(df_merge$title))
# 3206 missings

# Inner join
df_merge <- merge(df_info, df_genre, by="tmdb_id", all=FALSE)

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


## 13 Genres --> 13 Models

## Get x_vars
x_vars <- names(df_info)[-c(1, 2)]

## Split dataset into test and train 
## (Important before EDA)
set.seed(123)
sample_id <- sample(df_merge$tmdb_id, size=nrow(df_merge) * 0.7, replace=FALSE)
df_merge$partition <- ifelse(df_merge$tmdb_id %in% sample_id, "train", "test")

## Data preparation
# Dates
df_merge$release_date <- as.Date(df_merge$release_date)
df_merge$release_year <- as.character(strftime(df_merge$release_date,"%Y"))
df_merge$release_month <- month.abb[as.numeric(format(df_merge$release_date,"%m"))]


# Actual spliting
df_train <- df_merge[df_merge$partition == "train", ]
df_test <- df_merge[df_merge$partition == "test", ]

# df_train$plot
# df_train$poster_path


## Certain genres might be published in certain months
x <- df_merge[df_merge$Adventure == 1, ]



## Budget
## Beeswarm plot
ggplot(df_train, aes(DayOfWeek, PickupCount)) + 
  labs(title="Plot I: Beeswarm",
       subtitle="Pickup count vs. day of the week") +
  geom_beeswarm(color="black", alpha = 0.8) + 
  xlab("Weekday") + 
  ylab("Pickup count") +
  theme_bw()




ggplot(df_train[df_train$budget > 0, ], aes(x=budget)) +
  geom_histogram(col="white") +
  theme_bw()

names(df_train)
ggplot(df_train[df_train$budget > 0, ], aes(x=budget)) +
  geom_histogram(col="white") +
  theme_bw()

df_merge$Adventure
