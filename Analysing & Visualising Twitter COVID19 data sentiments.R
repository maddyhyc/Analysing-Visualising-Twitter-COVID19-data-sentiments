# install packages
# get Twitter account - free account, live tweets for the past 7 days
# premium account available for greater access

library(rtweet) # extract and convert Twitter data
library(httpuv) # authenticate Twitter API access
library(dplyr)

# Extracting live tweets from Twitter into dataframe (max 18000 per extract)
twt_date <- search_tweets("COVID19", n = 18000, include_rts = TRUE, lang = "en") #search up to 500 characters, space as AND, can use OR

library(syuzhet) 

# Get Twitter sentiments
# the NRC emotion lexicon is a list of words and their associations with 10 sentiments 
# anger, fear, anticipation, trust, surprise, sadness, joy, disgust, negative, positive 

sa.value_date <- get_nrc_sentiment(twt_date$text) 

scoredate <- colSums(sa.value_date[,]) #summing scores
score_df_date <- data.frame(scoredate) 
sa.scoredate <- cbind(sentiment = row.names(score_df_date), score_df_date, row.names=NULL)

# If you start with your own tweets extract, combine all the daily data like this step
# For the purpose of this exercise, this step is already done and all data is combined in score_df.csv
# Show collected data from Mar 29 - Apr 11
score_df <- cbind(score_df_mar29, score_df_mar30, score_df_mar31, score_df_apr1, score_df_apr2, score_df_apr3, score_df_apr4, score_df_apr5, score_df_apr6, score_df_apr7, score_df_apr8, score_df_apr9, score_df_apr10, score_df_apr11 ) 

# In the video I showed the example with data from Mar 29 - Mar 31 which I left out here
# I included here data collected from Mar 29 - Apr 11 which you can upload using read.csv()
# Once uploaded, you should be able to follow the steps below
score_df <- read.csv("score_df.csv")

# Drop first column
score_df <- score_df[,-1]

# Run this if you need to rename columns if not dont run. Do make sure that column names are in order
colnames(score_df)<- c("sentiment", "mar29", "mar30", "mar31", "apr1", "apr2", "apr3", "apr4", "apr5", "apr6", "apr7", "apr8","apr9", "apr10", "apr11")

# Using tidyr to reformat the table
library(tidyr)
score_df2 <- gather(score_df, 'mar29', 'mar30', 'mar31', 'apr1', 'apr2', 'apr3', 'apr4', 'apr5', 'apr6', 'apr7', 'apr8', 'apr9', 'apr10', 'apr11', key = "day", value = "score", na.rm = FALSE) #For an updated gather() function, use pivot_longer()
# Use ggplot to plot graphs
library(ggplot2)
library(forcats)
library(tidyverse)

ggplot(score_df2, aes(x = fct_relevel(day, c("mar29", "mar30","mar31", "apr1", "apr2", "apr3", "apr4", "apr5", "apr6", "apr7", "apr8", "apr9", "apr10", "apr11")), y = score)) + 
  geom_point(aes(colour = sentiment), size = 3)+ 
  scale_colour_viridis_d()+
  labs(title = "Sentiment Score over 2-week period since Mar 29, 2020", x = "Day (year = 2020)", y = "Score")

# Another  plot
ggplot(score_df2, aes(x = fct_relevel(day, c("mar29", "mar30","mar31", "apr1", "apr2", "apr3", "apr4", "apr5", "apr6", "apr7", "apr8", "apr9", "apr10", "apr11")), y = score)) + 
  geom_point(color = "purple", shape = "diamond", size = 2, alpha = 0.5)+ 
  facet_wrap(~sentiment)+ 
  labs(title = "Sentiment Score over 2-week period since Mar 29, 2020", x = "Day (year = 2020)", y = "Score")

# Heat maps
score_df2$day <- fct_relevel(score_df2$day, c("mar29", "mar30","mar31", "apr1", "apr2", "apr3", "apr4", "apr5", "apr6", "apr7", "apr8", "apr9","apr10", "apr11"))

ggplot(score_df2, aes(day, sentiment)) +
  geom_raster(aes(fill = score)) +
  labs(title = "Score over 2-week period since Mar 29, 2020", x = "Day (year = 2020)", y = "Sentiment")

ggplot(data = score_df2, mapping = aes(x = day, y = sentiment))+
  geom_tile(aes(fill = score)) +
  labs(title = "Score over 2-week period since Mar 29, 2020", x = "Day (year = 2020)", y = "Sentiment")

library(ggpubr)

# Balloon plot (alternative to bar plot for categorical data)
ggballoonplot(score_df2, fill = "score") +
  scale_fill_viridis_c(option = "plasma")
