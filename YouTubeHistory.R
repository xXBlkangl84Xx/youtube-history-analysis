# REQUIRED PACKAGES
library(stringr)
library(rvest) 
library(tidyverse) 
library(jsonlite) 
library(tidytext)
library(lubridate) 
library(wordcloud)
library(httr)
library(ggplot2)
library(wordcloud2)
library(RCurl)
library(curl)
library(pbapply)
library(ggthemes)

# READ SEARCH HISTORY
youtubeSearchHistory <- read_html("~/Documents/Data Science/youtube-history-analysis/data/My-YouTube-History/YouTube and YouTube Music/history/search-history.html")

# SCRAPING SEARCH HISTORY
youtubeSearch <- youtubeSearchHistory %>% html_nodes(".header-cell + .content-cell > a") %>% html_text()

# SCRAPING SEARCH HISTORY TIMESTAMPS
youtubeSearchContent <- youtubeSearchHistory %>%
  html_nodes(".header-cell + .content-cell")
youtubeSearchTimeStr <- str_match(youtubeSearchContent, "<br>(.*?)</div>")[,2]
youtubeSearchTime <- mdy_hms(youtubeSearchTimeStr)

# CREATING DATA FRAME SEARCH + TIMESTAMP
youtubeSearchDataFrame <- data.frame(search = youtubeSearch,
                                     time = youtubeSearchTime,
                                     stringsAsFactors = FALSE)

# READ WATCH HISTORY
watchHistory <- read_html("~/Documents/Data Science/youtube-history-analysis/data/My-YouTube-History/YouTube and YouTube Music/history/watch-history.html")

watchedVideoContent <- watchHistory %>% 
  html_nodes(".header-cell + .content-cell")

# POSSIBLE TIME CHARACTERS
watchVideoTimes <- str_match(watchedVideoContent,
                             "<br>([A-Z].*)</div>")[,2]
# POSSIBLE ID VALUES
watchedVideoIDs <- str_match(watchedVideoContent,
                             "watch\\?v=([a-zA-Z0-9-_]*)")[,2]

# VIDEO TITLE
watchedVideoTitles <- str_match(watchedVideoContent, 
                                "watch\\?v=[a-zA-Z0-9-_]*\">(.*?)</a>")[,2]

# DATA FRAME WATCH HISTORY
watchedVideosDataFrame <- data.frame(id = watchedVideoIDs,
                                     scrapedTitle = watchedVideoTitles,
                                     scrapedTime = watchVideoTimes,
                                     stringsAsFactors = FALSE)

watchedVideosDataFrame$time <- 
  mdy_hms(watchedVideosDataFrame$scrapedTime)