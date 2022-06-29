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

# ESTABLISH API KEY AND CONNECTION
youtubeAPIKey <- # PLACE API KEY HERE
connectionURL <- 'https://www.googleapis.com/youtube/v3/videos'


# REQUESTS OPTIONS
testConnection <- "https://www.google.com/"
testCount <- 100

# CURL TEST
pool <- new_pool()
for(i in 1:testCount){curl_fetch_multi(testConnection)}
system.time(out <- multi_run(pool = pool))


# CREATE REQUEST AND REMOVE DUPLICATES
createRequest  <- function(id){
  paste0(connectionURL,
         "?key=",youtubeAPIKey,
         "&id=",id,
         "&fields=","items(id,snippet(channelId,title,description,categoryId))",
         "&part=","snippet")
}
uniqueWatchedVideoIDs <- unique(watchedVideosDataFrame$id)
requests <- pblapply(uniqueWatchedVideoIDs, createRequest )

# PARSE OUT RESPONSE
getMetadataDataFrame <- function(response){
  rawchar <- rawToChar(response$content)
  parsedData <- fromJSON(rawchar)
  data.frame <- cbind(id = parsedData$items$id, parsedData$items$snippet)
  return(data.frame)
}

videoMetadataDataFrame <- data.frame(id = c(),
                                     channelId = c(),
                                     title = c(),
                                     description = c(),
                                     categoryId = c()
)

# SUCCESS
addToMetadataDataFrame <- function(response){
  .GlobalEnv$videoMetadataDataFrame <- rbind(.GlobalEnv$videoMetadataDataFrame,getMetadataDataFrame(response))
}

# FAIL
failFunction <- function(request){
  print("fail")
}

# GRAB REQUEST RESPONSE FROM MEMORY
fetchMetadataFromMemory <- function(request){
  return(getMetadataDataFrame(curl_fetch_memory(request)))
}

system.time(out <- multi_run(pool = pool)) 
saveRDS(videoMetadataDataFrame, file = "videoMetadataDataframeAsync1.rds")

length(requests)
nrow(videoMetadataDataFrame)

listMetadata <- pblapply(requests, fetchMetadataFromMemory)

# COMBINE LIST INTO A DATA FRAME
videoMetadataDataFrame <- bind_rows(listMetadata)
saveRDS(videoMetadataDataFrame, file = "videoMetadataDataFrame_memory.rds")

# CATEGORY ID REQUEST
categoryListURL <- "https://www.googleapis.com/youtube/v3/videoCategories"

categoryResponse <- GET(url = categoryListURL,
                        query = list(
                          key = youtubeAPIKey,
                          regionCode = "us",
                          part = "snippet"
                        ))
parsedCategoryResponse <- content(categoryResponse, "parsed")


categoryDataFrame <- data.frame(categoryId=c(), category=c())
for(item in parsedCategoryResponse$items){
  categoryDataFrame <<-rbind(categoryDataFrame, 
                             data.frame(categoryId = item$id, category=item$snippet$title))
}

categoryDataFrame
videoMetadata <- merge(x = videoMetadataDataFrame, y = categoryDataFrame, by = "categoryId")
head(videoMetadata)

# COMBINE WITH WATCH HISTORY
watchedVideos <- merge(watchedVideosDataFrame , videoMetadata, by="id")
str(watchedVideos)
