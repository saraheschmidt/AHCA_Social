setwd("~/R Projects/BCBSM_Project/")

library(plyr)
library(stringr)
library(ggplot2)
library(ggmap)
library(tm)
library(RColorBrewer)
library(rgdal)
library(wordcloud)
library(tigris)
library(Rgraphviz)
library(graph)
library(scales)
library(xlsx)
library(topicmodels)
library(longurl)
library(twitteR)
library(sentiment)
library(data.table)
library(maps)
library(mapproj)
library(tidyr)
library(longurl)
library(plotly)
library(stringi)
library(zoo)
library(venneuler)
library(RSentiment)

##Read data
data <- read.table("Raw Data/2017-05-13 08-PM_cleanedtweets.txt",
                    sep="\t", header=TRUE, fill=TRUE)

##Select relevant tweets and create 3 data frames to store them
aca <- data[grep("#aca", tolower(data$text)), ]
ahca <- data[grep("#ahca", tolower(data$text)), ]
healthcare <- ahca
healthcare <- unique(healthcare)

##Clean data frames
###Create functions
####Convert date fields
convertDate <- function(x) {
  times1 <- as.character(x$created_at)
  times1 <- as.vector(times1)
  times1 <- strptime(times1, "%a %b %d %H:%M:%S %z %Y",
                  tz = "GMT")
  x$created_at <- times1
  times2 <- as.character(x$user_created_at)
  times2 <- as.vector(times2)
  times2 <- strptime(times2, "%a %b %d %H:%M:%S %z %Y",
                     tz = "GMT")
  x$user_created_at <- times2
  x
}

####Clean and expand hashtags_mentions field
cleanHashtags <- function(x) {
  
  x$hashtags <- str_extract_all(x$text,
                                  "#[:alnum:]+",
                                  simplify=FALSE)
  x$mentions <- str_extract_all(x$text, 
                                  "@[:alnum:]+", 
                                  simplify=FALSE)
  ##split up lists into columns
  x$hashtags <- lapply(x$hashtags, trimws)
  d1 <- as.data.frame(do.call(rbind, lapply(x$hashtags, `length<-`, 
                                            max(lengths(x$hashtags)))), 
                      stringsAsFactors=FALSE)
  names(d1) <- paste0("hashtag", seq_along(d1))
  x$mentions <- lapply(x$mentions, trimws)
  d2 <- as.data.frame(do.call(rbind, lapply(x$mentions, `length<-`, 
                                            max(lengths(x$mentions)))), 
                      stringsAsFactors=FALSE)
  names(d2) <- paste0("mention", seq_along(d2))
  
  emptycols <- colSums(is.na(d1)) == nrow(d1)
  d1 <- d1[!emptycols]
  emptycols <- colSums(is.na(d2)) == nrow(d2)
  d2 <- d2[!emptycols]
  ##add up numbers of hashtags and mentions/tweet
  d1$hashtag_na_count <- apply(d1, 1, function(y) sum(is.na(y)))
  d1$hashtagSum <- (ncol(d1) - 1) - d1$hashtag_na_count
  d2$mention_na_count <- apply(d2, 1, function(y) sum(is.na(y)))
  d2$mentionSum <- (ncol(d2) - 1) - d2$mention_na_count
  ##combine data frames
  x <- cbind(x, d1, d2)
  ##remove unnecessary fields
  x$hashtags_mentions <- NULL
  x
  
}


####Expand links field
cleanLinks <- function(x) {
  
  x$links <- str_extract_all(x$text, 
                             "http[s]?://[[:alnum:].\\/]+", 
                             simplify=FALSE)
  
  ##split up lists into columns
  x$links <- lapply(x$links, trimws)
  d3 <- as.data.frame(do.call(rbind, lapply(x$links, `length<-`, 
                                            max(lengths(x$links)))), 
                      stringsAsFactors=FALSE)
  names(d3) <- paste0("link", seq_along(d3))
  
  emptycols <- colSums(is.na(d3)) == nrow(d3)
  d3 <- d3[!emptycols]
  
  ##add up numbers of hashtags and mentions/tweet
  d3$link_na_count <- apply(d3, 1, function(y) sum(is.na(y)))
  d3$linkSum <- (ncol(d3) - 1) - d3$link_na_count
  
  ##combine data frames
  x <- cbind(x, d3)
  x
  
}

###Apply functions to each data frame
aca <- convertDate(aca)
ahca <- convertDate(ahca)
healthcare <- convertDate(healthcare)

aca <- cleanHashtags(aca)
ahca <- cleanHashtags(ahca)
healthcare <- cleanHashtags(healthcare)

aca <- cleanLinks(aca)
ahca <- cleanLinks(ahca)
healthcare <- cleanLinks(healthcare)

####Convert time zone to EST
x <- as.POSIXct(aca$created_at, tz="GMT")
aca$created_at <- format(x, tz="America/Detroit",usetz=TRUE)

x <- as.POSIXct(ahca$created_at, tz="GMT")
ahca$created_at <- format(x, tz="America/Detroit",usetz=TRUE)

x <- as.POSIXct(healthcare$created_at, tz="GMT")
healthcare$created_at <- format(x, tz="America/Detroit",usetz=TRUE)

####Create hourly time bins
aca$hourBin <- cut(as.POSIXlt(aca$created_at), breaks="2 hours", 
                   labels=FALSE)
ahca$hourBin <- cut(as.POSIXlt(ahca$created_at), breaks="2 hours", 
                    labels=FALSE)
healthcare$hourBin <- cut(as.POSIXlt(healthcare$created_at), 
                          breaks="2 hours", labels=FALSE)

###Save data frames to cleaned data folder
saveRDS(aca, "Cleaned Data/aca_tweets.rds")
saveRDS(ahca, "Cleaned Data/ahca_tweets.rds")
saveRDS(healthcare, "Cleaned Data/healthcare_tweets.rds")

###Basic Visualizations
v <- venneuler(c(AHCA=7081, ACA=2137, "AHCA&ACA"=537))
plot(v)

ggplot() + 
  geom_bar(data=healthcare, aes(as.POSIXct(healthcare$created_at)), 
           stat="bin", alpha=0.5, position="dodge", bins=48, 
           fill="deepskyblue", color="gray40") +
  ggtitle('Overlay Plot of Tweet Counts over Time') +
  ylab('Tweet Count') +
  xlab('Time') +
  scale_x_datetime(labels = date_format("%a %I %p"),
                    breaks = date_breaks(width="6 hours")) +
  theme_classic()

ggplot() + 
  geom_bar(data=healthcare, aes(as.POSIXct(healthcare$created_at)), 
           stat="bin", alpha=0.5, position="dodge", bins=12, 
           fill="deepskyblue", color="gray40") +
  ggtitle('Overlay Plot of Tweet Counts over Time') +
  ylab('Tweet Count') +
  xlab('Time') +
  scale_x_datetime(labels = date_format("%a %I %p"),
                   breaks = date_breaks(width="6 hours")) +
  theme_classic()

hist(aca$followers_count)

ggplot(aca, aes(followers_count)) +
  geom_histogram()

ggplot(aca, aes(friends_count)) +
  geom_histogram()

ggplot(ahca, aes(followers_count)) +
  geom_histogram()

ggplot(ahca, aes(friends_count)) +
  geom_histogram()

ggplot(aca, aes(lang)) +
  geom_bar()

ggplot(ahca, aes(lang)) +
  geom_bar()

ggplot(aca, aes(in_reply_to_screen_name)) +
  geom_bar()

###Geocoding and Mapping#############################################
locations<-subset(healthcare, healthcare$location!="")

locations$location<-gsub("%", " ",
                         locations$location)

locations <- as.data.table(locations)

locations <- locations[, .N ,by = location]

locations <- as.data.frame(locations)

geo <- geocode(locations$location, source = "google", 
              output = "all")

condition_a <- sapply(geo, function(x) x["status"]=="OK")
geo<-geo[condition_a]

condition_b <- lapply(geo, lapply, length)
condition_b2<-sapply(condition_b, function(x) x["results"]=="1")
geo<-geo[condition_b2]
length(geo)

geocode_results <- geo
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/cleaning_geocoded_results.R")
results<-lapply(geocode_results, as.data.frame)

results2<-lapply(results,function(x) subset(x, select=c("results.formatted_address",
                                                            "results.geometry.location.lat",
                                                            "results.geometry.location.lng")))
resultsc<-lapply(results2,function(x) data.frame(Location=x[1,"results.formatted_address"],
                                                     lat=x[1,"results.geometry.location.lat"],
                                                     lng=x[2,"results.geometry.location.lng"]))

resultsd<-rbindlist(resultsc)

american<-subset(resultsd,
                    grepl(", USA", resultsd$Location)==TRUE)

american$commas<-sapply(american$Location, function(x)
  length(as.numeric(gregexpr(",", as.character(x))[[1]])))
american<-subset(american, commas==2)
#Drop the "commas" column:
american<-subset(american, select=-commas)

nrow(american)

###Hashtag Analysis

###Mentions Analysis

###URL Analysis

###Text Mining
corpus <- Corpus(VectorSource(healthcare$text))

corpus <- tm_map(corpus, content_transformer(tolower))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

corpus <- tm_map(corpus, content_transformer(removeNumPunct))

corpus <- tm_map(corpus, removeWords, c(stopwords("english"),
                                        "rt", "americorps", "via", 
                                        "amp", "htt", "na",
                                        "en", "href", "ahca"))

corpus <- tm_map(corpus, stripWhitespace)

corpusCopy <- corpus

tdm <- TermDocumentMatrix(corpus,
                            control = list(wordLengths = c(1, Inf)))

tdm

(freq.terms <- findFreqTerms(tdm, lowfreq = 500))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 500)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot(df, aes(x=reorder(term,freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

m <- as.matrix(tdm)

# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 100,
          random.order = F, colors = pal)

findAssocs(tdm, "false", 0.4)
findAssocs(tdm, "obamacare", 0.4)
findAssocs(tdm, "care", 0.4)
findAssocs(tdm, "senate", 0.4)
findAssocs(tdm, "gop", 0.2)
findAssocs(tdm, "bill", 0.2)
findAssocs(tdm, "support", 0.3)

plot(tdm, term = freq.terms, corThreshold = 0.2)

dtm <- as.DocumentTermMatrix(tdm)

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]

lda <- LDA(dtm.new, k = 8) # find 3 topics

term <- terms(lda, 10) # first 7 terms of every topic

(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics <- topics(lda) # 1st topic identified for every document (tweet)

topics <- cbind(healthcare$hourBin, topic=topics)

topics <- as.data.frame(topics)

ggplot(topics, aes(V1, fill = as.factor(topic))) +
  geom_density(aes(group=as.factor(topic), 
                   fill=as.factor(topic)), alpha=1, 
               position="fill", color="black") +
  scale_fill_brewer(palette="Spectral") +
  theme_classic()

ggplot(topics, aes(V1, fill = as.factor(topic))) +
  geom_bar(aes(group=as.factor(topic), fill=as.factor(topic)), 
           color="black", alpha=1, position=position_fill(vjust=1)) +
  scale_fill_brewer(palette="Spectral") +
  theme_classic()

###Sentiment Analysis
sentiments <- calculate_sentiment(aca$text)
sentiments <- calculate_score(healthcare$text)

library(qdap)

sentiments <- polarity(healthcare$text)
sentiments <- as.data.frame(sentiments)
sentiments$text <- healthcare$text
sentiments$hourBin <- healthcare$hourBin
sentiments$created_at <- healthcare$created_at

ggplot(sentiments, aes(x=hourBin, y=all.polarity)) +
  geom_jitter() +
  ylim(-3,3)

ggplot(sentiments, aes(x=hourBin, y=all.polarity)) +
  geom_hex(aes(fill=..count..), color="gray25") +
  ylim(-3,3) +
  scale_fill_gradient(high="dodgerblue4", low="deepskyblue") +
  theme_minimal()


acasentiments <- polarity(aca$text)
acasentiments <- as.data.frame(acasentiments)
acasentiments$text <- aca$text
acasentiments$hourBin <- aca$hourBin
acasentiments$created_at <- aca$created_at

ahcasentiments <- polarity(ahca$text)
ahcasentiments <- as.data.frame(ahcasentiments)
ahcasentiments$text <- ahca$text
ahcasentiments$hourBin <- ahca$hourBin
ahcasentiments$created_at <- ahca$created_at

ggplot() +
  geom_jitter(data=acasentiments, aes(x=hourBin, y=all.polarity),
              color="red", alpha=0.5) +
  geom_jitter(data=ahcasentiments, aes(x=hourBin, y=all.polarity),
              color="blue", alpha=0.5) +
  ylim(-3,3)

ggplot() +
  geom_hex(data=acasentiments, aes(x=hourBin, y=all.polarity,
                                   alpha=..count..),
              fill="blue", color="lightgray") +
  geom_hex(data=ahcasentiments, aes(x=hourBin, y=all.polarity,
                                    alpha=..count..),
              fill="red", color="lightgray") +
  ylim(-3,3) +
  theme_minimal()

ggplot(acasentiments, aes(x=hourBin, y=all.polarity)) +
  geom_hex(aes(fill=..count..), color="gray25") +
  ylim(-3,3) +
  scale_fill_gradient(high="dodgerblue4", low="deepskyblue") +
  theme_minimal()

###Structural topic modeling

###Network Analysis







#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1749

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(locations$location))){
  print(paste("Working on index", ii, "of", length(locations$location)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(locations$location[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
}

locations$index <- as.numeric(rownames(locations))
locations <- merge(locations, geocoded, by="index")

##Remove locations outside US and too general
locations <- locations[!(locations$accuracy=="country"),]

electionResults <- read.csv("Raw Data/state_election_results.csv")

electionResults$region <- tolower(electionResults$region)

map <- map_data("state")



map <- merge(map, electionResults, by="region")

map$clinton_perc <- as.numeric(sub("%", "", 
                                   as.character(map$clinton_perc)))

map$trump_perc <- as.numeric(sub("%", "", 
                                   as.character(map$trump_perc)))

map<-get_map(location='united states', zoom=4, maptype = "toner",
             source='stamen',color='color')

ggmap(map) + 
  geom_point(data=locations,
  aes(x=long, y=lat, size=N), alpha=0.5, color="deepskyblue",
  na.rm = T) +
  scale_size_area(max_size=12) +
  theme_nothing()

#####################################################################

corpus2 <- Corpus(VectorSource(healthcare$hashtags))

corpus2 <- tm_map(corpus2, content_transformer(tolower))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
corpus2 <- tm_map(corpus2, content_transformer(removeURL))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

corpus2 <- tm_map(corpus2, content_transformer(removeNumPunct))

corpus2 <- tm_map(corpus2, removeWords, c(stopwords("english"),
                                        "rt", "americorps", "via", 
                                        "amp", "htt", "na",
                                        "en", "href", "cahca",
                                        "caca"))

corpus2 <- tm_map(corpus2, stripWhitespace)

corpusCopy2 <- corpus2

tdm2 <- TermDocumentMatrix(corpus2,
                          control = list(wordLengths = c(1, Inf)))

tdm2

(freq.terms <- findFreqTerms(tdm2, lowfreq = 100))
term.freq2 <- rowSums(as.matrix(tdm2))
term.freq2 <- subset(term.freq2, term.freq2 >= 100)
df2 <- data.frame(term = names(term.freq2), freq = term.freq2)

ggplot(df2, aes(x=reorder(term,freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

m2 <- as.matrix(tdm2)

# calculate the frequency of words and sort it by frequency
word.freq2 <- sort(rowSums(m2), decreasing = T)
# colors
pal2 <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = names(word.freq2), freq = word.freq2, min.freq = 25,
          random.order = F, colors = pal2)

findAssocs(tdm, "trumpcare", 0.4)
findAssocs(tdm, "protectourcare", 0.3)
findAssocs(tdm, "saveaca", 0.4)
findAssocs(tdm, "theresistance", 0.4)
findAssocs(tdm, "gop", 0.2)
findAssocs(tdm, "preexistingcondition", 0.2)
findAssocs(tdm, "votenoahca", 0.3)

plot(tdm, term = freq.terms, corThreshold = 0.15)

dtm2 <- as.DocumentTermMatrix(tdm2)

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm.new2   <- dtm2[rowTotals> 0, ]

lda2 <- LDA(dtm.new2, k = 6) # find 3 topics

term2 <- terms(lda2, 7) # first 7 terms of every topic

(term2 <- apply(term2, MARGIN = 2, paste, collapse = ", "))

topics2 <- topics(lda2) # 1st topic identified for every document (tweet)

topics2 <- cbind(healthcare$created_at, topic=topics2)

topics2 <- as.data.frame(topics2)

ggplot(topics2, aes(as.POSIXlt(V1), y=..count.., 
                    fill = as.factor(topic))) +
  geom_area(stat="bin", alpha=0.5, 
            color="black", bins=12) +
  scale_fill_brewer(palette="Paired") +
  theme_classic()

ggplot(topics2, aes(as.POSIXlt(V1))) +
  facet_wrap(~as.factor(topic)) +
  geom_bar(aes(group=as.factor(topic), fill=as.factor(topic)), 
           color="black", alpha=1, position="stack",
           stat="bin", bins=48) +
  scale_fill_brewer(palette="Paired") +
  scale_x_datetime(labels = date_format("%a %I%p"),
                   breaks = date_breaks(width="6 hours")) +
  theme_classic()
