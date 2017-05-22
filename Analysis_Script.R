setwd("~/R Projects/AHCA_Social/")

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
library(Rmisc)
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

api_key <- "cxdwxchGYGmzGOzNAHtqn6yus"
api_secret <- "uQnmcS8ksscrD10yAY9DrP8XfrmSSwBjr8fCgsMQqVoqTMkPbm"
access_token <- "15086936-DDSo3VdbkpSBUrnY6SaVLKU8GVk6Bi4hYi3DOdhcx"
access_token_secret <- "Ss9IUOAtmRLYhlG615wGCkhPek6omAZW2JLfvZbZEt0hI"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

##Read data
data <- read.table("Raw Data/2017-05-13 08-PM_cleanedtweets.txt",
                    sep="\t", header=TRUE, fill=TRUE)

##Select relevant tweets and create data frameto store them
healthcare <- data[grep("#ahca", tolower(data$text)), ]
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
healthcare <- convertDate(healthcare)

##Remove NA dates
healthcare <- healthcare[!is.na(healthcare$created_at),]

##Calculate number of unique tweets
unique_tweets <- healthcare$text
unique_tweets <- unique(unique_tweets)
length(unique_tweets)

healthcare <- cleanHashtags(healthcare)

healthcare <- cleanLinks(healthcare)

####Convert time zone to EST
x <- as.POSIXct(healthcare$created_at, tz="GMT")
healthcare$created_at <- format(x, tz="America/Detroit",usetz=TRUE)

####Create hourly time bins
healthcare$hourBin <- cut(as.POSIXlt(healthcare$created_at), 
                          breaks="1 hour", labels=FALSE)

###Save data frames to cleaned data folder
saveRDS(healthcare, "Cleaned Data/healthcare_tweets.rds")

###Basic Visualizations

DT <- table(healthcare$screen_name)
DT <- as.data.frame(DT)
DT <- DT[order(-DT$Freq),]
DT <- head(DT, 4768)

ggplot(DT, aes(Freq)) +
  geom_histogram(bins=60, fill="deepskyblue") +
  ggtitle("Distribution of Number of Tweets/User") +
  ylab("User Count") +
  xlab("Number of Tweets") +
  theme_minimal()

ggplot() + 
  geom_bar(data=healthcare, aes(as.POSIXct(healthcare$created_at),
                                alpha=..count..), 
           stat="bin", position="dodge", bins=49, 
           fill="deepskyblue") +
  ggtitle('Tweet Counts over Time') +
  ylab('Tweet Count') +
  xlab('Time') +
  scale_x_datetime(labels = date_format("%a %I %p"),
                    breaks = date_breaks(width="6 hours")) +
  scale_alpha_continuous(name="Count") +
  theme_minimal()


ggplot() + 
  geom_bar(data=healthcare, aes(healthcare$hourBin,
                                alpha=..count..), 
           stat="count", position="dodge",
           fill="deepskyblue") +
  ggtitle('Tweet Counts over Time') +
  ylab('Tweet Count') +
  xlab('Hour') +
  theme_minimal()

###Who?

users <- unique(healthcare$screen_name)

users <- as.vector(users)

user_info <- lookupUsers(users)

user_info_df <- twListToDF(user_info)

user_info_df$days <- Sys.Date() - as.Date(user_info_df$created)
user_info_df$tweetRate <- user_info_df$statusesCount / as.integer(user_info_df$days)

ggplot(user_info_df) +
  geom_histogram(aes(friendsCount, fill="Friends"), 
                 alpha=0.5, bins=1000) +
  geom_histogram(aes(followersCount, fill="Followers"), 
                 alpha=0.5, bins=1000) +
  scale_fill_manual(values=c("Friends"="blue", "Followers"="red"),
                    guide=FALSE) +
  ylab("") +
  xlab("") +
  theme_classic()

ggplot(user_info_df) +
  geom_histogram(aes(friendsCount, fill="Friends"), 
                 alpha=0.5, binwidth=100) +
  geom_histogram(aes(followersCount, fill="Followers"), 
                 alpha=0.5, binwidth=100) +
  xlim(0,11509) +
  scale_fill_manual(name="Count Type", 
                     values=c("Friends"="blue", "Followers"="red")) +
  ggtitle("Distribution of Follower and Friend Counts Across Twitter Users") +
  ylab("User Count") +
  xlab("Follower and Friend Counts") +
  theme_classic()

user_info_df <- user_info_df[(order(-user_info_df$followersCount)),]

qqnorm(user_info_df$friendsCount)
qqnorm(user_info_df$followersCount)


q_followers <- quantile(user_info_df$followersCount, c(.1, .2, .3, .4, .5, .6, .7,
                                      .8, .9, .95,
                                      .99, .999))
q_followers <- as.data.frame(q_followers)
q_followers$percentile <- row.names(q_followers)

q_friends <- quantile(user_info_df$friendsCount, c(.1, .2, .3, .4, .5, .6, .7,
                                        .8, .9, .95,
                                        .99, .999))
q_friends <- as.data.frame(q_friends)
q_friends$percentile <- row.names(q_friends)

quant <- merge(q_followers, q_friends, by="percentile")

quant$percentile <- as.numeric(sub("%", "", quant$percentile))

ggplot(data=quant, aes(percentile, q_followers, group=1)) +
  geom_point(aes(color="Followers")) +
  geom_line(aes(color="Followers")) +
  geom_point(aes(y=q_friends, color="Friends")) +
  geom_line(aes(y=q_friends, color="Friends")) +
  scale_color_manual(name="Count Type", 
                     values=c("Followers"="red", "Friends"="blue")) +
  ylab("Count Value") +
  xlab("Percentile") +
  theme_classic()


user_info_df <- user_info_df[order(-user_info_df$followersCount),] 

screenNames<-subset(healthcare, healthcare$screen_name!="")

screenNames <- as.data.table(screenNames)

screenNames <- screenNames[, .N ,by = screen_name]

screenNames <- screenNames[order(-screenNames$N),]

user_info_df$screen_name <- user_info_df$screenName

user_info_df <- merge(user_info_df, screenNames, by="screen_name")

hist(user_info_df$N)

plot(user_info_df$tweetRate, user_info_df$N)

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
hashtags <- healthcare$hashtags
hashtags <- hashtags[hashtags != "character(0)"]
hashtags <- as.character(hashtags)
hashtags <- gsub("^.*?\\(",".", hashtags)


corpus <- Corpus(VectorSource(hashtags))

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

(freq.terms <- findFreqTerms(tdm, lowfreq = 75))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 50)
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

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 20,
          random.order = F, colors = pal)

findAssocs(tdm, "trumpcare", 0.1)
findAssocs(tdm, "aca", 0.1)


plot(tdm, term = freq.terms, corThreshold = 0.1, weighting=TRUE, 
     attrs = list(graph = list(rankdir ="BT"),
                  node = list(shape = "rectangle", fixedsize=FALSE,fontsize=14)))

dtm2 <- as.DocumentTermMatrix(tdm)

rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm.new2   <- dtm2[rowTotals> 0, ]

lda2 <- LDA(dtm.new2, k = 4) # find 3 topics

term2 <- terms(lda2, 7) # first 7 terms of every topic

(term2 <- apply(term2, MARGIN = 2, paste, collapse = ", "))

topics2 <- topics(lda2) # 1st topic identified for every document (tweet)

topics2 <- cbind(healthcare$created_at, topic=topics2)

topics2 <- as.data.frame(topics2)

ggplot(topics2, aes(as.POSIXlt(V1), y=..count.., 
                    fill = as.factor(topic))) +
  geom_area(stat="bin", alpha=1, 
            color="black", bins=24) +
  scale_fill_brewer(palette="Spectral") +
  theme_classic()

ggplot(topics2, aes(as.POSIXlt(V1))) +
  facet_grid(as.factor(topic)~.) +
  geom_bar(aes(group=as.factor(topic), fill=as.factor(topic)), 
           color="black", alpha=1, position="stack",
           stat="bin", bins=48) +
  scale_fill_brewer(palette="Spectral") +
  scale_x_datetime(labels = date_format("%a %I%p"),
                   breaks = date_breaks(width="12 hours")) +
  theme_classic()


###Mentions Analysis

mentions <- healthcare$mentions
mentions <- mentions[mentions != "character(0)"]
mentions <- as.character(mentions)
mentions <- gsub("^.*?\\(",".", mentions)

corpus <- Corpus(VectorSource(mentions))

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

(freq.terms <- findFreqTerms(tdm, lowfreq = 50))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 70)
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

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 50,
          random.order = F, colors = pal)

findAssocs(tdm, "peterdaou", 0.1)
findAssocs(tdm, "gop", 0.1)
findAssocs(tdm, "protectcare", 0.1)
findAssocs(tdm, "brucejapsen", 0.1)
findAssocs(tdm, "mmpadellan", 0.1)
findAssocs(tdm, "potus", 0.1)
findAssocs(tdm, "foxnews", 0.3)

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting=TRUE, 
     attrs = list(graph = list(rankdir ="BT"),
     node = list(shape = "rectangle", fixedsize=FALSE,fontsize=15)))




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
                                        "en", "href", "ahca",
                                        "theres", "thats", "et", "im",
                                        "ia", "s", "r", "w", "pm",
                                        "care", "health", "mt", "n",
                                        "u", "re", "il"))

corpus <- tm_map(corpus, stripWhitespace)

corpusCopy <- corpus

tdm <- TermDocumentMatrix(corpus,
                            control = list(wordLengths = c(1, Inf)))

tdm

(freq.terms <- findFreqTerms(tdm, lowfreq = 100))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 50)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot(df, aes(x=reorder(term,freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7)) +
  theme_minimal()

m <- as.matrix(tdm)

# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10,
          random.order = F, colors = pal)

findAssocs(tdm, "trumpcare", 0.3)
findAssocs(tdm, "obamacare", 0.3)
findAssocs(tdm, "senate", 0.4)
findAssocs(tdm, "senate", 0.4)
findAssocs(tdm, "gop", 0.2)
findAssocs(tdm, "bill", 0.2)
findAssocs(tdm, "support", 0.3)

plot(tdm, term = freq.terms, corThreshold = 0.1, weighting=TRUE, 
     attrs = list(graph = list(rankdir ="BT"),
                  node = list(shape = "rectangle", fixedsize=FALSE,fontsize=15)))

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

library(stm)
library(Rtsne)
library(geometry)
healthcare2 <- healthcare[,c("text","created_at","followers_count",
                             "hourBin")]
healthcare2$created_at <- as.POSIXlt(healthcare2$created_at)
healthcare2$text <- as.character(healthcare2$text)
healthcare2$clean_text <- healthcare2$text
healthcare2$clean_text <- gsub("http[^[:space:]]*", "", 
                              healthcare2$clean_text)
healthcare2$clean_text <- gsub("[^[:alpha:][:space:]]*", "", 
                              healthcare2$clean_text)
healthcare2$clean_text <- tolower(healthcare2$clean_text)
temp <- textProcessor(documents=healthcare2$clean_text, 
                      metadata=healthcare2,
                      striphtml = TRUE, stem=FALSE, removenumbers = FALSE,
                      customstopwords = c("rt", "americorps", "via", 
                                          "amp", "htt", "na",
                                          "en", "href", "ahca",
                                          "theres", "thats", "et", "im",
                                          "ia", "s", "r", "w", "pm",
                                          "care", "health"))
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta,  lower.thresh = 3,
                     upper.thresh = 1000)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta

K <- c(5,10,15,20,25)
mod <- searchK(docs, vocab, K, data=meta)
plot(mod)

K2 <- c(19,20,21,22,23)
mod2 <- searchK(docs, vocab, K2, data=meta)
plot(mod2)

model <- stm(documents=out$documents, vocab=out$vocab, K=21, 
             prevalence= ~ s(hourBin),
             data=out$meta, init.type = "Spectral")

plot(model)
labelTopics(model)

out$meta$hourBin <- as.numeric(out$meta$hourBin)
prep <- estimateEffect(1:20 ~ s(hourBin), model,
                       meta = out$meta, uncertainty = "Global")

plot(prep, covariate = "hourBin", topics = c(1, 3, 8),
     model = model, method="difference",
     cov.value1 = "Thurs", cov.value2 = "Sat",
     xlab = "Thurs ... Sat",
     xlim = c(-.1, .1),
     labeltype = "custom",
     custom.labels = c('Aetna', 'Senate','Root Canal'),
     main = "Effect of Time")

plot(prep, "hourBin", method = "continuous", topics = 1,
     model = model, printlegend = FALSE, xaxt = "n", xlab = "Time",
     main="Aetna")

mod.out.corr <- topicCorr(model)
plot(mod.out.corr)

###Sentiment Analysis
library(syuzhet)
text <- as.character(healthcare$text)
clean_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
clean_text <- gsub("((?:\\b\\W*#\\w+)+)", "", clean_text)
clean_text <- gsub('http\\S+\\s*', '', clean_text)

sentiments <- get_sentiment(clean_text)

sentiments <- as.data.frame(sentiments)
sentiments$text <- healthcare$text
sentiments$hourBin <- healthcare$hourBin
sentiments$created_at <- healthcare$created_at
sentiments$followers_count <- healthcare$followers_count
sentiments$screen_name <- healthcare$screen_name
sentiments$polarity <- abs(sentiments$sentiments)
sentiments$statuses_count <- healthcare$statuses_count
sentiments <- sentiments[!(sentiments$sentiments > 8),]

ggplot(sentiments, aes(x=hourBin, y=sentiments)) +
  geom_hex(aes(fill=..count..)) +
  ylim(-3,3) +
  scale_fill_gradient(high="navy", low="deepskyblue") +
  theme_minimal()

ggplot(sentiments, aes(x=hourBin, y=polarity)) +
  geom_hex(aes(fill=..count..)) +
  scale_fill_gradient(high="navy", low="deepskyblue") +
  theme_minimal()


summ <- summarySE(data=sentiments, measurevar="sentiments", 
                  groupvars="hourBin")
summ

summ2 <- aggregate(sentiments$sentiments, 
                   by=list(Category=sentiments$hourBin), 
                   FUN=sum)
summ2

ggplot(summ, aes(x=hourBin, y=sentiments)) +
  geom_bar(aes(fill=summ$sentiments), stat="identity") +
  ylim(-1.5, 1.5) +
  geom_errorbar(ymax=summ$sentiments + summ$ci, 
                ymin=summ$sentiments-summ$ci, width=0.5) +
  xlab("Hour") +
  ylab("Mean Sentiment Score") +
  ggtitle("Mean Sentiment Score over Time") +
  scale_fill_gradientn(name="", 
                      colours=c("red","lightgray","darkgreen"),
                      limits = c(-0.8, 0.8)) +
  theme_minimal()

ggplot(summ2, aes(x=Category, y=x)) +
  geom_bar(aes(fill=x), 
           stat="identity") +
  scale_fill_gradientn(name="", 
                       colours=c("red","lightgray","darkgreen"),
                       limits = c(-150, 150)) +
  xlab("Hour") +
  ylab("Sum of Sentiment Scores") +
  ggtitle("Sum of Sentiment Scores over Time") +
  theme_minimal()


ggplot(sentiments, aes(sentiments)) +
  geom_histogram(binwidth=0.1, fill="deepskyblue") +
  xlab("Sentiment Score") +
  ylab("Tweet Count") +
  ggtitle("Distribution of Sentiment Score") +
  theme_minimal() 

ggplot(sentiments, aes(polarity)) +
  geom_histogram(binwidth=0.1, fill="deepskyblue") +
  xlab("Polarity") +
  ylab("Tweet Count") +
  ggtitle("Distribution of Polarity") +
  theme_minimal() 

sentiments <- sentiments[order(-sentiments$sentiments),] 

positive <- head(sentiments, 15)
write.csv(positive, "Cleaned Data/Positive_Tweets.csv")

negative <- tail(sentiments, 15)
write.csv(negative, "Cleaned Data/Negative_Tweets.csv")

plot(sentiments$followers_count, sentiments$sentiments)

sentiments$statuses_count <- as.numeric(as.character(sentiments$statuses_count))
plot(sentiments$statuses_count, sentiments$sentiments)

ggplot(sentiments) +
  geom_hex(aes(x=followers_count, y=sentiments)) +
  scale_fill_gradient(high="navy", low="deepskyblue") +
  theme_minimal()

ggplot(sentiments) +
  geom_hex(aes(x=polarity, y=statuses_count)) +
  scale_fill_gradient(high="navy", low="deepskyblue") +
  theme_minimal()

ggplot(sentiments) +
  geom_hex(aes(x=statuses_count, y=polarity)) +
  scale_fill_gradient(high="navy", low="deepskyblue") +
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

daou <- healthcare[grep("@peterdaou", tolower(healthcare$text)), ]
daou <- unique(daou)

protectcare <- healthcare[grep("@protectcare", tolower(healthcare$text)), ]
protectcare <- unique(protectcare)
protectcare_RT <- protectcare[grep("RT @Protectcare", protectcare$text), ]


brucejapsen <- healthcare[grep("@brucejapsen", tolower(healthcare$text)), ]
brucejapsen <- unique(brucejapsen)
brucejapsen_RT <- brucejapsen[grep("RT @brucejapsen", brucejapsen$text), ]


mmpadellan <- healthcare[grep("@mmpadellan", tolower(healthcare$text)), ]
mmpadellan <- unique(mmpadellan)
mmpadellan_RT <- mmpadellan[grep("RT @mmpadellan", mmpadellan$text), ]

dylanlscott <- healthcare[grep("@dylanlscott", tolower(healthcare$text)), ]
dylanlscott <- unique(dylanlscott)
dylanlscott_RT <- dylanlscott[grep("RT @dylanlscott", dylanlscott$text), ]

ozark <- healthcare[grep("@ozark_lady", tolower(healthcare$text)), ]
ozark <- unique(ozark)
ozark_RT <- ozark[grep("RT @ozark_lady", ozark$text), ]

shrivercenter <- healthcare[grep("@shrivercenter", tolower(healthcare$text)), ]
shrivercenter <- unique(shrivercenter)
shrivercenter_RT <- ozark[grep("RT @shrivercenter", shrivercenter$text), ]

KaiserFamFound <- healthcare[grep("@kaiserfamfound", 
                                  tolower(healthcare$text)), ]
KaiserFamFound <- unique(KaiserFamFound)
KaiserFamFound_RT <- ozark[grep("RT @KaiserFamFound", KaiserFamFound$text), ]

sfdirewolf <- healthcare[grep("@sfdirewolf", 
                              tolower(healthcare$text)), ]
sfdirewolf <- unique(sfdirewolf)
sfdirewolf_RT <- sfdirewolf[grep("RT @SFdirewolf", sfdirewolf$text), ]

a35362 <- healthcare[grep("@a35362", 
                          tolower(healthcare$text)), ]
a35362 <- unique(a35362)
a35362_RT <- a35362[grep("RT @a35362", a35362$text), ]

mcspocky <- healthcare[grep("@mcspocky", 
                            tolower(healthcare$text)), ]
mcspocky <- unique(mcspocky)
mcspocky_RT <- mcspocky[grep("RT @mcspocky", mcspocky$text), ]

trump <- healthcare[grep("@realdonaldtrump", 
                         tolower(healthcare$text)), ]
trump <- unique(trump)
trump_RT <- trump[grep("RT @realDonaldTrump", trump$text), ]

ryan <- healthcare[grep("@speakerryan", 
                        tolower(healthcare$text)), ]
ryan <- unique(ryan)
ryan_RT <- ryan[grep("RT @SpeakerRyan", ryan$text), ]

RepTomMacArthur <- healthcare[grep("@reptommacarthur", 
                                   tolower(healthcare$text)), ]
RepTomMacArthur <- unique(RepTomMacArthur)
RepTomMacArthur_RT <- RepTomMacArthur[grep("RT @RepTomMacArthur", 
                                           RepTomMacArthur$text), ]

gopleader <- healthcare[grep("@gopleader", tolower(healthcare$text)), ]
gopleader <- unique(gopleader)
gopleader_RT <- gopleader[grep("RT @GOPLeader", gopleader$text), ]

housegop <- healthcare[grep("@housegop", tolower(healthcare$text)), ]
housegop <- unique(housegop)
housegop_RT <- housegop[grep("RT @HouseGOP", housegop$text), ]

potus <- healthcare[grep("@potus", tolower(healthcare$text)), ]
potus <- unique(potus)
potus_RT <- potus[grep("RT @POTUS", potus$text), ]

whitehouse <- healthcare[grep("@whitehouse", tolower(healthcare$text)), ]
whitehouse <- unique(whitehouse)
whitehouse <- whitehouse[grep("RT @POTUS", potus$text), ]
