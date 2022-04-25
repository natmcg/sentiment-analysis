library(caret)
library(tidyverse)
library(earth)
library(EGAnet)
library(SentimentAnalysis)
library(lubridate)
library(corpcor)
library(tm)
library(ggpubr)
library(ggplot2)
library(cld3)
library(SnowballC)
library(qgraph)
library(dplyr)
library(devtools)
install_github("hrbrmstr/streamgraph")
library(streamgraph)
install_github("tvall/emoxicon")
library(emoxicon)
library(eRm)

setwd('/Users/nataliemcgartland/Downloads/')

#I moved these files into the Ruins class folder so redirect if you want to play with this more :)
sandia_summaries <- read.csv('sandia_executive_summaries.csv')
sandia_messages <-  read.csv('Sandia.csv')
ruins <- read.csv('ruins_database.csv')

#pre-process raw text
require(tm, quietly = TRUE)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
removeNonWords <- function(x) gsub("[^a-zA-Z]+"," ",x)

ruins.corpus <- Corpus(VectorSource(ruins$text))
ruins.corpus <- tm_map(ruins.corpus, removeNonWords)
ruins.corpus <- tm_map(ruins.corpus, tolower)
ruins.corpus <- tm_map(ruins.corpus, removePunctuation)
ruins.corpus <- tm_map(ruins.corpus, removeNumbers)
ruins.corpus <- tm_map(ruins.corpus, removeWords, stopwords("english"))
ruins.corpus <- tm_map(ruins.corpus, removeNumPunct)
ruins.corpus.stem <- tm_map(ruins.corpus, stemDocument)
ruins.dtm <- DocumentTermMatrix(ruins.corpus)

sm.corpus <- Corpus(VectorSource(sandia_messages$text))
sm.corpus <- tm_map(sm.corpus, removeNonWords)
sm.corpus <- tm_map(sm.corpus, tolower)
sm.corpus <- tm_map(sm.corpus, removePunctuation)
sm.corpus <- tm_map(sm.corpus, removeNumbers)
sm.corpus <- tm_map(sm.corpus, removeWords, stopwords("english"))
sm.corpus <- tm_map(sm.corpus, removeNumPunct)
sm.corpus.stem <- tm_map(sm.corpus, stemDocument)
sm.dtm <- DocumentTermMatrix(sm.corpus)

ss.corpus <- Corpus(VectorSource(sandia_summaries$text))
ss.corpus <- tm_map(ss.corpus, removeNonWords)
ss.corpus <- tm_map(ss.corpus, tolower)
ss.corpus <- tm_map(ss.corpus, removePunctuation)
ss.corpus <- tm_map(ss.corpus, removeNumbers)
ss.corpus <- tm_map(ss.corpus, removeWords, stopwords("english"))
ss.corpus <- tm_map(ss.corpus, removeNumPunct)
ss.corpus.stem <- tm_map(ss.corpus, stemDocument)
ss.dtm <- DocumentTermMatrix(ss.corpus)

#return processed texts to original dataframe

for(i in 1:length(ruins.corpus)){
  ruins$text.nostem[i] <- strwrap(ruins.corpus[[i]], 10000)
}
for(i in 1:length(ruins.corpus)){
  ruins$text.stem[i] <- strwrap(ruins.corpus.stem[[i]], 10000)
}

for(i in 1:length(sm.corpus)){
  sandia_messages$text.nostem[i] <- strwrap(sm.corpus[[i]], 10000)
}
for(i in 1:length(sm.corpus)){
  sandia_messages$text.stem[i] <- strwrap(sm.corpus.stem[[i]], 10000)
}

for(i in 1:length(ss.corpus)){
  sandia_summaries$text.nostem[i] <- strwrap(ss.corpus[[i]], 10000)
}
for(i in 1:length(ss.corpus)){
  sandia_summaries$text.stem[i] <- strwrap(ss.corpus.stem[[i]], 10000)
}

#Set up scoring function for Liu Hu pos/neg scoring

scoring.texts <- function(text, pos, neg) { require(plyr)
  require(stringr)
  scores <- ldply(text, function(text, pos, neg) {
    words0 <- str_split(text, '\\s+')
    words <- unlist(words0)
    positive <- sum(!is.na(match(words, pos)))
    negative <- sum(!is.na(match(words, neg)))
    score <- positive - negative
    all <- data.frame(score, positive, negative)
    return(all)
  }, pos, neg)
  scores.df = data.frame(scores, text=text)
  return(scores.df)
}

liu.pos.words <- scan("./positive-words.txt", what = "character", comment.char = ";" , encoding = "UTF-8")
liu.neg.words <- scan("./negative-words.txt", what = "character", comment.char = ";", encoding = "UTF-8")

#Generate the scores for the words (just repeating 1 or -1 for length)
liu.pos.scores <- rep(1, length(liu.pos.words))
liu.neg.scores <- rep(-1, length(liu.neg.words))
#combine words into lexicon & scores into df
liu.lexicon <- c(liu.pos.words, liu.neg.words)
liu.scores <-  c(liu.pos.scores, liu.neg.scores)
liuhu.ruins <- scoring.texts(text = ruins$text.nostem,
                       pos = liu.pos.words, neg = liu.neg.words)
liuhu.sm <- scoring.texts(text = sandia_messages$text.nostem,
                       pos = liu.pos.words, neg = liu.neg.words)
liuhu.ss <- scoring.texts(text = sandia_summaries$text.nostem,
                       pos = liu.pos.words, neg = liu.neg.words)

#sentiment analysis package native to R 
ruins.data.sent <- analyzeSentiment(ruins$text.stem)
sm.data.sent <- analyzeSentiment(sandia_messages$text.stem)
ss.data.sent <- analyzeSentiment(sandia_summaries$text.stem)

#Staiano and Guerini sentiment analysis method
ruins.data.emotions <- data.frame(emoxicon(text = ruins$text.nostem, lexicon = emotions))
sm.data.emotions <- data.frame(emoxicon(text = sandia_messages$text.nostem, lexicon = emotions))
ss.data.emotions <- data.frame(emoxicon(text = sandia_summaries$text.nostem, lexicon = emotions))

#combine sentiment scores 
all.ruins.data <- bind_cols(ruins, liuhu.ruins, ruins.data.sent, ruins.data.emotions)
all.sandia_messages.data <- bind_cols(sandia_messages, liuhu.sm, sm.data.sent, sm.data.emotions)
all.sandia_summaries.data <- bind_cols(sandia_summaries, liuhu.ss, ss.data.sent, ss.data.emotions)

#correct for wordcount of passages
all.ruins.data$liuhu.corrected <- (all.ruins.data$score / all.ruins.data$WordCount)
all.sandia_messages.data$liuhu.corrected <- (all.sandia_messages.data$score / all.sandia_messages.data$WordCount)
all.sandia_summaries.data$liuhu.corrected <- (all.sandia_summaries.data$score / all.sandia_summaries.data$WordCount)


#making graphs!!!

#Sandia summaries (Team A vs Team B) charted by Liu and Hu sentiment scores
ggplot(all.sandia_summaries.data, aes(all.sandia_summaries.data$team, all.sandia_summaries.data$liuhu.corrected)) + geom_bar(stat = 'summary')

#Sandia messages (four levels of warning message) charted by Liu and Hu sentiment scores. Also I'm trying to figure out how to make the graphs colorful and there are a lot more options than I anticipated. 
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$liuhu.corrected)) + geom_bar(stat = 'summary', aes(fill='purple'))
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$liuhu.corrected)) + geom_bar(stat = 'summary', color='purple', fill='salmon')

#Sandia summaries (Team A vs Team B) charted by the eight emotions from Staiano and Guerini
ggplot(all.sandia_summaries.data, aes(all.sandia_summaries.data$team, all.sandia_summaries.data$AFRAID)) + geom_bar(stat = 'summary')
ggplot(all.sandia_summaries.data, aes(all.sandia_summaries.data$team, all.sandia_summaries.data$AMUSED)) + geom_bar(stat = 'summary')
ggplot(all.sandia_summaries.data, aes(all.sandia_summaries.data$team, all.sandia_summaries.data$ANGRY)) + geom_bar(stat = 'summary')
ggplot(all.sandia_summaries.data, aes(all.sandia_summaries.data$team, all.sandia_summaries.data$ANNOYED)) + geom_bar(stat = 'summary')
ggplot(all.sandia_summaries.data, aes(all.sandia_summaries.data$team, all.sandia_summaries.data$DONT_CARE)) + geom_bar(stat = 'summary')
ggplot(all.sandia_summaries.data, aes(all.sandia_summaries.data$team, all.sandia_summaries.data$HAPPY)) + geom_bar(stat = 'summary')
ggplot(all.sandia_summaries.data, aes(all.sandia_summaries.data$team, all.sandia_summaries.data$INSPIRED)) + geom_bar(stat = 'summary')
ggplot(all.sandia_summaries.data, aes(all.sandia_summaries.data$team, all.sandia_summaries.data$SAD)) + geom_bar(stat = 'summary')

#Sandia messages (Levels 2a, 2b, 3, and 4) charted by the eight emotions from Staiano and Guerini
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$AFRAID)) + geom_bar(stat = 'summary')
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$AMUSED)) + geom_bar(stat = 'summary')
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$ANGRY)) + geom_bar(stat = 'summary')
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$ANNOYED)) + geom_bar(stat = 'summary')
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$DONT_CARE)) + geom_bar(stat = 'summary')
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$HAPPY)) + geom_bar(stat = 'summary')
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$INSPIRED)) + geom_bar(stat = 'summary')
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$SAD)) + geom_bar(stat = 'summary')

#Most interesting emotion from each, in my opinion
ggplot(all.sandia_messages.data, aes(all.sandia_messages.data$level, all.sandia_messages.data$HAPPY)) + geom_bar(stat = 'summary', color='purple', fill='salmon')
#for the messages, level 3 is the most happy, which is interesting because well, they're not especially happy messages at all, but level 3 is "basic information" and is written for any audience just to gather what the site is
ggplot(all.sandia_summaries.data, aes(all.sandia_summaries.data$team, all.sandia_summaries.data$ANNOYED)) + geom_bar(stat = 'summary', color='purple', fill='salmon')
#team b is higher in every emotion, meaning maybe they just used more emotional language. "annoyed" is the only time team a comes close. this isn't corrected for wordcount, but the difference is so massive that I don't think it would make much of a difference in any emotion except this one. 

#Now for the ruins databank: charted by Liu and Hu sentiment score, checking to see if poetry or prose is more negative, and then checking to see which of us in class submitted the most negative things on average
ggplot(all.ruins.data, aes(all.ruins.data$category, all.ruins.data$liuhu.corrected)) + geom_bar(stat = 'summary')
ggplot(all.ruins.data, aes(all.ruins.data$submittor, all.ruins.data$liuhu.corrected)) + geom_bar(stat = 'summary')

#This is essentially just reformatting data so I can chart all eight emotions from Staiano and Guerini in one graph
all.ruins.data.long <- all.ruins.data %>% tidyr::gather("id", "value", 27:34)
ggplot(all.ruins.data.long, aes(all.ruins.data.long$id, all.ruins.data.long$value)) + geom_bar(stat = 'summary', aes(fill=all.ruins.data.long$submittor))
all.ruins.data.long$value.corrected <- (all.ruins.data.long$value / all.ruins.data.long$WordCount)
ggplot(all.ruins.data.long, aes(all.ruins.data.long$id, all.ruins.data.long$value.corrected)) + geom_bar(stat = 'summary')




#splitting up the summaries into teams (just more reformatting data)
team.a <- filter(all.sandia_summaries.data, all.sandia_summaries.data$team=='team_a')
ss.corpus.a <- Corpus(VectorSource(team.a$text...3))
ss.corpus.a <- tm_map(ss.corpus.a, removeNonWords)
ss.corpus.a <- tm_map(ss.corpus.a, tolower)
ss.corpus.a <- tm_map(ss.corpus.a, removePunctuation)
ss.corpus.a <- tm_map(ss.corpus.a, removeNumbers)
ss.corpus.a <- tm_map(ss.corpus.a, removeWords, stopwords("english"))
ss.corpus.a <- tm_map(ss.corpus.a, removeNumPunct)
ss.corpus.stem.a <- tm_map(ss.corpus.a, stemDocument)

team.b <- filter(all.sandia_summaries.data, all.sandia_summaries.data$team=='team_b')
ss.corpus.b <- Corpus(VectorSource(team.b$text...3))
ss.corpus.b <- tm_map(ss.corpus.b, removeNonWords)
ss.corpus.b <- tm_map(ss.corpus.b, tolower)
ss.corpus.b <- tm_map(ss.corpus.b, removePunctuation)
ss.corpus.b <- tm_map(ss.corpus.b, removeNumbers)
ss.corpus.b <- tm_map(ss.corpus.b, removeWords, stopwords("english"))
ss.corpus.b <- tm_map(ss.corpus.b, removeNumPunct)
ss.corpus.stem.b <- tm_map(ss.corpus.b, stemDocument)




#document term matrix actions (team a)
dtm.team.a.full <- DocumentTermMatrix(ss.corpus.stem.a)
dtm.team.a <- removeSparseTerms(dtm.team.a.full, sparse = 0.7)
#word frequency
corpus.df.a <- as.data.frame(as.matrix(dtm.team.a))
decreasing.frq <- sort(colSums(corpus.df.a), decreasing = TRUE)
freq.data.decreasing <- head(data.frame(word = names(decreasing.frq), freq=decreasing.frq), n=20L)
freq.data.decreasing

#document term matrix actions (team b)
dtm.team.b.full <- DocumentTermMatrix(ss.corpus.stem.b)
dtm.team.b <- removeSparseTerms(dtm.team.b.full, sparse = 0.7)
#word frequency
corpus.df.b <- as.data.frame(as.matrix(dtm.team.b))
decreasing.frq <- sort(colSums(corpus.df.b), decreasing = TRUE)
freq.data.decreasing <- head(data.frame(word = names(decreasing.frq), freq=decreasing.frq), n=20L)
freq.data.decreasing

#team a says message more, whereas team b says marker. team b is also more concrete terms, generally. reflects their pictorial and symbolic decisions, vs tem a's linguistic (team a wrote messages; team b didn't, only pictures)

#which word has the highest degree of centrality to the network (most connected to most other words)
cor.team.a <- cor_auto(corpus.df.a, forcePD = TRUE)
centrality.team.a <- centrality(cor.team.a)
which.max(centrality.team.a$InDegree)

cor.team.b <- cor_auto(corpus.df.b, forcePD = TRUE)
centrality.team.b <- centrality(cor.team.b)
which.max(centrality.team.b$InDegree)

#with a sparsity of 90%, team a's most central word is "room" while team b's is "diagram". to create the network plot, I had to reduce the sparsity to 70%, making the most central words "future" and "society" respectively
par(mfrow=c(1,2))
ega.bai.a <- EGA(data = cor.team.a, model = "glasso", n=nrow(dtm.team.a))
ega.bai.b <- EGA(data = cor.team.b, model = "glasso", n=nrow(dtm.team.b))


#poetry vs prose networks

#splitting up into poetry or prose (just more reformatting data)
ruins.poetry <- filter(all.ruins.data, all.ruins.data$category=='Poetry')
ruins.corpus.poetry <- Corpus(VectorSource(ruins.poetry$text...1))
ruins.corpus.poetry <- tm_map(ruins.corpus.poetry, removeNonWords)
ruins.corpus.poetry <- tm_map(ruins.corpus.poetry, tolower)
ruins.corpus.poetry <- tm_map(ruins.corpus.poetry, removePunctuation)
ruins.corpus.poetry <- tm_map(ruins.corpus.poetry, removeNumbers)
ruins.corpus.poetry <- tm_map(ruins.corpus.poetry, removeWords, stopwords("english"))
ruins.corpus.poetry <- tm_map(ruins.corpus.poetry, removeNumPunct)
corpus.poetry.stem <- tm_map(ruins.corpus.poetry, stemDocument)

ruins.prose <- filter(all.ruins.data, all.ruins.data$category=='Prose')
ruins.corpus.prose <- Corpus(VectorSource(ruins.prose$text...1))
ruins.corpus.prose <- tm_map(ruins.corpus.prose, removeNonWords)
ruins.corpus.prose <- tm_map(ruins.corpus.prose, tolower)
ruins.corpus.prose <- tm_map(ruins.corpus.prose, removePunctuation)
ruins.corpus.prose <- tm_map(ruins.corpus.prose, removeNumbers)
ruins.corpus.prose <- tm_map(ruins.corpus.prose, removeWords, stopwords("english"))
ruins.corpus.prose <- tm_map(ruins.corpus.prose, removeNumPunct)
corpus.prose.stem <- tm_map(ruins.corpus.prose, stemDocument)

#document term matrix actions (poetry)
dtm.poetry.full <- DocumentTermMatrix(corpus.poetry.stem)
dtm.poetry <- removeSparseTerms(dtm.poetry.full, sparse = 0.7)
#word frequency
corpus.df.poetry <- as.data.frame(as.matrix(dtm.poetry))
decreasing.frq.poetry <- sort(colSums(corpus.df.poetry), decreasing = TRUE)
freq.data.decreasing.poetry <- head(data.frame(word = names(decreasing.frq.poetry), freq=decreasing.frq.poetry), n=20L)
freq.data.decreasing.poetry

#document term matrix actions (prose)
dtm.prose.full <- DocumentTermMatrix(corpus.prose.stem)
dtm.prose <- removeSparseTerms(dtm.prose.full, sparse = 0.6)
#word frequency
corpus.df.prose <- as.data.frame(as.matrix(dtm.prose))
decreasing.frq.prose <- sort(colSums(corpus.df.prose), decreasing = TRUE)
freq.data.decreasing.prose <- head(data.frame(word = names(decreasing.frq.prose), freq=decreasing.frq.prose), n=20L)
freq.data.decreasing.prose

#which word has the highest degree of centrality to the network (most connected to most other words)
cor.poetry <- cor_auto(corpus.df.poetry, forcePD = TRUE)
centrality.poetry <- centrality(cor.poetry)
which.max(centrality.poetry$InDegree)

cor.prose <- cor_auto(corpus.df.prose, forcePD = TRUE)
centrality.prose <- centrality(cor.prose)
which.max(centrality.prose$InDegree)


par(mfrow=c(1,2))
ega.bai.poetry <- EGA(data = cor.poetry, model = "glasso", n=nrow(dtm.poetry))
ega.bai.prose <- EGA(data = cor.prose, model = "glasso", n=nrow(dtm.prose))






