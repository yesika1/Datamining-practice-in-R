# Natural Language Processing NLP
# =========================

# Featurize document based on word count.
# Bag of words:  Dct represented as a vector of word counts 
# Corpus: collection/group of all docs
# A = Blue house -> (red,blue,house) -> (0,1,1)
# B = Red house -> (red,blue,house) -> (1,0,1)
# cosine similarity: on vectors to det. similarity 
# Sim(A,B) = cos(teta) = A.B / (||A|| . ||B||)
# improve bag of words, adjusting word counts based on their frequency on corpus
# TF-IDF: Term Frequency - Inverse Document Frequency
# TF-IDF: How important a word is just not relevan to a doc but to a entire corpus 
# TF: Importance of a term within that doc.
# TF(d,t) = Number of occurrences of term t in doc d
# IDF: Importance of the term in the corpus
# IDF(t) = log(D/T)
# D: total number of docs
# T= number of docs with the term t
# TF-IDF = Wx,y = TFx,y . log (N/dfx)
# Wx,y: Term x in doc y
# TFx,y: frequency of x in doc y
# N: number total of docs (corpus).
# dfx: number of docs containing x
# stop words: common words that we want to remove that do not add info


# Twiter
install.packages('tm')
install.packages('twitteR')
install.packages('wordcloud')
install.packages('RColorBrewer')
install.packages('e1017')
install.packages('class')

library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

#open twitter
# open website: apps.twitter.com > create new app > keys and access tokens
# app name: NLPexampleYCD
# Consumer Key (API Key) xxx
ckey <- 'xxx'
# Consumer Secret (API Secret)	
skey <- 'xxx'
# Access Token	xxx
token <- 'xxx'
# Access Token Secret	
stoken<- 'xxx'


# connect to twitter
setup_twitter_oauth( ckey,skey,token,stoken)

# Returning socce tweets
soccer_tweets <- searchTwitter('soccer', n=1000, lang='en' ) # n= most recent 3 tweets
print(soccer_tweets)

# grab text data from tweets
soccer_text <- sapply(soccer_tweets, function(x){x$getText()})

# clean Text data
soccer_text2 <-iconv(soccer_text, 'UTF-8', 'ASCII') # remove symbols, emoticons 

# creating corpus
soccer_corpus <- Corpus(VectorSource(soccer_text2))

# Document term matrix:
# one word per row and times that appear in a doc
term_doc_matrix <- TermDocumentMatrix(soccer_corpus, control=list(removePunctuation=T,
                                                           stopwords= c('soccer', 'https',stopwords('english')),
                                                           removeNumbers =T,
                                                           tolower=T ))

term_doc_matrix <- as.matrix(term_doc_matrix)

#gert word counts
word_freq <- sort(rowSums(term_doc_matrix),decreasing = T)
dm <- data.frame(word=names(word_freq), freq=word_freq)

# create wordcloud
wordcloud(dm$word, dm$freq,random.order = F, colors = brewer.pal(8,'Dark2'))