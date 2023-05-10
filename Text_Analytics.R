# set the directory
setwd("C:/Masters/gwu/big data/data")
getwd()

install.packages("tm")
(library(tm))

# read the input txt file
my_text <- readLines("APrincessOfMars.txt")

# logic to extract chapters 1 - 11
index_ch1 <- which(my_text == "CHAPTER I", arr.ind = TRUE)
index_ch2 <- which(my_text == "CHAPTER II", arr.ind = TRUE)
index_ch3 <- which(my_text == "CHAPTER III", arr.ind = TRUE)
index_ch4 <- which(my_text == "CHAPTER IV", arr.ind = TRUE)
index_ch5 <- which(my_text == "CHAPTER V", arr.ind = TRUE)
index_ch6 <- which(my_text == "CHAPTER VI", arr.ind = TRUE)
index_ch7 <- which(my_text == "CHAPTER VII", arr.ind = TRUE)
index_ch8 <- which(my_text == "CHAPTER VIII", arr.ind = TRUE)
index_ch9 <- which(my_text == "CHAPTER IX", arr.ind = TRUE)
index_ch10 <- which(my_text == "CHAPTER X", arr.ind = TRUE)
index_ch11 <- which(my_text == "CHAPTER XI", arr.ind = TRUE)
index_ch12 <- which(my_text == "CHAPTER XII", arr.ind = TRUE)

book_ch1 <- my_text[(index_ch1+1):(index_ch2-1)]
book_ch2 <- my_text[(index_ch2+1):(index_ch3-1)]
book_ch3 <- my_text[(index_ch3+1):(index_ch4-1)]
book_ch4 <- my_text[(index_ch4+1):(index_ch5-1)]
book_ch5 <- my_text[(index_ch5+1):(index_ch6-1)]
book_ch6 <- my_text[(index_ch6+1):(index_ch7-1)]
book_ch7 <- my_text[(index_ch7+1):(index_ch8-1)]
book_ch8 <- my_text[(index_ch8+1):(index_ch9-1)]
book_ch9 <- my_text[(index_ch9+1):(index_ch10-1)]
book_ch10 <- my_text[(index_ch10+1):(index_ch11-1)]
book_ch11 <- my_text[(index_ch11+1):(index_ch12-1)]

dir.create("chapters")
write.table(book_ch1, file = "chapters/book_ch1.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(book_ch2, file = "chapters/book_ch2.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(book_ch3, file = "chapters/book_ch3.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(book_ch4, file = "chapters/book_ch4.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(book_ch5, file = "chapters/book_ch5.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(book_ch6, file = "chapters/book_ch6.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(book_ch7, file = "chapters/book_ch7.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(book_ch8, file = "chapters/book_ch8.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(book_ch9, file = "chapters/book_ch9.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(book_ch10, file = "chapters/book_ch10.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(book_ch11, file = "chapters/book_ch11.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)


# create VCorpus object
book_corpus <- VCorpus(DirSource("chapters", mode = "text"))
str(book_corpus)
inspect(book_corpus)


install.packages("dplyr")
install.packages("tidytext")
library(dplyr)
library(tidytext)

# 10 longest words
book_words <- tidy(book_corpus) %>%
  unnest_tokens(word, text) %>%
  select(id, word) %>%
  mutate(word_length = nchar(word)) %>%
  arrange(desc(word_length))

book_words %>% filter(id=="book_ch1.txt")
book_words %>% filter(id=="book_ch2.txt")
book_words %>% filter(id=="book_ch3.txt")
book_words %>% filter(id=="book_ch4.txt")
book_words %>% filter(id=="book_ch5.txt")
book_words %>% filter(id=="book_ch6.txt")
book_words %>% filter(id=="book_ch7.txt")
book_words %>% filter(id=="book_ch8.txt")
book_words %>% filter(id=="book_ch9.txt")
book_words %>% filter(id=="book_ch10.txt")
book_words %>% filter(id=="book_ch11.txt")

# 10 longest sentences
book_sentences <- tidy(book_corpus) %>%
  unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>%
  select(id, sentence) %>%
  mutate(sentence_length = nchar(sentence)) %>%
  arrange(desc(sentence_length))

book_sentences %>% filter(id == "book_ch1.txt")
book_sentences %>% filter(id == "book_ch2.txt") 
book_sentences %>% filter(id == "book_ch3.txt") 
book_sentences %>% filter(id == "book_ch4.txt") 
book_sentences %>% filter(id == "book_ch5.txt") 
book_sentences %>% filter(id == "book_ch6.txt") 
book_sentences %>% filter(id == "book_ch7.txt") 
book_sentences %>% filter(id == "book_ch8.txt")
book_sentences %>% filter(id == "book_ch9.txt") 
book_sentences %>% filter(id == "book_ch10.txt") 
book_sentences %>% filter(id == "book_ch11.txt") 

btext <- book_corpus[[1]]
btext
btext[1]

bookDTM <- DocumentTermMatrix(book_corpus)
bookDTM
inspect(bookDTM)
str(bookDTM)

book_df <- data.frame(btext[1])
book_df[1]

# remove punctuation and numbers
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
removeNumPunct
bookcl <- tm::tm_map(book_corpus, content_transformer(removeNumPunct))
bookcl
str(bookcl)
inspect(bookcl)

# with lower case
booklow <- tm::tm_map(bookcl, content_transformer(tolower))
booklow
str(booklow)
inspect(booklow)

# computing DTM
bookDTM <- DocumentTermMatrix(booklow)
bookDTM
str(bookDTM)
inspect(bookDTM)

# stopwords
myStopwords <- c(tm::stopwords("en"))
myStopwords
bookStop <- tm::tm_map(booklow, tm::removeWords, myStopwords)
inspect(bookStop[1])

# TDM again
bookStopTDM <- tm::TermDocumentMatrix(bookStop)
bookStopTDM

# frequent terms
freqTerms <- tm::findFreqTerms(bookStopTDM, lowfreq = 5)
freqTerms

booktf <- tm::termFreq(bookStop[[1]])
booktf

inspect(bookStopTDM)

# dendogram
bookdf <- as.data.frame(bookStopTDM[[1]])
bookDist <- dist(bookdf)
bookDG <- hclust(bookDist, method = "ward.D2")
str(bookDG)

plot(bookDG)

# word cloud
install.packages("wordcloud")
library(wordcloud)

words <- names(booktf)
words

pal <- brewer.pal(9, "BuGn")
str(pal)
booksWc <- wordcloud(words, booktf, colors = pal[-(1:4)])
str(booksWc)

pal2 <- brewer.pal(9, "Spectral")
booksWc <- wordcloud(words, booktf, colors = pal2)


install.packages("quanteda")
library(quanteda)
bookText <- bookcl[[1]]
bookText$content[1:10]

bookTokens <- quanteda::tokens(bookText$content[1:10])
str(bookTokens)

booksDFM <- quanteda::dfm(bookTokens)
str(booksDFM)

bookDocFreq <- quanteda::docfreq(booksDFM)
str(bookDocFreq)
bookDocFreq

bookWeights <- quanteda::dfm_weight(booksDFM)
str(bookWeights)
bookWeights

bookingTFIDF <- quanteda::dfm_tfidf(booksDFM, scheme_tf = "count", scheme_df = "inverse")
str(bookingTFIDF)

install.packages("syuzhet")
library(syuzhet)

bookTextDf <- as.data.frame(bookText$content)
bookTextDf

bookAsStings <- get_text_as_string("APrincessOfMars.txt")
bookAsStings

bookSentences <- get_sentences(bookAsStings)
bookSentences
str(bookSentences)

bookSentiment <- get_sentiment(bookSentences, "syuzhet")
bookSentiment

bookSBing <- get_sentiment(bookSentences, "bing")
bookSBing

BSDictionary <- get_sentiment_dictionary()
BSDictionary

BSDictionaryBing <- get_sentiment_dictionary("bing")
BSDictionaryBing

BSSum <- sum(bookSentiment)
BSSum

BSBingSum <- sum(bookSBing)
BSBingSum

BSMean <- mean(bookSentiment)
BSMean

BSBingMean <- mean(bookSBing)
BSBingMean

summary(BSBingMean)

plot(bookSentiment, main = "Book text plot trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(bookSentiment, main = "Book text plot trajectory: Bing", xlab = "Narrative", ylab = "Emotional Valence")

BSSentimentPctValue <- get_percentage_values(bookSentiment, bins = 10)
structure(BSSentimentPctValue)
plot(BSSentimentPctValue, main = "Book text PCT value 10 bins", xlab = "Narrative", ylab = "Emotional Valence", col = "blue")


BSSentimentPctValue <- get_percentage_values(bookSentiment, bins = 20)
structure(BSSentimentPctValue)
plot(BSSentimentPctValue, main = "Book text PCT value 10 bins", xlab = "Narrative", ylab = "Emotional Valence", col = "blue")


# chapter wise sentimental analysis
for (i in 1:length(book_corpus)) {
  doc <- content(book_corpus[i])
  chapSentences <- get_sentences(doc[[1]]$content)
  chapSent <- get_sentiment(chapSentences, method = "syuzhet")
  print(paste("Chapter ", i))
  print(chapSent)
  plot(chapSent, main = "Chap Text plot trajectory", xlab = "Narrative", ylab = "Emotional Valence")
}

# 3 functions of each package
# stringi package
sentence2 <- bookcl[[1]]$content[2]
sentence2

strings <- stringi::stri_split_fixed(sentence2, "THE")
stringi::stri_sort(strings[[1]])

stringi::stri_count_regex(sentence2, "T.")

sentence2
stringi::stri_trans_tolower(sentence2)

# quanteda
bookTokens1 <- quanteda::tokens(bookText$content)
kwic(bookTokens1, pattern = "remarkable")
kwic(bookTokens1, pattern = "independent")

dfm1 <- dfm(bookTokens)
dfm1

topfeatures(dfm1, 20)














