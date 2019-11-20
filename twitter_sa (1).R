#install the required packages
install.packages("twitteR", dependencies=TRUE)
install.packages("RCurl")
install.packages('bitops')
install.packages('base64enc')
install.packages('httpuv')
install.packages('tm')
install.packages("stringr")


libs = c("twitteR", "RCurl", "bitops", "base64enc", "httpuv", "tm", "wordcloud", "stringr")
lapply(libs, require, character.only=TRUE)

options(stringsAsFactors = FALSE)

file="C:/Users/Hp/Desktop/twitterOauth.txt"
OauthCreds = read.table(file, header=T)
OauthCreds

setup_twitter_oauth(OauthCreds$consumer_key,
                    OauthCreds$consumer_secret,
                    OauthCreds$access_token,
                    OauthCreds$access_secret) 
yes


searchTerms = c("Samsung + India")

set.seed(1234)
pos = scan('C:/Users/Hp/Desktop/positive-words.txt',
           what='character',
           comment.char=';')

class(pos)
str(pos)

neg = scan('C:/Users/HP/Desktop/negative-words.txt',
           what='character',
           comment.char=';')

class(neg)
str(neg)

numberOfTweets=1100

tweets_list = searchTwitter(searchTerms,lang="en",n=numberOfTweets,resultType="recent")

tweets_text = sapply(tweets_list, function(x) x$getText())

tweets_corpus = Corpus(VectorSource(tweets_text))

inspect(tweets_corpus[1:3])

tweets_corpus_clean = tm_map(tweets_corpus, removePunctuation)

tweets_corpus_clean = tm_map(tweets_corpus_clean, stripWhitespace)

tweets_corpus_clean = tm_map(tweets_corpus_clean, removeNumbers)

tweets_corpus_clean = tm_map(tweets_corpus_clean, removeWords, stopwords("english"))

tweets_corpus_clean = tm_map(tweets_corpus_clean, content_transformer(tolower))

toSpace = content_transformer(function(x, pattern) gsub(pattern,"",x))

tweets_corpus_clean = tm_map(tweets_corpus_clean, toSpace,"https*|youtu*")

return(tweets_corpus_clean)

tweets_tdm = TermDocumentMatrix(tweets_corpus_clean)
tweets_tdm = as.matrix(tweets_tdm)

tdm_term_freq_sort = sort(rowSums(tweets_tdm), decreasing=TRUE)
tdm_term_freq_sort_inc = sort(rowSums(tweets_tdm), decreasing=FALSE)

tdm_term_freq_df = data.frame(word = names(tdm_term_freq_sort),
                              freq = tdm_term_freq_sort)

tweet_words = tdm_term_freq_df$word
str(tdm_term_freq_df)

pos.matches = match(tweet_words, pos)
head(pos.matches)

neg.matches = match(tweet_words, neg)
head(neg.matches)

pos.matches = !is.na(pos.matches)
neg.matches = !is.na(neg.matches)
score = sum(pos.matches)-sum(neg.matches)
score








