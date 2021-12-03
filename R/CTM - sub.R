library(topicmodels)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(readxl)
library(tm)
library(SnowballC)
library(stringr)


#reading
wos_data <- read_excel(path  = 'C:/Users/micky/OneDrive/Desktop/Tesi DS/data/wos_data.xlsx')

#sample 10% of the total dataset
set.seed(0)
wos_data <- sample_frac(tbl = wos_data, size = 0.1, replace = FALSE)

#re-sample 10% of the dataset already sampled (10%)
set.seed(0)
wos_data <- sample_frac(tbl = wos_data, size = 0.1, replace = FALSE)


#remove null values
wos_data <- wos_data[wos_data[, "Abstract"] != "",]

#take column for analysis
dataset <- wos_data$Abstract

#corpus creation
text_corpus <- VCorpus(VectorSource(dataset))

#remove url
removeUrl <- content_transformer(function(x){
  gsub("http\\S+", " ", x)
})
text_corpus <- tm_map(text_corpus, removeUrl)
writeLines(as.character(text_corpus[[1]]))
inspect(text_corpus[[1]])

#remove numbers
remove_numbers <- content_transformer(function(x){
  gsub("[0-9]+", " ", x)
})
text_corpus <- tm_map(text_corpus, remove_numbers)
text_corpus[[1]]$content


#lowercase
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus[[1]]$content

#remove all special chars (and the puntuaction)
removeSpecialChars <- content_transformer(function(x, pattern) gsub("[^a-zA-Z0-9 ]"," ",x))

text_corpus <- tm_map(text_corpus, removeSpecialChars)
text_corpus[[1]]$content

#remove single / double letters
removeLetters <- content_transformer(function(x, pattern) gsub('\\b\\w{1,2}\\s'," ",x))
text_corpus <- tm_map(text_corpus, removeLetters)
text_corpus[[1]]$content

#strip whitespace
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus[[1]]$content

#remove stopwords
my_stopwords <- stopwords("english")
text_corpus <- tm_map(text_corpus, removeWords, my_stopwords)
text_corpus[[1]]$content


#strip whitespace
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus[[1]]$content

#tokenization
tokenizer <- content_transformer(function(x) Boost_tokenizer(x))
text_corpus <- tm_map(text_corpus, tokenizer)
text_corpus[[1]]$content

#stemming
text_corpus <- tm_map(text_corpus, stemDocument, language="english")
text_corpus[[1]]$content

#create doc-term matrix
dtm <- DocumentTermMatrix(text_corpus, )
inspect(dtm)

#-----------------------------------------------------------------------------------
#Set parameters of the CTM model (Topicmodels)

control_CTM_VEM <- list(seed = 999, verbose = 1, 
                        var = list(tol = 10^-4), 
                        em = list(tol = 10^-3))

#Run the CTM models and take execution time
system.time({
  run1=CTM(dtm, k=10, control=control_CTM_VEM)
})

top_20 <- data.frame(terms(run1, k=20))

#get correlations
mat1 <- build_graph(x=run1, lambda = .1, and=T)
par(mar=c(8.1, 4.1, 4.1, 4.1))   # adapt margins
plot(mat1, xlab="Topics", ylab="Topics", col = c("white", "darkred"))
