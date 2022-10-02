library(rJava)
.jinit(parameters = "-Xmx4g")
library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(tm)

source.pos = DirSource("donald trump", encoding = "UTF-8")

corpus = Corpus(source.pos)

# remove URLs
removeURL <- function(x)
  gsub("http.*", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))

tdm = TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf,
    stopwords = T,
    removePunctuation = T,
    removeNumbers = T,
    stemming = T
  )
)
yes<-"yes"
while(yes=="yes"){
  word <- stemDocument(tolower(readline("Insert term to study:   ")))
  cat("\nInsert correlation percentage")
  corlimit <- as.numeric(readline("Value must be between 0 and 1:   "))
  while(0>corlimit | corlimit>1){
    cat("\nWrong input")
    corlimit <- as.numeric(readline("Value must be between 0 and 1:   "))
  }
  asoc <- (findAssocs(tdm, word, corlimit))[[1]]
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  verbs <- list()
  nouns <- list()
  adjectives <- list()
  if(length(asoc)<1){
    cat("\nSorry, the term \"", word, "\" doesn't have any correlations at the given percentage", sep="")
    yes<-readline("Do you want to change the parameters? say yes:    ")
    next
  }
  for (i in 1:length(asoc)) {
    name <- names(asoc[i][1])
    cor <- asoc[[i]][[1]]
    a <-
      annotate(name,
               list(
                 sent_token_annotator,
                 word_token_annotator,
                 pos_tag_annotator
               ))
    type <- a[[length(a)]]$features[[1]][[1]]
    if (type == "JJ" | type == "JJS" | type == "JJR") {
      adjectives <- append(adjectives, list(list(name, cor)))
    } else if (type == "NN" |
               type == "NNS" | type == "NNP" | type == "NNPS") {
      nouns <- append(nouns, list(list(name, cor)))
    }
    else if (type == "VB" |
             type == "VBD" |
             type == "VBG" | type == "VBN" | type == "VBP" | type == "VBZ") {
      verbs <- append(verbs, list(list(name, cor)))
    }
  }
  
  cat("\nLIST OF MOST RELATED NOUNS\n")
  if((n<-length(nouns))>0){
    for (i in 1:n){
      cat(i,". ", nouns[[i]][[1]], ": with a correlation of ", nouns[[i]][[2]], "\n", sep="")
    }
  }else{
    cat("no significant nouns found\n")
  }
  
  cat("\nLIST OF MOST RELATED VERBS\n")
  if((n<-length(verbs))>0){
    for (i in 1:n){
      cat(i,". ", verbs[[i]][[1]], ": with a correlation of ", verbs[[i]][[2]], "\n", sep="")
    }
  }else{
    cat("no significant verbs found\n")
  }
  
  cat("\nLIST OF MOST RELATED ADJECTIVES\n")
  if((n<-length(adjectives))>0){
    for (i in 1:n){
      cat(i,". ", adjectives[[i]][[1]], ": with a correlation of ", adjectives[[i]][[2]], "\n", sep="")
    }
  }else{
    cat("no significant adjectives found\n")
  }
  yes<-readline("\nDo you want to search another association? say yes:    ")
}
