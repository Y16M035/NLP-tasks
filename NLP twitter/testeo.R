library(rJava)
.jinit(parameters = "-Xmx4g")
library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(tm)

b <- stemDocument(tolower(readline("Word: ")))
