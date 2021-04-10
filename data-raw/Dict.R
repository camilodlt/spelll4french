## code to prepare `Dict` dataset goes here

library(dplyr)

x<-read.delim("data/lexique-grammalecte-fr-v7.0.txt",
              header = T,sep="\t",skip=14,encoding = "UTF-8")

# Normalize function

normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

# Select useful columns
x<-x%>% select(Flexion,Lemme,Notes,Total.occurrences)

# Remove non unique words (keep the one with highest frequency)
x<-x%>% arrange(desc(Total.occurrences))%>% distinct(Flexion,.keep_all=T)

# Ensure frequency is at least 1. Augment frequency by one.
x$Total.occurrences<- x$Total.occurrences+1

# Normalize frequency count
x$Total.occurrences<- normalize(x$Total.occurrences)

dict<- x

# DT
setDT(dict,key='Flexion')
usethis::use_data(dict,internal = TRUE,overwrite = TRUE)
