
source('~/Documents/Own Projects/github/spelll4french/R/synthetic_mistakes.R')
decider <- function(x, sep= " ",...){
  rand<-sample(0:1, size = 1, prob = c(0.5, 0.5))
  if(rand==0){
    return(x)
  }
  else {
    words<-unlist(strsplit(x, sep))
    synthetic<-map_chr(words,~ decider_word(.),...)
    synthetic<- paste0(synthetic, collapse = sep)
    return(synthetic)
  }
}
decider_f <- function(x, sep= " ",...){
  rand<-sample(0:1, size = 1, prob = c(0.5, 0.5))
  if(rand==0){
    return(x)
  }
  else {
    words<-unlist(strsplit(x, sep))
    synthetic<-future_map_chr(words,~ decider_word(.),...)
    synthetic<- paste0(synthetic, collapse = sep)
    if(is.null(synthetic)){synthetic<-'ERROR OCCURED'}
    return(synthetic)
  }
}

de_split <- function(x){
  return(unlist(map(splits(x), ~paste(., collapse = ' '))))
}
decider_word<- function(x,prob= 0.3){
  rand<-sample(0:1, size = 1, prob = c(1-prob, prob))
  if(rand==0){
    return(x)
  } else {
    functions<- c("de_split","deletes","transposes","replaces","insertions")
    to_sample<- sample(1:5)
    sampled<- sample(functions, to_sample)
    mistakes <- apply_depth_multiple(sampled, x)
    mistakes[['orig_word']]<- NULL
    mistakes<- map(mistakes, ~sample(x = ., size = length(mistakes[[1]]), replace = T))
    mistakes= unlist (mistakes)
    mistake= sample(mistakes, size = 1)
    if(is.null(mistake)){mistake<-'ERROR OCCURED'}
    return(mistake)
    }
}

#tokens<- letters
