#source('~/Documents/Own Projects/github/spelll4french/R/synthetic_mistakes.R')
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
# decider_f <- function(x, sep= " ",...){
#   rand<-sample(0:1, size = 1, prob = c(0.5, 0.5))
#   if(rand==0){
#     return(x)
#   }
#   else {
#     words<-unlist(strsplit(x, sep))
#     synthetic<-future_map_chr(words,~ decider_word(.),...)
#     synthetic<- paste0(synthetic, collapse = sep)
#     if(is.null(synthetic)){synthetic<-'ERROR OCCURED'}
#     return(synthetic)
#   }
# }

#functions<- c("de_split","deletes","transposes","replaces","insertions")
decider_word<- function(x,prob= 0.3, functions){
  rand<-sample(0:1, size = 1, prob = c(1-prob, prob))
  if(rand==0){
    return(x)
  } else {
    #to_sample<- sample(1:5,1) #
    to_sample<- sample(1:6,1, prob=c(0.5, 0.3, 0.15,0.05,0.0,0.0)) # more natural mistakes
    if(to_sample==1){
      sampled<- functions[to_sample]
    } else{
      sampled<- sample(functions, to_sample)
    }
    print(sampled)
    mistake <- spell4french:::apply_depth_multiple(sampled, x,ret1=TRUE)
    return(mistake)
    }
}
