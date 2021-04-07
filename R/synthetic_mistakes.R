#splits = [(word[:i], word[i:]) for i in range(len(word) + 1)]

splits = function(word){

  #stopifnot(length(word)==1)

  chars=strsplit(word,split="")[[1]]

  size=length(chars)

  start= list()
  stop=list()

  for (i in 1:size){
    if((i-1)>0){
      split_left=chars[1:(i-1)]
      start=append(start,paste(split_left, collapse = ""))
    }
    if((i)>1){
      split_right= chars[(i):size]
      stop=append(stop,paste(split_right, collapse = ""))

    }

  }

  biglist=mapply(c, start,stop, SIMPLIFY=FALSE)
  return(biglist)


}

#map(unlist(list(c("hola","chao","yes")),recursive = F),~splits(.))[[1]]

