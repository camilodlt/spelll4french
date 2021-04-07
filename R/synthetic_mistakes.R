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


#deletes = [L + R[1:] for L, R in splits if R]

deletes = function(word){

    chars=strsplit(word,split="")[[1]]

    size=length(chars)

    deletions= list()

    for(i in 1:size){
      deletions= append(deletions,paste(chars[-i], collapse = ""))
    }
    return(deletions)
}

# transposes = [L + R[1] + R[0] + R[2:] for L, R in splits if len(R)>1]


transposes <- function(word){

  chars=strsplit(word,split="")[[1]]

  size=length(chars)

  transposes_list = list()

  for(i in 1:size){
    text= paste(
      c(
        if((i-1)<=size & (i-1)>0 ){chars[1:(i-1)]}, # begin
        if((i+1)<=size){chars[(i+1)]}, # transpose 1
        if((i)<=size){chars[i]}, # transpose 2
        if((i+2)<=size){chars[(i+2):size]}), # end
      collapse = "")
    if(text!= word){
    transposes_list= append(transposes_list,text)}
  }
return(transposes_list)
}



# replaces   = [L + c + R[1:]           for L, R in splits if R for c in letters]

# for the moment: possible letters:
letters    = 'abcdefghijklmnopqrstuvwxyz'
letters= strsplit(letters,split="")[[1]]


replaces <- function(word){

  chars=strsplit(word,split="")[[1]]

  size=length(chars)

  replaces_list = list()

  for(i in 1:size){
    begin= if((i-1)>0) {paste(chars[1:(i-1)], collapse = "")}

    end = if((i+1)<=size) {paste(chars[(i+1):size],collapse ="")}

    if(!is.null(end)){
    combinations= purrr::cross(list(letters,end))

    concatenations= unlist(purrr::map(combinations,~paste(purrr::flatten_chr(.), collapse="")))
    replaces_list[[i]]= concatenations
    }

    if(!is.null(begin) & !is.null(end)){
      replaces_list[[i]]= paste0(begin, replaces_list[[i]])
    }

    if(!is.null(begin) & is.null(end)){

      combinations= purrr::cross(list(begin,letters))

      concatenations= unlist(purrr::map(combinations,~paste(purrr::flatten_chr(.), collapse="")))

      replaces_list[[i]]= concatenations
    }
  }
  return(replaces_list)
}

apply_depth<- function(depth=1, fun=transposes, word){
  depth=depth+1
  results= list()
  results[[1]]= word
  for(i in 2:depth){
    results[[i]]=unlist(purrr::map(results[[i-1]],fun))
  }
  return(results)
}

apply_depth_multiple<- function(funs,word){
  results= list()
  results[[1]]= word
  for(i in funs){
  fun=eval(parse(text =i ))
  #results[[name]]<- apply_depth(fun)
  }
  apply_depth
}

#map(unlist(list(c("hola","chao","yes")),recursive = F),~splits(.))[[1]]

x<- letters

