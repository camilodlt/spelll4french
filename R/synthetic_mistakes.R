
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

replaces <- function(word,letters=tokens){

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


# INSERTS
insertions<-function(word,letters=tokens){

  chars=strsplit(word,split="")[[1]]

  size=length(chars)

  insertions_list = list()

  for(i in 1:(size+1)){

    if(i == 1) { # first it. No begin
     begin= NULL
     end= paste(chars,collapse = "")
    }

    if (i %in% 2:(size+1)){
    begin= paste(chars[1:(i-1)], collapse = "")

    end = if(i<=size) {paste(chars[i:size],collapse ="")}
    }

    if(!is.null(end)){ # if the is an end
      combinations= purrr::cross(list(letters,end))

      concatenations= unlist(purrr::map(combinations,~paste(purrr::flatten_chr(.), collapse="")))
      insertions_list[[i]]= concatenations
    }

    if(!is.null(begin) & !is.null(end)){ # if there is both
      insertions_list[[i]]= paste0(begin, insertions_list[[i]])
    }

    if(!is.null(begin) & is.null(end)){ # if there is only a beginning

      combinations= purrr::cross(list(begin,letters))

      concatenations= unlist(purrr::map(combinations,~paste(purrr::flatten_chr(.), collapse="")))

      insertions_list[[i]]= concatenations
    }
  }
  return(insertions_list)
}


###

apply_depth<- function(depth=1, fun='transposes', word, warm.start=0, results=NULL){
  function_name= fun
  fun_eval=eval(parse(text =fun ))
  depth=depth+1
  # if called directly and word provided
  if(warm.start==0){
  temp= list()
  temp[[1]]= word

  for(i in 2:depth){
    temp_result=unlist(purrr::map(temp[[(i-1)]],fun_eval))
    temp_result= unique(temp_result)
    # append
    temp_name= paste0(function_name,(i-1))
    temp[[temp_name]]<- temp_result

  }
  temp[[1]]<- NULL
  results=temp
  } else
  # if called by apply multiple
  {
    names_new_list<- names(results)
    #results is provided
    for(i in 2:depth){
      temp_result=unique(purrr::map( # every sublist
        results[names_new_list],~unique(unlist(purrr::map(# every word
          .,fun_eval)
          )))
        )
      # names for each sublist
      over=names_new_list # old names

      # Concatenated names
      names_new_list= names_new_list=paste0(substr(function_name,1,4),(i-1),'_',over)
      #Set names
      temp_result<-purrr::set_names(temp_result,nm =names_new_list)

      # append
      results=append(results,temp_result)
    }

  }
  return(results)
}




apply_depth_multiple<- function(funs,word){

  results= list()
  results[["orig_word"]]= word

  index=0

  for(i in funs){
    temp_list= apply_depth(fun=i,word = word, warm.start = 1,results = results)
    index=index+1
    #if(index==1){results[["orig_word"]]<- NULL}
    results= temp_list
  }
  results
}

# on dictionnary
  #Prefer split first
##


