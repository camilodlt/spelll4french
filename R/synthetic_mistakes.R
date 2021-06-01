# SPLITS ------
splits = function(word){
  if(nchar(word)>1){
  chars=strsplit(word,split="")[[1]]

  size=length(chars)

  start= expandingList()
  stop=expandingList()

  for (i in 1:size){
    if((i-1)>0){
      split_left=chars[1:(i-1)]
      start$add(paste(split_left, collapse = ""))
      #start=append(start,paste(split_left, collapse = ""))
    }
    if((i)>1){
      split_right= chars[(i):size]
      stop$add(paste(split_right, collapse = ""))
      #stop=append(stop,paste(split_right, collapse = ""))

    }

  }

  biglist=mapply(c, start$as.list(),stop$as.list(), SIMPLIFY=FALSE)
  return(biglist)

} else {return(word)}}
# DELETES ------
deletes = function(word){
  if(nchar(word)>1){
    chars=strsplit(word,split="")[[1]]

    size=length(chars)

    deletions= expandingList()

    for(i in 1:size){
      deletions$add(paste(chars[-i], collapse = ""))
      #deletions= append(deletions,paste(chars[-i], collapse = ""))
    }

    return(deletions$as.list())
  } else { return(word)}
  }
# TRANSPOSES ------
transposes <- function(word){
  if(nchar(word)>1){
  chars=strsplit(word,split="")[[1]]

  size=length(chars)

  transposes_list = expandingList()

  for(i in 1:size){
    text= paste(
      c(
        if((i-1)<=size & (i-1)>0 ){chars[1:(i-1)]}, # begin
        if((i+1)<=size){chars[(i+1)]}, # transpose 1
        if((i)<=size){chars[i]}, # transpose 2
        if((i+2)<=size){chars[(i+2):size]}), # end
      collapse = "")
    if(text!= word){
      transposes_list$add(text)}
      #transposes_list= append(transposes_list,text)}
  }
return(transposes_list$as.list())
} else {return(word)}

}
# REPLACES ------
replaces<- function(word,letters=tokens){
  if(nchar(word)==0){
    return(word)
  }
  if(nchar(word)==1){
    replaces_list = list(tokens[!tokens%in% word])
    return(replaces_list)}
  if(nchar(word)>1){
  chars=strsplit(word,split="")[[1]]
  size=length(chars)

  begin= stringi::stri_sub(word, to=1:size)
  end= stringi::stri_sub(word, from=2:size)

  begin= as.list(begin)
  end=as.list(end)
  end[[size]]<- character(0)

  # repeat
  begin<-purrr::map(begin,~rep(.x,length(tokens)))
  # replace last character of every repeated word by a token
  begin<-purrr::map(begin, ~stringi::stri_sub_replace(.x,from=nchar(.x[1]),nchar(.x[1]),replacement = tokens))
  # paste with ends
  replaces_list <-purrr::map2(begin,end,paste0)
  return(replaces_list)
  }

  }
# INSERTS ------
insertions<-function(word,letters=tokens){
  if (nchar(word) >=1){
  chars=strsplit(word,split="")[[1]]
  size=length(chars)
  Size<-size+1
  begin=expandingList(Size)
  end= expandingList(Size)
  for(i in 1:Size){
    temp_begin= if((i-1)==0){character(0)}else{chars[1:i-1]}
    temp_end=if(i>size){character(0)}else{chars[i:size]}

    # paste
    temp_begin= paste0(temp_begin,collapse = '')
    temp_end= paste0(temp_end,collapse = '')

    # append
    begin$add(temp_begin)
    end$add(temp_end)
    # begin= append(begin,temp_begin)
    # end= append(end,temp_end)
  }
  begin<- begin$as.list()
  end<- end$as.list()
  # Combine with Tokens
  for(i in 1:length(begin)){
    if(i==1){
      begin[[i]]<- tokens
    }
    if(i>1){
      begin[[i]]<- paste0(begin[[i]],tokens)
    }
  }
  # end result
  insertions_list <-purrr::map2(begin,end,paste0)
  return(insertions_list)
  }

}

# APPLY_DEPTH ------
apply_depth<- function(depth=1, fun='transposes', word, warm.start=0, results=NULL){
  function_name= fun
  fun_eval=eval(parse(text =fun ))
  depth=depth+1
  # if called directly and word provided
  if(warm.start==0){
  temp= expandingList()
  temp$add_named(word,'orig_word')
  # temp[[1]]= word

  for(i in 2:depth){
    temp_result=unlist(purrr::map(temp$as.list()[[(i-1)]],fun_eval))
    temp_result= unique(temp_result)
    # append
    temp_name= paste0(function_name,(i-1))
    temp$add_named(temp_result,temp_name)
    # temp[[temp_name]]<- temp_result

  }
  # temp[[1]]<- NULL
  # results=temp
  results<- temp$as.list()
  results[[1]]<- NULL
  } else
  # if called by apply multiple
  {
    l<- results$as.list()
    names_new_list<- names(l)
    #results is provided
    for(i in 2:depth){
      temp_result=purrr::map( # every sublist
        l[names_new_list],~unique(unlist(purrr::map(# every word
          .,fun_eval)
          )))
      # names for each sublist
      over=names_new_list # old names
      # Concatenated names
      names_new_list= paste0(substr(function_name,1,4),(i-1),'_',over)
      #Set names
      temp_result<-purrr::set_names(temp_result,nm =names_new_list)
      # remove NULLS
      temp_result<- discard(temp_result, is_null)
      # Unique
      temp_result<- temp_result[!duplicated(temp_result)] # unique() drops names
      # correct names, if dropped for next iter
      names_new_list<- names(temp_result)
      # append
      # results=append(results,temp_result)
      results$add(temp_result)
    }

  }
  # results<- results[!duplicated(results)]
  results$rm_duplicated()
  results$reduce(n=500)
  return(results)
}
# APPLY_DEPTH_MULTIPLE ------
apply_depth_multiple<- function(funs,word,ret1){

  results= expandingList()
  results$add_named(word, "orig_word")
  # results= list()
  # results[["orig_word"]]= word
  index=0

  for(i in funs){
    # temp_list= apply_depth(fun=i,word = word, warm.start = 1,results = results)
    apply_depth(fun=i,word = word, warm.start = 1,results = results)
    index=index+1
    #results= temp_list
  }
  if(ret1==TRUE){
  results$sample1()
  } else{results$as.list()}
}
# SIMPLE_APPLY ------
simple_apply<- function(depth=1, fun='transposes', word, warm.start=0, results=NULL){
  # get the function to eval
  function_name= fun
  fun_eval=eval(parse(text =fun ))
  # initialize parameter
  depth=depth+1
  # if called directly and word provided
  if(warm.start==0){
    temp= list()
    temp[[1]]= word

    for(i in 2:depth){
      temp_result=unlist(fun_eval(temp[[i-1]]))
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
      temp_result=unique(fun_eval(temp[[i-1]])
      )
      # names for each sublist
      over=names_new_list # old names

      # Concatenated names
      names_new_list=paste0(substr(function_name,1,4),(i-1),'_',over)
      #Set names
      temp_result<-purrr::set_names(temp_result,nm =names_new_list)

      # append
      results=append(results,temp_result)
    }

  }
  return(results)
}
# on dictionnary
  #Prefer split first
##

