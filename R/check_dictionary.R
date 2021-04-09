proper_noun<- function(word){
  upper_word=stringr::str_to_title(word)
  return(upper_word)
}

check_proper_dictionary<- function(word,dictionary=dict){
  proper= proper_noun(word)
  if(proper %in% dict$Flexion) {return(TRUE)} else {return(FALSE)}
}

check_dictionary<- function(word,dictionary=dict){

  if(word %in% dict$Flexion) {return(TRUE)} else {return(FALSE)}

}

check_words<- function(word,dictionary= dict,...){
# word in dict
  word_in_dict=check_dictionary(word,dictionary)
  proper_in_dict=check_proper_dictionary(word,dictionary)

# Upper case needed
  if(!word_in_dict& proper_in_dict){
    return(stringr::str_to_title(word))
  }
#
  if(word_in_dict )
  {return(word)}

# decision
  if(!word_in_dict & !proper_in_dict){
    results = apply_depth(word = word,...)
    results=unique(unlist(results))
    in_dict=purrr::map_lgl(results, check_dictionary, dictionary)
    indices= which(in_dict)
    extract = results[indices]
    return(extract)
  }
}
