proper_noun<- function(word){
  upper_word=stringr::str_to_title(word)
  return(upper_word)
}

check_proper_dictionary<- function(word,dictionary=dict){
  proper= proper_noun(word)
  #if(proper %in% dict$Flexion) {return(TRUE)} else {return(FALSE)}
  # With DT
  subset<-dictionary[Flexion==proper][['Flexion']]
  if(!identical(subset, character(0))){return(TRUE)} else {return(FALSE)}
}

check_dictionary<- function(word,dictionary=dict){

  #if(word %in% dict$Flexion) {return(TRUE)} else {return(FALSE)}

  # With DT
  subset<-dictionary[Flexion==word][['Flexion']]
  if(!identical(subset, character(0))){return(TRUE)} else {return(FALSE)}

}

check_words<- function(word,dictionary= dict,...){
# word in dict
  word_in_dict=check_dictionary(word,dictionary)
  proper_in_dict=check_proper_dictionary(word,dictionary)

# Upper case needed
  if(!word_in_dict & proper_in_dict){
    return(stringr::str_to_title(word))
  }
#
  if(word_in_dict)
    # independly if proper in dict or not, return the original word
  {return(word)}

# decision
  if(!word_in_dict & !proper_in_dict){
    results = apply_depth(word = word,...)
    results=unique(unlist(results))
    # DT
    results= data.frame(Flexion=results)
    setDT(results,key = 'Flexion')
    subset<-dictionary[results,nomatch=0]
      # Check emptyness
    if(identical(subset, character(0))){return(word)} else {subset[['Flexion']]}
    #in_dict=purrr::map_lgl(results, check_dictionary, dictionary)
    #indices= which(in_dict)
    #extract = results[indices]
    #return(extract)
  }
}
