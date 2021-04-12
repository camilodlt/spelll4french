
#.datatable.aware=TRUE
# @imports data.table

proper_noun<- function(word){
  upper_word=stringr::str_to_title(word)
  return(upper_word)
}
#' @export
check_proper_dictionary<- function(word,dictionary=dict){
  proper= proper_noun(word)
  #if(proper %in% dict$Flexion) {return(TRUE)} else {return(FALSE)}
  # With DT
  subset<-dictionary[Flexion==proper][['Flexion']]
  if(!identical(subset, character(0))){return(TRUE)} else {return(FALSE)}
}

#' @export
check_dictionary<- function(word,dictionary=dict){

  #if(word %in% dict$Flexion) {return(TRUE)} else {return(FALSE)}

  # With DT
  subset<-dictionary[Flexion==word][['Flexion']]
  if(!identical(subset, character(0))){return(TRUE)} else {return(FALSE)}

}

#
check_words<- function(word,dictionary= dict, n_transformations='single_trans',
                       fun=c('transposes','insertions','deletes','replaces'),...){
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

    # Apply_depth
    if(n_transformations=='single_trans'){ # single_trans will use apply_depth
    results = apply_depth(word = word,fun=fun[1],...) # first function will be used if multiple or a single are provided
    results=unique(unlist(results))
    # DT
    results= data.frame(Flexion=results)
    setDT(results,key = 'Flexion')
    subset<-dictionary[results,nomatch=0]
      # Check emptyness
    if(identical(subset, character(0))){return(word)} else {subset[['Flexion']]}
    } else

    if(n_transformations=='multiple_trans'){
      results = apply_depth_multiple(word = word,funs=fun,...) # transformations must be provided
      results=unique(unlist(results))
      # DT
      results= data.frame(Flexion=results)
      setDT(results,key = 'Flexion')
      subset<-dictionary[results,nomatch=0]
      # Check emptyness
      if(identical(subset, character(0))){return(word)} else {subset[['Flexion']]}
    }
}}
