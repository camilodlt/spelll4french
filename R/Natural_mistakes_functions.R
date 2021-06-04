###############################################################################
#                NATURAL ("FAT FINGERS") MISTAKES  FUNCTIONS                  #
###############################################################################

# FUNCTIONS ------
# natural mistake fun
  # bonjour=> bonjoiur

# Split Word to char ---
chars<- word_to_char(word = word, ...)

# Pass a function and a word
word_to_char<- function(word,use_default=TRUE, fun_word_splitter=FALSE){
  if(use_default==TRUE){
    fun_word_splitter<-function(x){strsplit(x,split="")[[1]]}
    }

  if(nchar(word)>1){
    chars=fun_word_splitter(word)
    size= length(chars)
  }
  return(list("chars"=chars,"size"=size))
}
# Natural_mistake_decider
natural_mistake_decider<-function(char,char_modification_prob){

  # Decide if a word is going to be modified
  modification<- c(0,1)
  modify_decision<- sample(modification, size=1,prob=char_modification_prob)
  return(modify_decision)
}
# neighbor_size decider
neighbor_size_decider <- function(max_neigbor, prob) {
  n_neighbors <- 1:max_neigbor
  neighbors_size<- sample(n_neighbors,size=1,prob = prob)
  return(neighbors_size)
}
# fat finger decider
fat_finger_decider <- function(fat_finger){
  fat_finger_decision<-sample(c(FALSE,TRUE),size=1, prob = c(1-fat_finger, fat_finger))
  return(fat_finger_decision)
}

# Modify if requested, pass if requested.
natural_mistake_modifier <- function(char,modify_decision, dict=natural_dict_unnested_DT, neighbor_size=1,
                                     max_neighbor_size= 0,
                                     sample_neighbor_prob= numeric(max_neighbor_size),
                                     fat_finger= 1 ){
  # if it's going to be modified, look for neighbors
  if(modify_decision==1){
    # is a correct char == has a neighbor/s ?
    neighbors= dict[letters == char]%>% pull(neighbor) # empty if char not in dict
    if(!purrr::is_empty(neighbors)){

      # Neighbor size is to be sampled (not fixed)
      # if sampled, neighbor_size parameter is ignored.
      if(max_neighbor_size > 0){
        neighbor_size<-neighbor_size_decider(max_neighbor_size, prob=sample_neighbor_prob)
      }
      # verify enough neighbors for neighbor_size
      if(neighbor_size>length(neighbors)){neighbor_size<-1}
      # sample neighbor_size of neighbors
      neighbor_char=sample(neighbors,size = neighbor_size)
      # concatenate if multiple neighbors
      if(neighbor_size>1){neighbor_char<-paste(neighbor_char, collapse = "")}
      # Fat finger effect => char + neighbor/s
      fat_finger_decision <- fat_finger_decider(fat_finger)
      if(isTRUE(fat_finger_decision)){neighbor_char<- paste0(char,neighbor_char, collapse = "")}
      # return neighbors
      return(neighbor_char)
    }
    # Char not in dict
    else { return(char)}
    # Modification will not take place => return char
  } else {return(char)}
}
# Natural_mistake_azerty
natural_mistake_azerty <- function(char, char_modification_prob,
                                   neighbor_size=1,
                                   max_neighbor_size= 0,
                                   sample_neighbor_prob= numeric(max_neighbor_size),fat_finger=1,...){

  #Arguments for natural_mistake_decider
  decision_args <- list()
  decision_args$char<- char
  decision_args$char_modification_prob<- char_modification_prob
  # Decide to modify or not the char
  modify_decision <- do.call(natural_mistake_decider,decision_args)

  # Depending on the decision it may return a neighbor or not.
  # if decision == 1, one (or more, depending on neighbor_size) of the neighbors will be returned
  # if decision == 0, char returned (pass)
  # if char not in dict, char returned (pass)
  returning_char <- natural_mistake_modifier(char,modify_decision=modify_decision,neighbor_size=neighbor_size,
                                             max_neighbor_size=max_neighbor_size,
                                             sample_neighbor_prob=sample_neighbor_prob,...) # dots for dict ?

  return(returning_char)
}

# ITERATOR ---
natural_mistakes<- function(word, return_fun= FALSE,args_splitter=list(use_default=TRUE),...){
  # Args natural_mistake_azerty ---
  args_natural_mistakes <- list()
  args_natural_mistakes$char = ".x"
  natural_mistake_azerty_dots <- list(...)
  args_natural_mistakes[names(natural_mistake_azerty_dots)]<- natural_mistake_azerty_dots

  # Args splitter
  args_splitter$word<- word
  callable_word_to_char<- function(){do.call(word_to_char, args_splitter)}
  #
  callable_fun <- function(char){
    args_natural_mistakes["char"]<- char
    do.call(natural_mistake_azerty,args_natural_mistakes)
  }
  mapable_fun<- function(word){
    word_made_char <-callable_word_to_char()
    word_neighbor_mistake <-map_chr(word_made_char$chars, ~callable_fun(.x))
    word_neighbor_mistake<-paste(word_neighbor_mistake, collapse = "")
    return(word_neighbor_mistake)
  }
  if(!isTRUE(return_fun)){
    word_neighbor_mistake<- mapable_fun(word)
    return(word_neighbor_mistake)
  } else {
  return(mapable_fun)}
}

# natural_mistakes(word="hello",char_modification_prob = c(0.9,0.1), max_neighbor_size=2,sample_neighbor_prob = c(0.95,0.05), fat_finger=0.7,return_fun = TRUE)
