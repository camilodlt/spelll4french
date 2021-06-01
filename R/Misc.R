# Expanding List class ------
#
#Function to create a list optimized for memory.
#The list doubles in size as the items are added.
#

expandingList <- function(capacity = 10) {
  buffer <- vector('list', capacity)
  length <- 0
  methods <- list()

  # METHODS ---

  # Double size if no more slots
  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    capacity <<- capacity * 2
  }

  # append an item
  methods$add <- function(val) {
    if(length == capacity) {
      methods$double.size()
    }
    # if list is appended
    if(is_list(val) & !is_empty(val)){
      for(i in 1:length(val)){
        methods$add_named(val[[i]],names(val[i]))
      } # append one item at a time
    } else { # if val is not a list
      length <<- length + 1
      buffer[[length]] <<- val
    }
  }

  # append a named vector
  methods$add_named <- function(val, name) {
    if(length == capacity) {
      methods$double.size()
    }
    length <<- length + 1
    buffer[[length]] <<- val
    names(buffer)[length]<<- name
  }

  # reduce list keeping n items per sublist
  methods$reduce <- function(n) {

    walk2(buffer,1:length(buffer),function(.x,y,size=n){
      if(length(.x)>size){
        buffer[[y]]<<-sample(buffer[[y]],size)
      }
    })
  }

  # rm duplicated items per sublist
  methods$rm_duplicated <- function() {
    buffer<<-buffer[!duplicated(buffer)]
    buffer<<-purrr::discard(buffer, purrr::is_null)
    l<- length(buffer)
    length <<- l
  }

  # sample a single item over ALL sublists
  # sample resets the list
  methods$sample1 <- function() {
  ret_val<-as.character(sample(unlist(buffer),1))
  buffer <<- vector('list', capacity)
  length <<- 0
  return(ret_val)
  }

  # transform to a list (keeping only used slots)
  methods$as.list <- function() {
    b <- buffer[0:length]
    return(b)
  }
  methods
}
