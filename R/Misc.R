
expandingList <- function(capacity = 10) {
  buffer <- vector('list', capacity)
  length <- 0

  methods <- list()

  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    capacity <<- capacity * 2
  }

  methods$add <- function(val) {
    if(length == capacity) {
      methods$double.size()
    }

    if(is_list(val)& !is_empty(val)){
      for(i in 1:length(val)){
        methods$add_named(val[[i]],names(val[i]))
      }
    } else {
      length <<- length + 1
      buffer[[length]] <<- val
    }

  }

  methods$add_named <- function(val, name) {
    if(length == capacity) {
      methods$double.size()
    }
    length <<- length + 1
    buffer[[length]] <<- val
    names(buffer)[length]<<- name
  }

  methods$reduce <- function(n) {

    walk2(buffer,1:length(buffer),function(.x,y,size=n){
      if(length(.x)>size){
        buffer[[y]]<<-sample(buffer[[y]],size)
      }
    })
  }

  methods$as.list <- function() {
    b <- buffer[0:length]
    return(b)
  }
  methods$rm_duplicated <- function() {
    #buffer<<- purrr::flatten(buffer)
    buffer<<-buffer[!duplicated(buffer)]
    buffer<<-purrr::discard(buffer, purrr::is_null)
    l<- length(buffer)
    length <<- l
  }

  methods$sample1 <- function() {
  ret_val<-as.character(sample(unlist(buffer),1))
  buffer <<- vector('list', capacity)
  length <<- 0
  return(ret_val)
  }

  methods
}
