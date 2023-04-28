#' @title Dictionary initialization
#' @description Prepares a dictionary for use with the viterbi algorithm.
#' @param dictionary Any char data structure that can be run through sapply().
#' @param remove_singles Bool flag to remove single letter words from dictionary.
#'
#' @return Processed dictionary.
#' @export

populate_dictionary <- function(dictionary, remove_singles=FALSE){
  dictionary <- sapply(dictionary, tolower)
  if(remove_singles){
    dictionary <- dictionary[nchar(dictionary) > 0]
  }
  return(dictionary)
}
