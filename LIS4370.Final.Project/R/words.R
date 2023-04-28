#' @title Word split
#' @description Separates a sentence into individual words removing numbers.
#' @param text Any sentence/paragraph/paper or series of words separated by a space or punctuation.
#'
#' @return Returns the separated text in the form of individual words.
#' @export

words <- function(text){
  # Removes numbers from text and converts it to lowercase
  text <- gsub("[0-9]", "", text)
  text <- tolower(text)
  # Separates each word into it's own element
  return(regmatches(text, gregexpr("(*UCP)[\\w']+", text, perl=TRUE)))
}
