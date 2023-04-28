#' @import hash
#' @import stats
#' @title Train viterbi dictionary
#' @description Builds and trains the hashed dictionary for use with the viterbi algorithm.
#' @param text Any sentence/paragraph/paper or series of words separated by a space or punctuation.
#' @param dictionary Any char data structure that has been processed by dictionary()
#' @param hashed_dictionary Hashed dictionary obtained through the train_viterbi() function.
#'
#' @return Hashed_dictionary, total, and max_word_length as a list
#' @export

train_viterbi <- function(text, dictionary, hashed_dictionary=NULL){
  # Check if a hashed_dictionary is provided, if not create an empty dictionary
  if(is.null(hashed_dictionary)){
    hashed_dictionary <- hash(keys=dictionary, values=0)
  }
  text <- words(text)
  temp_df <- data.frame("words" = unlist(text),
                        "sum" = 1)
  # Split words, sum, and create a dataframe from them
  temp_df <- aggregate(sum ~ words, temp_df, sum)
  # Covert dataframe to hashmap
  temp_hash <- hash(keys=temp_df$words, values=temp_df$sum)

  # If key does not show up in the hashed_dictionary toss it
  for(i in keys(temp_hash)){
    if(!is.null(hashed_dictionary[[i]])){
      hashed_dictionary[[i]] <- temp_hash[[i]] + hashed_dictionary[[i]]
    }
  }
  # Sum all of the values over 0
  total <- sum(values(hashed_dictionary) > 0)
  # Find the longest word in the dictionary
  max_word_length <- max(nchar(keys(hashed_dictionary)))
  return(list(hashed_dictionary, total, max_word_length))
}
