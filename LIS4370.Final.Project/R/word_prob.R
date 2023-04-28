#' @title Word probability
#' @description Checks the probability of a word showing up based on a trained dictionary.
#' @param word Any given single word.
#' @param dictionary_hash Hashed dictionary obtained through the train_viterbi() function.
#' @param total Total sum of dictionary values.
#'
#' @return Returns simple probability of word showing up in trained dictionary.
#' @export

word_prob <- function(word, dictionary_hash, total){
  return(values(dictionary_hash[word])/total)
}
