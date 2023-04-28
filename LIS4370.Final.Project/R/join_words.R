#' @title Probability of word
#' @description Checks how common the word is in the dictionary provided.
#' @param word Any given single fussed/misspelt word.
#' @param dictionary_hash Hashed dictionary obtained through the train_viterbi() function.
#' @param total Total sum of dictionary values.
#' @param max_word_length Longest word in the dictionary.
#'
#' @return Returns the corrected word/words from the provided word.
#' @export

join_words <- function(word, dictionary_hash, total, max_word_length){
  string <- ""
  for(key in words(word)){
    flag <- tryCatch(values(hashed_dictionary["the"]) > 0, error=function(e){flag<-FALSE})
    if(flag){
      string <- append(string, key)
    }
    else{
      viterbi_out <- viterbi_segment(key, dictionary_hash, total, max_word_length)
      viterbi_words <- viterbi_out[[1]]
      viterbi_prob <- viterbi_out[[2]]
      if(viterbi_prob != 0){
        for(value in viterbi_words){
          string <- append(string, value)
        }
      }
    }
  }
  return(paste(string, collapse = " "))
}
