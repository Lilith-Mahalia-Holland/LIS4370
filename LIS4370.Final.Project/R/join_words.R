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
    # This is rather complicated but I'm taking advantage of the fact that if
    # a value is not in the hashed_dictionary it'll throw an error. If I catch
    # that error I can set a flag to determine if the word is or isn't in the
    # dictionary and how to process it.
    flag <- tryCatch(values(hashed_dictionary["the"]) > 0, error=function(e){flag<-FALSE})
    if(flag){
      string <- append(string, key)
    }
    else{
      # If the word is not in the dictionary process it through the viterbi
      # algorithm and provide the new words.
      viterbi_out <- viterbi_segment(key, dictionary_hash, total, max_word_length)
      viterbi_words <- viterbi_out[[1]]
      viterbi_prob <- viterbi_out[[2]]
      # if the probability of the word being correct is not 0 then add to output
      if(viterbi_prob != 0){
        for(value in viterbi_words){
          string <- append(string, value)
        }
      }
    }
  }
  return(paste(string, collapse = " "))
}
