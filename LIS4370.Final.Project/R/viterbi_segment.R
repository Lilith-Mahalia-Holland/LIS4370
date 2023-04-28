#' @import stats
#' @title Viterbi segment algorithm
#' @description Runs the viterbi segment algorithm on any text. The algorithm will use the provided dictionary to split and correct fussed and misspelt words.
#' @param text Any given single fussed text.
#' @param dictionary_hash Hashed dictionary obtained through the train_viterbi() function.
#' @param total Total sum of dictionary values.
#' @param max_word_length Longest word in the dictionary.
#'
#' @return Returns he decomposed word and probability of correctness as a list..
#' @export

viterbi_segment <- function(text, dictionary_hash, total, max_word_length) {
  probs <- 1
  last <- 0

  # The index may be off and potentially could need to be 1:+2
  for(i in 1:(nchar(text)+2)){
    prob_k <- NULL
    k <- NULL
    for(j in max(0, i-max_word_length):i+1){
      prob_k[j] <- probs[j] * word_prob(substr(text, j, i), dictionary_hash, total)
      k[j] <- j
    }
    probs <- append(probs, max(prob_k))
    last <- append(last, max(k))
  }
  words <- NULL
  while (0 < i){
    words <- append(words, text[last[i]:i])
    i <- last[i]
  }
  words <- rev(words)
  # -1 in this case just removes the last one not selects it
  return(list(words, probs[length(probs)]))
}
