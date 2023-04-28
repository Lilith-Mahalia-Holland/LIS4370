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

  # March over each character in the provided text/word
  for(i in 1:(nchar(text)+2)){
    prob_k <- NULL
    k <- NULL
    # expand the word search starting with the first character, the first two, etc...
    for(j in max(0, i-max_word_length):i+1){
      # probability of a word is just the last probability * the new probability
      prob_k[j] <- probs[j] * word_prob(substr(text, j, i), dictionary_hash, total)
      k[j] <- j
    }
    probs <- append(probs, max(prob_k))
    last <- append(last, max(k))
  }
  words <- NULL
  while (0 < i){
    # append all of the potential words to the words vector
    words <- append(words, text[last[i]:i])
    i <- last[i]
  }
  # reverse words
  words <- rev(words)
  # take the last probability
  return(list(words, probs[length(probs)]))
}
