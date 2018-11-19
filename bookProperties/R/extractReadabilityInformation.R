#' @title         extractReadabilityInformation
#' @description   A function that extracts readability information from a book
#' @import        MASS
#' @param         book Stagger tagged book
#' @param         path_models Path to folder where glm models are stored
#' @param         path_genre_prop Path to folder where genre probabilities are stored
#' @param         language Book language 
#' @return        Returns a vector with readability information, such as number of words and LIX
#' @export

extractReadabilityInformation <- function(book, path_models, path_genre_prop, language) {
  
  # Calculate LIX score  -----------------------------------------------------
  readability = as.data.frame(matrix(nrow = 1, ncol = 5))
  readability[,] = 0
  names(readability) = c("N_words", "N_unique_words", "N_long_words", "N_sentences", "LIX")
  M = length(unique(book$Sentence.ID)) # Number of sentences
  words = book$Form
  words = words[words != ","]
  words = words[words != "."]
  words = words[words != "-"]
  words = words[words != ":"]
  words = words[words != ";"]
  words = words[words != "!"]
  words = words[words != "?"]
  O = length(words) #Number of words
  L = sum(nchar(words) > 6)
  LIX = O/M + (L*100/O)
  readability$N_words = O
  readability$N_unique_words = length(unique(words))
  readability$N_long_words = L
  readability$N_sentences = M
  readability$LIX = LIX
  
  return(readability)
  
}