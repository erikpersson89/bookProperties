#' @title         suggestEmotionsBasedOnGenre
#' @description   A function that suggests the emotions of a text based on book genre
#' @import        MASS
#' @param         genre Book genre
#' @param         path_emotions_genre_prop Path to folder where emotion probabilities are stored
#' @param         language Book language
#' @return        Returns the predicted feelings of the text
#' @export

suggestFeelingsBasedOnGenre <- function(genre, path_emotions_genre_prop, language) {
  
  # Predict feeling based on genre (in case there are no) -------------------
  path = paste(path_emotions_genre_prop, "/feel_prob_norm_", language, ".csv", sep = "")
  feel_prob_norm = read.csv(file = path, encoding = "UTF-8") 
  feelings = as.character(feel_prob_norm$X)
  feel_prob_norm = feel_prob_norm[,-1]
  names(feel_prob_norm) = c("Barn", "Biografier", "Deckare", "Erotiskt", "Fantasy & SciFi", "Harlequin", "Klassiker", "Noveller",
                            "Romaner", "Spänningsromaner", "Tonår & Nästan vuxen")
  rownames(feel_prob_norm) = feelings
  genres = names(feel_prob_norm)
  prob_k = as.data.frame(feel_prob_norm[,names(feel_prob_norm) == genre])
  if (ncol(prob_k) > 0) {
    names(prob_k) = "Probability" 
    feel_prob_k = cbind(feelings, prob_k)
    feel_prob_k = feel_prob_k[order(feel_prob_k$Probability, decreasing = TRUE),]
    names(feel_prob_k)[1] = "Emotion"
  } else {
    feel_prob_k = NA
  }
  
  return(feel_prob_k)
  
}