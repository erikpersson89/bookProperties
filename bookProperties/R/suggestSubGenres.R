#' @title         suggestSubGenres
#' @description   A function that suggests sub genres based on book genre
#' @import        MASS
#' @param         genre Book genre
#' @param         path_sub_genre_prop Path to folder where sub genre probabilities are stored
#' @param         language Book language
#' @return        Returns suggested sub genres
#' @export

suggestSubGenres <- function(genre, path_sub_genre_prob, language) {
  
  # Get sub genres ----------------------------------------------------------
  path = paste(path_sub_genre_prob, "/sub_genre_prob_", language, ".csv", sep = "")
  prob_norm = read.csv(file = path, encoding = "UTF-8")
  sub_genres = as.character(prob_norm$X)
  prob_norm = prob_norm[,-1]
  names(prob_norm) = c("Barn", "Biografier", "Deckare", "Erotiskt", "Fakta", "Fantasy & SciFi", "Harlequin", "Klassiker", "Noveller", "Romaner",
                       "Spänningsromaner", "Tonår & Nästan vuxen")
  genres = names(prob_norm)
  rownames(prob_norm) = sub_genres
  
  
  # Suggest sub genres based on genre ---------------------------------------
  ind = which(names(prob_norm) == genre)
  sg_k = as.data.frame(cbind(rownames(prob_norm), prob_norm[,ind]))
  sg_k = sg_k[order(sg_k$V2, decreasing = TRUE),]
  names(sg_k) = c("Sub_genre", "Probability")
  
  return(sg_k)
  
}