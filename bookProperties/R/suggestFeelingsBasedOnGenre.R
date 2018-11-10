#' @title         suggestFeelingsBasedOnGenre
#' @description   A function that suggests the feelings of a text based on book genre
#' @import        MASS
#' @param         genre Book genre
#' @param         language Book language 
#' @return        Returns the predicted feelings of the text
#' @export

suggestFeelingsBasedOnGenre <- function(genre, langauge) {
  
  # Predict feeling based on genre (in case there are no) -------------------
  feel_prob_norm = read.csv(file = "/srv/rstudio/Book properties/Data/Feelings/feel_prob_norm.csv", encoding = "UTF-8") 
  feelings = as.character(feel_prob_norm$X)
  feel_prob_norm = feel_prob_norm[,-1]
  names(feel_prob_norm) = c("Barn", "Biografier", "Deckare", "Erotiskt", "Fantasy & SciFi", "Harlequin", "Klassiker", "Noveller",
                            "Romaner", "Spänningsromaner", "Tonår & Nästan vuxen")
  rownames(feel_prob_norm) = feelings
  genres = names(feel_prob_norm)
  
  book_list_genre_prob = c()
  for (k in 1:N_books) {
    isbn = substr(x = book_list[k], start = 1, stop = 13)
    genre = book_meta_data$GENRE[book_meta_data$EBOOK_ISBN == isbn]
    prob_k = as.data.frame(feel_prob_norm[,names(feel_prob_norm) == genre])
    if (ncol(prob_k) > 0) {
      names(prob_k) = "Probability" 
      feel_prob_k = cbind(feelings, prob_k)
      feel_prob_k = feel_prob_k[order(feel_prob_k$Probability, decreasing = TRUE),]
      feel_prob_k = feel_prob_k[1:20,]
      book_list_genre_prob_k = c()
      for (i in 1:nrow(feel_prob_k)) {
        book_list_genre_prob_k = rbind(book_list_genre_prob_k, cbind(book_meta_data[book_meta_data$EBOOK_ISBN == isbn,], feel_prob_k[i,]))
      }
    } else {
      book_list_genre_prob_k = cbind(book_meta_data[book_meta_data$EBOOK_ISBN == isbn,], NA, NA)
      names(book_list_genre_prob_k)[(ncol(book_list_genre_prob_k)-1):ncol(book_list_genre_prob_k)] = c("feelings", "Probability")
    }
    book_list_genre_prob = rbind(book_list_genre_prob, book_list_genre_prob_k)
  }
  
}

