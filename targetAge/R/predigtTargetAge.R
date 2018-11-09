#' @title         predictTargetAge
#' @description   A function that predicts target age of a text
#' @import        MASS
#' @param         book Stagger tagged book
#' @param         genre Book genre
#' @param         path_models Path to folder where glm models are stored
#' @param         path_genre_prop Path to folder where genre probabilities are stored
#' @return        Returns a predicted target age
#' @export

predictTargetAge <- function(book, genre, path_models, path_genre_prop) {
  
  target_groups = c("0 till 3 år", "3 till 6 år", "6 till 9 år", "9 till 12 år", 
                    "Ungdom", "Nästan vuxen", "Vuxen")
  
# Get distribution of probabilities ---------------------------------------
  path_prob = paste(path_genre_prop, "/prob_genre_norm.csv", sep = "")
  prob_genre_norm = read.csv(file = path_prob, encoding = "UTF-8") 
  genres = as.character(prob_genre_norm$X)
  prob_genre_norm = prob_genre_norm[,2:8]
  names(prob_genre_norm) = target_groups
  rownames(prob_genre_norm) = genres
  
# Get models --------------------------------------------------------------
  path_model_child = paste(path_models, "/model_child.rds", sep = "")  
  path_model_class = paste(path_models, "/model_class.rds", sep = "") 
  path_model_fan = paste(path_models, "/model_fan.rds", sep = "") 
  path_model_teen = paste(path_models, "/model_teen.rds", sep = "") 
  path_model_all = paste(path_models, "/model_all.rds", sep = "") 
  model_child = readRDS(file = path_model_child, refhook = NULL)
  model_class = readRDS(file = path_model_class, refhook = NULL)
  model_fan = readRDS(file = path_model_fan, refhook = NULL)
  model_teen = readRDS(file = path_model_teen, refhook = NULL)
  model_all = readRDS(file = path_model_all, refhook = NULL)
  
# Calculate LIX score  -----------------------------------------------------
  N_books = length(books)
  targets = as.data.frame(matrix(nrow = 1, ncol = 5))
  targets[,] = 0
  names(targets) = c("N_words", "N_unique_words", "N_long_words", "N_sentences", "LIX")
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
  targets$ISBN[k] = isbn
  targets$N_words[k] = O
  targets$N_unique_words[k] = length(unique(words))
  targets$N_long_words[k] = L
  targets$N_sentences[k] = M
  targets$LIX[k] = LIX

  
  # # Predict target age groups  ----------------------------------------------
  # predictions = matrix(nrow = N_books, ncol = 7)
  # predicted_class = c()
  # certainty = c()
  # e = 0
  # for (k in 1:N_books) {
  #   isbn = targets$ISBN[k]
  #   genre = book_meta_data$GENRE[book_meta_data$ISBN == isbn]  
  #   if (genre == "Barn") {
  #     predictions[k,] = as.numeric(predict(model_child, targets[k,], type = "p")) * as.numeric(prob_genre_norm[genre == rownames(prob_genre_norm),])
  #     predictions[k,] = predictions[k,] / sum(predictions[k,])
  #     predicted_class[k] = target_groups[which(predictions[k,] == max(predictions[k,]))] 
  #     certainty[k] = max(predictions[k,])
  #   } else if (genre == "Fantasy & SciFi") {
  #     predictions[k,] = as.numeric(predict(model_fan, targets[k,], type = "p")) * as.numeric(prob_genre_norm[genre == rownames(prob_genre_norm),])
  #     predictions[k,] = predictions[k,] / sum(predictions[k,])
  #     predicted_class[k] = target_groups[which(predictions[k,] == max(predictions[k,]))] 
  #     certainty[k] = max(predictions[k,])
  #   } else if (genre == "Klassiker") {
  #     predictions[k,] = as.numeric(predict(model_class, targets[k,], type = "p")) * as.numeric(prob_genre_norm[genre == rownames(prob_genre_norm),])
  #     predictions[k,] = predictions[k,] / sum(predictions[k,])
  #     predicted_class[k] = target_groups[which(predictions[k,] == max(predictions[k,]))] 
  #     certainty[k] = max(predictions[k,])
  #   } else if (genre == "Tonår & Nästan vuxen") {
  #     predictions[k,] = as.numeric(predict(model_teen, targets[k,], type = "p")) * as.numeric(prob_genre_norm[genre == rownames(prob_genre_norm),])
  #     predictions[k,] = predictions[k,] / sum(predictions[k,])
  #     predicted_class[k] = target_groups[which(predictions[k,] == max(predictions[k,]))] 
  #     certainty[k] = max(predictions[k,])
  #   } else if (sum(genre == rownames(prob_genre_norm)) > 0 && genre != "Barn" && genre != "Fantasy & SciFi" && 
  #              genre != "Klassiker" && genre != "Tonår & Nästan vuxen") {
  #     predictions[k,] = as.numeric(prob_genre_norm[genre == rownames(prob_genre_norm),])
  #     predictions[k,] = predictions[k,] / sum(predictions[k,])
  #     predicted_class[k] = target_groups[which(predictions[k,] == max(predictions[k,]))] 
  #     certainty[k] = max(predictions[k,])
  #   }
  #   targets$tag_ai[k] = predicted_class[k]
  #   targets$certainty[k] = certainty[k]  
  # }
  
  return(targets)
  
}