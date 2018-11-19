#' @title         predictTargetAge
#' @description   A function that predicts the target age of a book
#' @import        MASS
#' @param         genre Book genre
#' @param         path_models Path to folder where glm models are stored
#' @param         path_genre_prop Path to folder where genre probabilities are stored
#' @param         readability Vector with: N_words, N_unique_words, N_long_words, N_sentences, LIX
#' @param         language Book language 
#' @return        Returns a predicted target age
#' @export

predictTargetAge <- function(genre, path_models, path_genre_prop, readability, language) {
  
  # Get names from target_age_genre_prob instead
  target_groups = c("0 to 3 years", "3 to 6 years", "6 to 9 years", "9 to 12 years", 
                    "Youth", "Young adult", "Adult")
  
# Get distribution of probabilities ---------------------------------------
  path_prob = paste(path_genre_prop, "/target_age_genre_prob_", language, ".csv", sep = "")
  prob_genre_norm = read.csv(file = path_prob, encoding = "UTF-8") 
  genres = as.character(prob_genre_norm$X)
  prob_genre_norm = prob_genre_norm[,2:8]
  names(prob_genre_norm) = target_groups
  rownames(prob_genre_norm) = genres
  
# Get models --------------------------------------------------------------
  path_model_child = paste(path_models, "/target_age_model_child_", language, ".rds", sep = "")  
  path_model_class = paste(path_models, "/target_age_model_classics_", language, ".rds", sep = "") 
  path_model_fan = paste(path_models, "/target_age_model_fantasy_", language, ".rds", sep = "") 
  path_model_teen = paste(path_models, "/target_age_model_teen_", language, ".rds", sep = "") 
  path_model_all = paste(path_models, "/target_age_model_all_", language, ".rds", sep = "") 
  model_child = readRDS(file = path_model_child, refhook = NULL)
  model_class = readRDS(file = path_model_class, refhook = NULL)
  model_fan = readRDS(file = path_model_fan, refhook = NULL)
  model_teen = readRDS(file = path_model_teen, refhook = NULL)
  model_all = readRDS(file = path_model_all, refhook = NULL)
  
  # Define matrix where readability and predictions are stored --------------
  targets = readability
  
  # Predict target age groups  ----------------------------------------------
  predictions = matrix(nrow = 1, ncol = 7)
  predicted_class = c()
  certainty = c()
  if (genre == "Barn") {
    predictions = as.numeric(predict(model_child, targets, type = "p")) * as.numeric(prob_genre_norm[genre == rownames(prob_genre_norm),])
    predictions = predictions/sum(predictions)
    predicted_class = target_groups[which(predictions == max(predictions))]
    certainty = max(predictions)
  } else if (genre == "Fantasy & SciFi") {
    predictions = as.numeric(predict(model_fan, targets, type = "p")) * as.numeric(prob_genre_norm[genre == rownames(prob_genre_norm),])
    predictions = predictions/sum(predictions)
    predicted_class = target_groups[which(predictions == max(predictions))]
    certainty = max(predictions)
  } else if (genre == "Klassiker") {
    predictions = as.numeric(predict(model_class, targets, type = "p")) * as.numeric(prob_genre_norm[genre == rownames(prob_genre_norm),])
    predictions = predictions/ sum(predictions)
    predicted_class = target_groups[which(predictions == max(predictions))]
    certainty = max(predictions)
  } else if (genre == "Tonår & Nästan vuxen") {
    predictions = as.numeric(predict(model_teen, targets, type = "p")) * as.numeric(prob_genre_norm[genre == rownames(prob_genre_norm),])
    predictions = predictions/sum(predictions)
    predicted_class = target_groups[which(predictions == max(predictions))]
    certainty = max(predictions)
  } else if (sum(genre == rownames(prob_genre_norm)) > 0 && genre != "Barn" && genre != "Fantasy & SciFi" &&
             genre != "Klassiker" && genre != "Tonår & Nästan vuxen") {
    predictions = as.numeric(prob_genre_norm[genre == rownames(prob_genre_norm),])
    predictions = predictions/sum(predictions)
    predicted_class = target_groups[which(predictions == max(predictions))]
    certainty = max(predictions)
  }
  targets$tag_ai = predicted_class
  targets$certainty = certainty

  return(targets)
  
}