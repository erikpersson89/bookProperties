#' @title         extractFeelings
#' @description   A function that predicts the feelings of a text
#' @import        MASS
#' @param         text Character vector with text
#' @param         path_sentiment_lexicon Path to folder where sentiment lexicon is stored
#' @param         language Book language 
#' @return        Returns the predicted feelings of the text
#' @export

extractFeelings <- function(text, path_sentiment_lexicon, language) {
  
  # Import emotion words ----------------------------------------------------
  path = paste(path_sentiment_lexicon, "/emotion_words_", language, ".xlsx", sep = "")
  emotion_words = readxl::read_xlsx(path = path, sheet = 1)
  emotion_tags = readxl::read_xlsx(path = path, sheet = 2)
  
  # Get list with alternative feeling tags ----------------------------------
  tags_alt = tolower(sort(unique(emotion_tags$`Alternative tag`[!is.na(emotion_tags$`Alternative tag`)])))
  
  # Loop through books, extract reviews and predict feelings ----------------
  all_feeling_scores = c()
  book_list_predicted_tags = c()
  feelings = c()
  t_m = as.data.frame(matrix(nrow = length(tags_alt), ncol = 1))
  rownames(t_m) = tags_alt
  t_m[,] = 0
  words = tolower(unlist(strsplit(x = text, split = " ")))
  words = gsub("\\.", "", words)
  words = gsub("\\,", "", words)
  words = gsub("\\!", "", words)
  words = gsub("\\?", "", words)
  if (length(words) > 0 && !is.na(words)) {
    for (i in 1:length(words)) {
      word = words[i]
      feeling = tolower(emotion_words$Feeling)[tolower(emotion_words$Word) == word]
      if (length(feeling) > 0 && !is.na(feeling)) {
        feelings = c(feelings, feeling)
        t_m[which(rownames(t_m) == feeling),1] = t_m[which(rownames(t_m) == feeling),1] + 1
      }
    }
  }
  
  # Calculate score for review ----------------------------------------------
  feeling_scores = sort(table(feelings), decreasing = TRUE)
  feelings_r = rownames(t_m)[t_m[,1]!=0]
  scores_r = 0
  if (length(feelings_r) > 0) {
    for (f in 1:length(feelings_r)) {
      feeling_f = feelings_r[f]
      score_f = as.numeric(feeling_scores[names(feeling_scores) == feeling_f])
      scores_r = scores_r + score_f
    }
  } else {
    scores_r = 0
  }
  
  if (length(feeling_scores) > 0) {
    for (s in 1:length(feeling_scores)) {
      FEELING = names(feeling_scores)[s]
      SCORE = feeling_scores[s]
      if (!is.null(FEELING)) {
        FEELING = FEELING
        SCORE = as.numeric(SCORE)
      } 
    }
  } else {
    FEELING = NA
    SCORE = NA
  }
  
  return(FEELING, SCORE)
  
}