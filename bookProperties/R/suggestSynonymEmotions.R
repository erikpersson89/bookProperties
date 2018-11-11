#' @title         suggestSynonymEmotions
#' @description   A function that suggests synonym emotions of an emotion word 
#' @import        MASS
#' @param         emotion_word Emotion word
#' @param         path_emotion_synonyms Path to folder where emotion synonyms are stored
#' @param         language Book language 
#' @return        Returns synonym emotions
#' @export

suggestSynonymEmotions <- function(emotion_word, path_emotion_synonyms, language) {
  
  path = paste(path_sentiment_lexicon, "/emotion_words_", language, ".xlsx", sep = "")
  emotion_synonym_list = readxl::read_xlsx(path = path, sheet = 2)
  
  emotion_synonyms = emotion_synonym_list$Tag[tolower(emotion_synonym_list$`Alternative tag`) == emotion_word]
  emotion_synonyms = emotion_synonyms[!is.na(emotion_synonyms)]
  
  return(emotion_synonyms)
  
}


