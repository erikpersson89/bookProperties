#' @title         extractCharacters
#' @description   A function that extracts characters from a text
#' @import        MASS
#' @param         book Stagger tagged book
#' @return        Returns extracted characters
#' @export

extractCharacters <- function(book) {
  
  characters = rStagger::extract_entity(object = book, type = "person")
  characters = sort(table(characters), decreasing = TRUE)
  l = length(characters)
  if (l > 29) {
    characters_df = as.data.frame(cbind(names(characters), as.numeric(characters))[1:30,])
  } else {
    characters_df = as.data.frame(cbind(names(characters), as.numeric(characters))[1:l,])
  }
  names(characters_df) = c("PERSON", "FREQUENCY")
  N = nrow(characters_df)
  
  return(characters_df)
  
}