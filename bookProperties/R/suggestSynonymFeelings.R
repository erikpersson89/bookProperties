# Combine reviews with scores ---------------------------------------------
reviews_scores = cbind(reviews, all_feeling_scores)
names(reviews_scores)[ncol(reviews_scores)] = "SCORE"

# Use alternative feeling tags to add real feeling tags -------------------
book_list_tags = c()
for (k in 1:nrow(book_list_predicted_tags)) {
  feeling_alt = as.character(book_list_predicted_tags$FEELING[k])
  feelings_true = emotion_tags$Tag[which(tolower(emotion_tags$`Alternative tag`) == feeling_alt)]
  feelings_true = feelings_true[!is.na(feelings_true)]
  book_list_tags_k = c()
  if (length(feelings_true) > 0) {
    for (i in 1:length(feelings_true)) {
      FEELING_TRUE = feelings_true[i]
      book_list_tags_k = rbind(book_list_tags_k, cbind(book_list_predicted_tags[k,], FEELING_TRUE))
    }
  } else {
    book_list_tags_k = cbind(book_list_predicted_tags[k,], NA)
  }
  names(book_list_tags_k)[ncol(book_list_tags_k)] = "FEELING_CONTENT"
  book_list_tags = rbind(book_list_tags, book_list_tags_k)
}

# Change format of reviews (due to some stupid problems with the CSV format) ------------------------------------------------
reviews_scores$REVIEW = gsub(pattern = '\r', replacement = " ", x = reviews_scores$REVIEW)
reviews_scores$REVIEW = gsub(pattern = '\n', replacement = " ", x = reviews_scores$REVIEW)

