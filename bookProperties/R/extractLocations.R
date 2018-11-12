#' @title         extractLocations
#' @description   A function that extract locations and corresponding countries/continents from a book 
#' @import        MASS
#' @param         book Stagger tagged book
#' @param         path_geo Path to folder where geographical data is stored
#' @param         language Book language 
#' @return        Returns a a vector with locations and corresponding countries/continents as well as their frequencies
#' @export

extractLocations <- function(book, path_geo, language) {
  
  # Import geographical data ---------------------------------------------------------
  path = paste(path_geo, "/geo_", language, ".csv", sep = "")
  geo = read.csv(file = path, encoding = "UTF-8")
  geo$City = as.character(geo$City)
  geo$Country_code = as.character(geo$Country_code)
  geo$Country = as.character(geo$Country)
  geo$Continent = as.character(geo$Continent)
  
  # Extract locations -------------------------------------------------------
  places_mat = c()
  places = extract_entity(object = book, type = "place")
  places = sort(table(as.factor(places)), decreasing = TRUE)
  places = places[places > 1] 
  if (length(places) > 0) {
    for (i in 1:length(places)) {
      place = names(places[i])
      frequency = as.numeric(places[i]) 
      city_index = which(geo$City == place)
      country_index = which(geo$Country == place)[1]
      continent_index = which(geo$Continent == place)[1]
      if (length(city_index) > 0) {
        city = place
        country = geo$Country[city_index]
        continent = geo$Continent[city_index]
      } else if (length(country_index) > 0) {
        city = NA
        country = geo$Country[country_index]
        continent = geo$Continent[country_index]
      } else if (length(continent_index) > 0) {
        city = NA
        country = NA
        continent = geo$Continent[continent_index]
      } else {
        city = NA
        country = NA
        continent = NA
      }
      places_mat = rbind(places_mat, cbind(place, frequency, city, country, continent))
    }
  } else {
    places_mat = cbind(NA, NA, NA, NA, NA)
  }
  places_mat = as.data.frame(places_mat)
  names(places_mat) = c("LOCATION", "FREQUENCY", "CITY", "COUNTRY", "CONTINENT")
  
  return(places_mat)
    
}