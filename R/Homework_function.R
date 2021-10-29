
#' @title Convert Fahrenheit to Celsius
#' @description This function will convert a temperature from Fahrenheit to Celsius
#' @param temp_F a temperature in Fahrenheit
#' @return temperature in Celsius
fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

#' @title Convert Celsius to Kelvin
#' @description This function will convert a temperature from Celsius to Kelvin
#' @param temp_C a temperature in Celsius
#' @return temperature in Kelvin
celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}

