library(palmerpenguins)
data(penguins)

fit <- lm(bill_length_mm ~ bill_depth_mm, data = penguins)
print.character <- function(x, ...){
  cat("you are here.\n")
}
rm(print.character)
