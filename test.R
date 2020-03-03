# construct test dataframe ####

vec_x1 <- sample(seq(100, 1000, 100), size = 1000, replace = TRUE)
vec_x2 <- sample(seq(0, 1.3, by = 0.1), size = 1000, replace = TRUE)
vec_x3 <- sample(c('red', 'blue'), 1000, replace = TRUE)
vec_x4 <- sample(c(5.546, 5.5, by = 0.01), 1000, replace = TRUE)
vec_y1 <- sample(100:15000, 1000)
vec_y2 <- sample(c(seq(0.2:0.9, by = 0.01),
                   seq(1:60)), 1000, replace = TRUE)
df_test <- data.frame(x1 = vec_x1,
                      x2 = vec_x2,
                      x3 = vec_x3,
                      x4 = vec_x4,
                      xmeasurement6 = vec_y1,
                      measurement_A = vec_y2)
library(tidyverse)
write_csv(df_test, "testData.csv")


testFunction <- function(df, KEYWORD) {
  vec_y <- df %>%
    select(contains(KEYWORD)) %>% 
    rowMeans()
  
  df_factors <- df %>% 
    select(-contains(KEYWORD)) %>% 
    select_if(negate(is.numeric))
  
  df_forModel <- df_factors %>% 
    mutate(y = vec_y)
  
  paste("y ~", paste(names(df_factors), collapse = " + "))
}

testFunction(df_test, "measurement")

library(carData)
data("Blackmore")
