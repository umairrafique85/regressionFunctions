# TASK2 ####
# function: RegrMean(df, KEYWORD, regressionOrder = 1, twowayInteractions = FALSE)
# outputs:
# dfRegrMean: Term, Coef, SEcoef, tvalue, pvalue, fvalue (this is a mistake in the question), VIF
# dfRegrResiduals: df + Residuals, StdResiduals
# Rsq
# Rsqadj
# MSE
# SSE
# flag
# errorMessage

RegrMean <- function(df, KEYWORD, regressionOrder = 1, towayInteractions = FALSE) {
  require(tidyverse)
  vec_y <- df %>%
    select(contains(KEYWORD)) %>% 
    rowMeans()
  
  df_factors <- df %>% 
    select(-contains(KEYWORD))
  
  df_factors %>% 
    names() -> vec_allFactors
  
  df_factors %>% 
    select_if(is.numeric) %>% 
    names() -> vec_numerics
  
  df_factors %>% 
    select_if(negate(is.numeric)) %>% 
    names() -> vec_categoricals
  
  if (regressionOrder == 2) {
    if (towayInteractions) {
      if (length(vec_categoricals) > 1) {
        quadSwitch <- TRUE
        interaction <- TRUE
        str_formula <- paste("y ~ ",
                             paste("poly(", paste(vec_numerics, collapse = ", 2) + poly("), ", 2)"),
                             " + ",
                             paste("(", 
                                   paste(vec_categoricals, collapse = " + "),
                                   ")^2", sep = ""),
                             sep = " ")
      } else {
        quadSwitch <- TRUE
        interaction <- FALSE
        str_formula <- paste("y ~ ",
                             paste("poly(", paste(vec_numerics, collapse = ", 2) + poly("), ", 2)"),
                             " + ",
                             vec_categoricals,
                             sep = " ")
      }
    } else {
      quadSwitch <- TRUE
      interaction <- FALSE
      str_formula <- paste("y ~ ", 
                           paste("poly(", 
                                 paste(vec_numerics, collapse = ", 2) + poly("), 
                                 ", 2)"),
                           " + ", paste(vec_categoricals, collapse = " + "))
    } 
  } else if (towayInteractions & regressionOrder == 1) {
    if (length(vec_categoricals) > 1) {
      quadSwitch <- FALSE
      interaction <- TRUE
      str_formula <- paste("y ~ ",
                           paste(vec_numerics, collapse = " + "),
                           " + ",
                           paste("(", 
                                 paste(vec_categoricals, collapse = " + "),
                                 ")^2", sep = ""))
    } else {
      quadSwitch <- FALSE
      interaction <- FALSE
      str_formula <- paste("y ~ ",
                           paste(vec_allFactors, collapse = " + ")
                           )
    }
  } else {
    quadSwitch <- FALSE
    interaction <- FALSE
    str_formula <- paste("y ~ ", 
                         paste(vec_allFactors, collapse = " + ")
                         )
  }
  
  df_forModel <- df_factors %>% 
    mutate(y = vec_y)
  
  tryCatch(
    {
      model <- lm(as.formula(str_formula),
                  data = df_forModel)
      require(car)
      regTerms <- rownames(summary(model)$coefficients)
      if (!quadSwitch & !interaction) {
        vec_vifs <- vif(model)[,1]
        vec_vifsFixed <- rep(NA, length(regTerms))
        for (j in seq_along(vec_vifs)) {
          vec_vifsFixed[grep(names(vec_vifs)[j], regTerms)] <- vec_vifs[j]}
      }
      browser()
      regTerms <- gsub("[:]", "*", regTerms)
      regTerms <- gsub(", 2)1", "", gsub(", 2)2", "^2", 
                                         gsub("^poly\\(", "", regTerms), 
                                         fixed = TRUE), fixed = TRUE)
      
      dfRegrMean <- data.frame("Term" = regTerms,
                               "Coef" = summary(model)$coefficients[,1],
                               "SEcoef" = summary(model)$coefficients[,2],
                               "tvalue" = summary(model)$coefficients[,3],
                               "pvalue" = summary(model)$coefficients[,4],
                               "vif" = vec_vifsFixed)
      rownames(dfRegrMean) <- NULL
      vec_residuals <- model$residuals
      vec_stdResiduals <- model$residuals / sd(model$residuals)
      dfRegrResiduals <- df %>% 
        mutate(Residuals = vec_residuals,
               StdResiduals = vec_stdResiduals)
      lst_return <- list(dfRegrMean = dfRegrMean,
                         dfRegrResiduals = dfRegrResiduals,
                         Rsq = summary(model)$r.squared,
                         Rsqadj = summary(model)$adj.r.squared,
                         MSE = mean(vec_residuals^2),
                         SSE = sum(vec_residuals^2),
                         flag = FALSE)
    },
    error = function(errorMessage) {
      lst_return <- list(
        flag = TRUE,
        errorMessage = errorMessage
      )
    }
  )
  
  return(lst_return)
}

# test
results <- RegrMean(df_test, "measurement")
View(results$dfRegrMean)
