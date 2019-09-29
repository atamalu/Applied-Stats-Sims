lmer_effects <- function(model, formula = FALSE){
  model.icc <- icc(model); model.r2 <- r2(model) # effect sizes
  
  model.effects <- data.frame(
    ICC.adj = model.icc$ICC_adjusted,
    ICC.cond = model.icc$ICC_conditional,
    ICC.AminusC = model.icc$ICC_adjusted - model.icc$ICC_conditional,
    R2.marg = model.r2$R2_marginal,
    R2.cond = model.r2$R2_conditional,
    row.names = NULL
  )
  
  if(formula == TRUE){
    model.effects$Formula = deparse(model@call$formula, width.cutoff = 200L)
  }
  
  return(model.effects)
}

### mod1 = null; mod2 = alternative
comp_lmer_effects <- function(mod.null, mod.alt){
  
  Dataframe <- rbind(lmer_effects(mod.null), 
                     lmer_effects(mod.alt))
  
  Dataframe[3,] <- Dataframe[2,] - Dataframe[1,]
  
  rownames(Dataframe) <- c('Compact', 'Augmented', 'Difference')
  
  return(Dataframe)
  
}

comp_lmer_mods <- function(mod.null, mod.alt){
  
  ##### Model comparison ---------------
  terms1 <- attributes(terms(mod.null))$term.labels
  terms2 <- attributes(terms(mod.alt))$term.labels
  interest.var <- terms2[-which(terms2 %in% terms1)]
  
  mod.summ <- summary(mod.alt)
  var.summ <- mod.summ$coefficients[interest.var,]
  var.summ <- var.summ[c('Estimate', 'Std. Error', 'Pr(>|t|)')]
  var.summ <- as.data.frame(t(var.summ))

  ##### Effect size comparisons, CI's ---------------
  ef.dif <- comp_lmer_effects(mod.null, mod.alt)[3,] # isolates variance from lone parameter
  CI <- confint(mod.alt, method = 'Wald')[interest.var,] 
  
  ##### fin ---------------
  Final.df <- merge(var.summ, ef.dif) 
  Final.df$CI_lower <- CI[1]
  Final.df$CI_upper <- CI[2]
  
  names(Final.df)[1] <- 'Est'
  names(Final.df)[2] <- 'SE'
  names(Final.df)[3] <- 'p'
  
  return(Final.df)
  
}

###
f <- function(num){
  sapply(num, function(x){
    x <- ifelse(x < 0 & x > -0.001, 0.001, x)
    x <- round(x, 3)
    x
  })
}

###
fcomp <- function(num){
  num <- f(num)
  
  num <- as.data.frame(t(num), stringsAsFactors = FALSE)
  return(num)

}

###
se <- function(x) { sd(x)/sqrt(length(x)) }
