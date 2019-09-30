### NOT BY ME! ###

mcSummary <- function (object, correlation = FALSE, symbolic.cor = FALSE, 
                       ...) 
{
  z <- object
  p <- z$rank
  rdf <- z$df.residual
  if (p == 0) {
    r <- z$residuals
    n <- length(r)
    w <- z$weights
    if (is.null(w)) {
      rss <- sum(r^2)
    }
    else {
      rss <- sum(w * r^2)
      r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    ans <- z[c("call", if (!is.null(z$weights)) "weights")]
    df <- c(0L, n, n)
    SS <- c(NA, rss, rss)
    MS <- c(NA, resvar, resvar)
    F.list <- c(NA, NA, NA)
    r.squared <- adj.r.squared <- NA
    PRE.list <- c(r.squared, NA, NA)
    p.list <- c(NA, NA, NA)
    ans$anova <- round(cbind(SS, df, MS, PRE.list, F.list, p.list), digits = 3)
    dimnames(ans$anova) <- list(c("Model", "Error", "Corr Total"), c("SS", "df", "MS", "EtaSq", "F", "p"))
    rmse <- sqrt(resvar)
    ans$extras <- round(rbind(c(rmse, adj.r.squared)), digits = 3)
    dimnames(ans$extras) <- list("Model", c("RMSE", "AdjEtaSq"))
    return(ans)
  }
  
  if (is.null(z$terms)) 
    stop("invalid 'lm' object:  no 'terms' component")
  if (!inherits(object, "lm")) 
    warning("calling summary.lm(<fake-lm-object>) ...")
  Qr <- object$qr# qr.lm(object)
  n <- NROW(Qr$qr)
  if (is.na(z$df.residual) || n - p != z$df.residual) 
    warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
  r <- z$residuals
  f <- z$fitted.values
  w <- z$weights
  if (is.null(w)) {
    mss <- if (attr(z$terms, "intercept")) 
      sum((f - mean(f))^2)
    else sum(f^2)
    rss <- sum(r^2)
  }
  else {
    mss <- if (attr(z$terms, "intercept")) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    }
    else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }
  resvar <- rss/rdf
  if (is.finite(resvar) && resvar < (mean(f)^2 + var(f)) * 
      1e-30) 
    warning("essentially perfect fit: summary may be unreliable")
  p1 <- 1L:p
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- z$coefficients[Qr$pivot[p1]]
  tval <- est/se
  fval <- tval^2
  SS3val <- fval*resvar
  PREval <- SS3val/(SS3val+rss)
  ans <- z[c("call", if (!is.null(z$weights)) "weights")]
  if (attr(z$terms, "intercept")==1) {
    df.int <- 1L
    r.squared <- mss/(mss + rss)
    adj.r.squared <- 1 - (1 - r.squared) * ((n - df.int)/rdf)
    omni.F <- NA
    omni.p <- NA
    
  }
  else {
    df.int <- 0L
    r.squared <- adj.r.squared <- NA
    omni.F <- NA
    omni.p <- NA
  }
  if ((p-df.int>=1) & (df.int==1)) { 
    omni.F <- (mss/(p - df.int))/resvar
    omni.p <- pf(omni.F,p-df.int,rdf,lower.tail = FALSE)
  }
  if ((p-df.int>=2) & (df.int==1)) { 
    tol <- c(NA, 1/vif(z)) 
  }
  else { 
    tol <- rep(NA,p) 
  }
  df <- c(p-df.int, rdf, n-df.int)
  SS <- c(mss, rss, mss+rss)
  MS <- c(mss/(p-df.int), resvar, (mss+rss)/(rdf+p-df.int))
  F.list <- c(omni.F, NA, NA)
  PRE.list <- c(r.squared, NA, NA)
  p.list <- c(omni.p, NA, NA)
  ans$anova <- round(cbind(SS, df, MS, PRE.list, F.list, p.list), digits = 3)
  dimnames(ans$anova) <- list(c("Model", "Error", "Corr Total"), c("SS", "df", "MS", "EtaSq", "F", "p"))
  rmse <- sqrt(resvar)
  CI <- confint(z) 
  ans$extras <- round(rbind(c(rmse, adj.r.squared)), digits = 3)
  dimnames(ans$extras) <- list("Model", c("RMSE", "AdjEtaSq"))
  ans$coefficients <- round(cbind(est, se, tval, SS3val, PREval, tol, CI, 
                                  2 * pt(abs(tval), rdf, lower.tail = FALSE)), digits=3)
  dimnames(ans$coefficients) <- list(names(z$coefficients)[Qr$pivot[p1]], 
                                     c("Est", "StErr", "t", "SSR(3)", "EtaSq", "tol", "CI_2.5", "CI_97.5", "p"))
  if (!is.null(z$na.action)) 
    ans$na.action <- z$na.action
  #  class(ans) <- "summary.lm"
  return(ans)
}
