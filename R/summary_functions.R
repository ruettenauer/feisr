###########################
#### Describe function ####
###########################
#' @importFrom Rdpack reprompt


describe <- function(x,
                     what = c('model', 'effect', 'random.method',
                              'inst.method', 'transformation')){
  what <- match.arg(what)
  cl <- x$args
  switch(what,
         "model"          = ifelse(!is.null(cl$model), cl$model, "within"),
         "effect"         = ifelse(!is.null(cl$effect), cl$effect, "individual"),
         "random.method"  = ifelse(!is.null(cl$random.method),
                                   cl$random.method, "swar"),
         "inst.method"    = ifelse(!is.null(cl$inst.method),
                                   cl$inst.method, "bvk"),
         "transformation" = ifelse(!is.null(cl$transformation),
                                   cl$transformation, "d")
  )
}




######################
#### Summary feis ####
######################

#' @title Summary for feis objects
#'
#' @description
#' The summary method for feis objects generates some additional information
#' about estimated feis models.
#'
#' @param object an object of class "\code{plm}".
#' @param vcov a variance-covariance matrix furnished by the user or a function to calculate one.
#' @param ...	further arguments.
#'
#' @return An object of class "\code{summary.feis}", containing the elements
#' of the feis object (see \code{\link[feisr]{feis}}). The following objects
#' are modified:
#' \item{coefficients}{a matrix with the estimated coefficients, standard errors,
#' t-values, and p-values, if argument vcov is NULL the standard errors
#' are calculated by the \code{vcov} in the input object.}
#' \item{r.squared}{a vector containing R squared and adjusted R squared.}
#'
#' @examples
#' data("mwp", package = "feisr")
#' feis.mod <- feis(lnw ~ marry | exp,
#'                  data = mwp, id = "id")
#' summary(feis.mod)
#' @export
summary.feis <- function(object, vcov = NULL, ...){

  if(is.null(vcov)){
    vcov <- object$vcov
  }else{
    object$vcov <- vcov
    object$vcov_arg <- as.list(match.call())$vcov
  }

  #object$fstatistic <- pwaldtest(object, test = "F", vcov = vcov)
  model <- describe(object, "model")
  effect <- describe(object, "effect")

  object$r.squared <- c(rsq  = object$r2,
                        adjrsq = object$adj.r2)


  # construct the table of coefficients
  std.err <- sqrt(diag(vcov))
  b <- coefficients(object)
  z <- b / std.err
  p <- 2 * pt(abs(z), df = object$df.residual, lower.tail = FALSE)

  # construct the object of class summary.feis
  object$coefficients <- cbind("Estimate"   = b,
                               "Std. Error" = std.err,
                               "t-value"    = z,
                               "Pr(>|t|)"   = p)



  class(object) <- c("summary.feis", "feis")
  object
}



############################
#### Print Summary FEIS ####
############################

#' @export
print.summary.feis <- function(x, digits = max(3, getOption("digits") - 2),
                               width=getOption("width"), subset=NULL,  ...){
  formula <- formula(x)
  effect <- describe(x, "effect")
  model  <- describe(x, "model")

  cat("\n")
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  #pdim <- pdim(x)
  #print(pdim)

  cat("\nResiduals:\n")
  save.digits <- unlist(options(digits = digits))
  on.exit(options(digits = save.digits))
  print(sumres(x))

  cat("\nCoefficients:\n")
  if (is.null(subset)) printCoefmat(coef(x), digits = digits)
  else printCoefmat(coef(x)[subset, , drop = FALSE], digits = digits)
  cat("\n")
  cat(paste(x$vcov_arg, "\n", sep = ""))
  cat(paste("Slope parameters: ", paste(x$slopevars, collapse=", "),  "\n", collapse = ""))
  cat(paste("Total Sum of Squares:    ", signif(tss.feis(x), digits), "\n", sep = ""))
  cat(paste("Residual Sum of Squares: ", signif(rss.feis(x), digits), "\n", sep = ""))
  cat(paste("R-Squared:      ", signif(x$r.squared[1], digits),       "\n", sep = ""))
  cat(paste("Adj. R-Squared: ", signif(x$r.squared[2], digits),       "\n", sep = ""))
  # fstat <- x$fstatistic
  # if (names(fstat$statistic) == "F"){
  #   cat(paste("F-statistic: ",signif(fstat$statistic),
  #             " on ",fstat$parameter["df1"]," and ",fstat$parameter["df2"],
  #             " DF, p-value: ",format.pval(fstat$p.value,digits=digits), "\n", sep=""))
  # }
  # else{
  #   cat(paste("Chisq: ",signif(fstat$statistic),
  #             " on ",fstat$parameter,
  #             " DF, p-value: ",format.pval(fstat$p.value,digits=digits), "\n", sep=""))
  #
  #}
  invisible(x)
}



###################################################
#### Print Augmented Regression Test FEIS - FE ####
###################################################

#' @export
summary.feistest <- function(object, ...){
  class(object) <- c("summary.feistest", "feistest")
  object
}


#' @export
print.summary.feistest <- function(x, digits = max(3, getOption("digits") - 2),
                         width=getOption("width"),  ...){

  cl <- x$call
  type <- x$type

  name <- "Augmented Regression Test"
  if(x$robust == T){name <- paste("Robust", name)}

  wt_feis <- x$wald_feis
  wt_fe <- x$wald_fe
  wt_re <- x$wald_re

  Terms1  <-  wt_feis[["Terms"]]
  b1  <-  wt_feis[["b"]]
  H01  <-  wt_feis[["H0"]]
  v1  <-  wt_feis[["result"]][["chi2"]]
  df1  <-  wt_feis[["df"]]

  Terms2  <-  wt_fe[["Terms"]]
  b2  <-  wt_fe[["b"]]
  H02  <-  wt_fe[["H0"]]
  v2  <-  wt_fe[["result"]][["chi2"]]
  df2  <-  wt_fe[["df"]]

  Terms3  <-  wt_re[["Terms"]]
  b3  <-  wt_re[["b"]]
  H03  <-  wt_re[["H0"]]
  v3  <-  wt_re[["result"]][["chi2"]]
  df3  <-  wt_re[["df"]]

  names1 <- names(wt_feis$b)[Terms1]
  names2 <- names(wt_fe$b)[Terms2]
  names3 <- names(wt_re$b)[Terms3]


  cat("\n")
  cat("\nCall:\n")
  print(cl)
  cat("\n")

  cat(name, "\n")
  cat("\n")

  # FEIS-FE
  if(!type %in% c("art2", "art3")){
    cat("FEIS vs. FE:\n", "------------\n", sep = "")
    cat("H0: FEIS and FE estimates consistent", "\n")
    cat("Alternative H1: FE inconsistent", "\n")
    cat("Model constraints:", names1, "= 0","\n", fill = TRUE)
    cat("Chi-squared test:\n")
    cat("Chisq = ", format(v1["chi2"], digits = digits, nsmall = 1), ", df = ", v1["df"],
        ", P(> X2) = ", format(v1["P"], digits = digits, nsmall = 1), "\n", sep = "")
  }

  if(type == "all"){
    cat("\n")
    cat("\n")
  }

  # FE-RE
  if(!type %in% c("art1", "art3")){
    cat("FE vs. RE:\n", "------------\n", sep = "")
    cat("H0: FE and RE estimates consistent", "\n")
    cat("Alternative H1: RE inconsistent", "\n")
    cat("Model constraints:", names2, "= 0","\n", fill = TRUE)
    cat("Chi-squared test:\n")
    cat("Chisq = ", format(v2["chi2"], digits = digits, nsmall = 1), ", df = ", v2["df"],
        ", P(> X2) = ", format(v2["P"], digits = digits, nsmall = 1), "\n", sep = "")
  }

  if(type == "all"){
    cat("\n")
    cat("\n")
  }

  # FEIS-RE
  if(!type %in% c("art1", "art2")){
    cat("FEIS vs. RE:\n", "------------\n", sep = "")
    cat("H0: FEIS and RE estimates consistent", "\n")
    cat("Alternative H1: RE inconsistent", "\n")
    cat("Model constraints:", names3, "= 0","\n", fill = TRUE)
    cat("Chi-squared test:\n")
    cat("Chisq = ", format(v3["chi2"], digits = digits, nsmall = 1), ", df = ", v3["df"],
        ", P(> X2) = ", format(v3["P"], digits = digits, nsmall = 1), "\n", sep = "")
  }


  invisible(x)

}



######################################################
#### Print Bootstrapped Regression Test FEIS - FE ####
######################################################

#' @export
summary.bsfeistest <- function(object, ...){
  class(object) <- c("summary.bsfeistest", "bsfeistest")
  object
}


#' @export
print.summary.bsfeistest <- function(x, digits = max(3, getOption("digits") - 2),
                                   width=getOption("width"),  ...){

  cl <- x$call
  type <- x$type

  name <- "Bootstrapped Hausman Test"

  wt_feis <- x$wald_feis
  wt_fe <- x$wald_fe
  wt_re <- x$wald_re

  Terms1  <-  wt_feis[["Terms"]]
  b1  <-  wt_feis[["b"]]
  H01  <-  wt_feis[["H0"]]
  v1  <-  wt_feis[["result"]][["chi2"]]
  df1  <-  wt_feis[["df"]]

  Terms2  <-  wt_fe[["Terms"]]
  b2  <-  wt_fe[["b"]]
  H02  <-  wt_fe[["H0"]]
  v2  <-  wt_fe[["result"]][["chi2"]]
  df2  <-  wt_fe[["df"]]

  Terms3  <-  wt_re[["Terms"]]
  b3  <-  wt_re[["b"]]
  H03  <-  wt_re[["H0"]]
  v3  <-  wt_re[["result"]][["chi2"]]
  df3  <-  wt_re[["df"]]

  names1 <- names(wt_feis$b)[Terms1]
  names2 <- names(wt_fe$b)[Terms2]
  names3 <- names(wt_re$b)[Terms3]


  cat("\n")
  cat("\nCall:\n")
  print(cl)
  cat("\n")

  cat(name, "\n")
  cat("Repetitions:", nrow(x$bscoef.feis), "\n")
  cat("\n")

  # FEIS-FE
  if(!type %in% c("bs2", "bs3")){
    cat("FEIS vs. FE:\n", "------------\n", sep = "")
    cat("H0: FEIS and FE estimates consistent", "\n")
    cat("Alternative H1: FE inconsistent", "\n")
    cat("Model constraints:", "beta_FEIS", "=", "beta_FE", "\n", fill = TRUE)
    cat("Chi-squared test:\n")
    cat("Chisq = ", format(v1["chi2"], digits = digits, nsmall = 1), ", df = ", v1["df"],
        ", P(> X2) = ", format(v1["P"], digits = digits, nsmall = 1), "\n", sep = "")
  }

  if(type == "all"){
    cat("\n")
    cat("\n")
  }

  # FE-RE
  if(!type %in% c("bs1", "bs3")){
    cat("FE vs. RE:\n", "------------\n", sep = "")
    cat("H0: FE and RE estimates consistent", "\n")
    cat("Alternative H1: RE inconsistent", "\n")
    cat("Model constraints:", "beta_FE", "=", "beta_RE", "\n", fill = TRUE)
    cat("Chi-squared test:\n")
    cat("Chisq = ", format(v2["chi2"], digits = digits, nsmall = 1), ", df = ", v2["df"],
        ", P(> X2) = ", format(v2["P"], digits = digits, nsmall = 1), "\n", sep = "")
  }

  if(type == "all"){
    cat("\n")
    cat("\n")
  }

  # FEIS-RE
  if(!type %in% c("bs1", "bs2")){
    cat("FEIS vs. RE:\n", "------------\n", sep = "")
    cat("H0: FEIS and RE estimates consistent", "\n")
    cat("Alternative H1: RE inconsistent", "\n")
    cat("Model constraints:", "beta_FEIS", "=", "beta_RE", "\n", fill = TRUE)
    cat("Chi-squared test:\n")
    cat("Chisq = ", format(v3["chi2"], digits = digits, nsmall = 1), ", df = ", v3["df"],
        ", P(> X2) = ", format(v3["P"], digits = digits, nsmall = 1), "\n", sep = "")
  }

  invisible(x)

}


#######################################
#### Extract Function (for texreg) ####
#######################################

# #' @title Extract method for \code{feis}-class
# #'
# #' @description
# #' Provides an extract method for usage of \code{\link[texreg]{texreg}} with \code{feis}-class.
# #'
# #'@seealso \code{\link[texreg]{texreg}}
# #'
# #' @param model	an object of class \code{feis}
# #' @param include.rsquared logical. If \code{TRUE} (default) R squared is reported.
# #' @param include.adjrs logical. If \code{TRUE} (default) adjusted R squared is reported.
# #' @param include.nobs logical. If \code{TRUE} number of observations is reported.
# #' @param include.groups logical. If \code{TRUE} number of groups is reported.
# #' @param include.rmse logical. If \code{TRUE} RMSE is reported.
# #' @param ...	further arguments.
# #'
# #' @examples
# #' library(texreg)
# #'
# #' setMethod("extract", signature = className("feis", "feisr"),
# #'           definition = extract.feis)
# #'
# #' data("mwp", package = "feisr")
# #' feis1.mod <- feis(lnw ~ marry + as.factor(yeargr)
# #'                   | exp + I(exp^2), data = mwp, id = "id")
# #'
# #' feis2.mod <- feis(lnw ~ marry + enrol + as.factor(yeargr)
# #'                   | exp + I(exp^2), data = mwp, id = "id")
# #' screenreg(list(feis1.mod, feis2.mod))
# #'
# #'@export
# #'
extract.feis <- function(model, include.rsquared = TRUE, include.adjrs = TRUE,
                         include.nobs = TRUE, include.groups = TRUE,
                         include.rmse = TRUE, ...) {
  s <- summary(model, ...)

  coefficient.names <- rownames(coef(s))
  coefficients <- coef(s)[, 1]
  standard.errors <- coef(s)[, 2]
  significance <- coef(s)[, 4]

  rs <- s$r.squared[1]
  adj <- s$r.squared[2]
  n <- length(model$residuals)

  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.rsquared == TRUE) {
    gof <- c(gof, rs)
    gof.names <- c(gof.names, "R$^2$")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.adjrs == TRUE) {
    gof <- c(gof, adj)
    gof.names <- c(gof.names, "Adj.\ R$^2$")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num.\ obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.groups == TRUE) {
    grps <-length(unique(model$id))
    grp.names <- model$call[[match(c("id"), names(model$call))]]
    grp.names <- paste("Num.\ groups:", grp.names)
    gof <- c(gof, grps)
    gof.names <- c(gof.names, grp.names)
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (include.rmse == TRUE) {
    rmse <- sqrt(sum((model$residuals * model$residuals)) / model$df.residual)
    gof <- c(gof, rmse)
    gof.names <- c(gof.names, "RMSE")
    gof.decimal <- c(gof.decimal, TRUE)
  }

  tr <- texreg::createTexreg(
    coef.names = coefficient.names,
    coef = coefficients,
    se = standard.errors,
    pvalues = significance,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}

