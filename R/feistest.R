#############################################
#### Augmented Regression Test FEIS - FE ####
#############################################
#' @title Augmented Regression Test
#'
#' @description
#' Estimates a regression-based Hausmann test for fixed effects individual slope models.
#'
#'
#' @details
#' The Hausmann test can be computed by estimating a correlated random effects model
#' \insertCite{@see @Wooldridge.2010.384, pp. 328-334}{feisr}. This is achieved by
#' estimating a Mundlak \insertCite{Mundlak.1978.0}{feisr} specification using random effects models
#' with \code{\link[plm]{plm}}.
#' Subsequently, \code{feistest} tests whether the time-constant variables / slope variables are correlated with
#' the unobserved heterogeneity by using a Wald-test with \code{\link[aod]{wald.test}}.
#'
#' While \code{type="art2"} estimates the conventional regression-based Hausmann test
#' \insertCite{@as described in @Wooldridge.2010.384, pp. 328-334}{feisr} comparing conventional
#' fixed effects models against random effects models, \code{type="art1"} estimates an extended
#' regression-based Hausmann test comparing fixed effects individual slope models and conventional
#' fixed effects model. For "\code{art1} the Mundlak-specification includes the person-specific averages,
#' but additionally the person-specific slope estimates used for "detrending" in \code{\link[feisr]{feis}}.
#' The Wald test of \code{type="art1"} is applied to the slope variables only.
#'
#' If specified, \code{feistest} uses panel-robust standard errors.
#'
#' @seealso
#' \code{\link[feisr]{feis}}, \code{\link[plm]{plm}}, \code{\link[aod]{wald.test}}, \code{\link[plm]{phtest}}
#'
#' @param model	an object of class "\code{feis}".
#' @param robust logical. If \code{TRUE} uses cluster robust standard errors (Default is \code{FALSE}).
#' @param type one of "\code{both}" (the Default), "\code{art1}", "\code{art2}".
#' @param ...	further arguments.
#'
#' @return An object of class "\code{feistest}", containing the following elements:
#' \item{wald_feis}{an object of class "\code{wald.test}" testing the fixed effects individual slopes model
#'   against the conventional fixed effects model.}
#' \item{wald_fe}{an object of class "\code{wald.test}" testing the fixed effects model
#'   against the random effects model.}
#' \item{vcov1}{the variance-covariance matrix of CREIS.}
#' \item{vcov2}{the variance-covariance matrix of CRE.}
#' \item{CREIS}{an object of class "\code{plm}" estimating a Correlated Random Effect Individual Slope model.}
#' \item{CRE}{an object of class "\code{plm}" estimating a Correlated Random Effect model.}
#' \item{call}{the matched call.}
#' \item{robust}{logical. If \code{TRUE} cluster robust standard errors were used.}
#' \item{formula}{an object of class "\code{Formula}" describing the model.}
#' \item{type}{the type of performed test(s).}
#' @references
#' \insertAllCited{}
#'
#' @examples
#' data("Produc", package = "plm")
#' feis.mod <- feis(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp | year,
#'                  data = Produc, id = "state", robust = TRUE)
#' ht <- feistest(feis.mod, robust = TRUE, type = "both")
#' summary(ht)
#' @export


#' @export
feistest <- function(model = NA, robust = FALSE, type = c("both", "art1", "art2"), ...){


  # require(plm)
  # require(aod)

  formula  <- model$formula
  data <- model$model
  i <- model$id
  cl <- model$call
  type <- type[1]

  # Names
  sv <- model$slopes
  cv <- names(model$coefficients)
  cv <- cv[which(cv != "(Intercept)")]
  rv <- all.vars(formula(formula, rhs = 0, lhs = 1))

  # Variables (clean AsIs expressions)
  Y <- as.matrix(model.response(data, "numeric"))

  X <- model.matrix(formula, data, rhs = 1, lhs = 0, cstcovar.rm = "all")
  colnames(X) <- cleani(colnames(X))
  X <- X[, which(colnames(X) %in% cleani(cv))]

  S <- model.matrix(formula, data, rhs = 2, lhs = 0, cstcovar.rm = "all")
  S <- S[, -1, drop = FALSE]
  colnames(S) <- cleani(colnames(S))

  # Transformed variables
  X_hat <- data.frame(model$modelhat)
  X_hat <- X_hat[, -1, drop = F]
  colnames(X_hat) <- cleani(colnames(X_hat))
  X_hat <- X_hat[, which(colnames(X_hat) %in% cleani(cv)), drop = F]
  colnames(X_hat) <- paste(colnames(X_hat), "_hat", sep = "")

  X_mean <- apply(X, 2, function(u) ave(u, i, FUN = function(v) mean(v)))
  if(ncol(X) < 2){
    X_mean <- data.frame(X_mean, row.names = seq_along(X_mean))
  }
  colnames(X_mean) <- paste(colnames(X_mean), "_mean", sep = "")

  S_mean <- apply(S, 2, function(u) ave(u, i, FUN = function(v) mean(v)))
  if(ncol(S) < 2){
    S_mean <- data.frame(S_mean, row.names = seq_along(S_mean))
  }
  colnames(S_mean) <- paste(colnames(S_mean), "_mean", sep = "")


  # Combine (with fake year)
  i2 <- ave(1:length(i), i, FUN = function(u) seq_along(u))
  df <- data.frame(id = i, id2 = i2, Y, X, X_hat, X_mean, S, S_mean)

  if(!type=="art2"){
    ### Set up formula

    fm <- paste(paste(colnames(X), collapse = " + "),
              paste(colnames(X_hat), collapse = " + "),
              paste(colnames(X_mean), collapse = " + "),
              paste(colnames(S), collapse = " + "),
              paste(colnames(S_mean), collapse = " + "), sep = " + ")
    fm <- as.formula(paste("Y", fm, sep = " ~ -1 +"))


    ### Estimate Correlated RE model

    creis.mod <- plm::plm(fm,
                   data = df, index = c("id", "id2"),
                   model = "random", effect = "individual", random.method = "walhus" )

    # Robust vcov if specified
    if(robust == TRUE){
      vcov1 <- plm::vcovHC(creis.mod, type = "sss")
      creis.mod$vcov <- vcov1
    } else{vcov1 <- creis.mod$vcov}

    ### Perform wald test

    v_hat <- colnames(X_hat)
    v_mean <- colnames(X_mean)
    v_smean <- colnames(S_mean)

    # FEIS-FE
    wt_feis <- aod::wald.test(b = coef(creis.mod), Sigma = vcov1,
                       Terms = which(names(creis.mod$coefficients) %in% v_hat))
  } else{wt_feis <- NULL; vcov1 <- NULL; creis.mod <- NULL}

  if(!type == "art1"){
    ### Set up formula

    fm <- paste(paste(colnames(X), collapse = " + "),
              paste(colnames(X_mean), collapse = " + "),
              paste(colnames(S), collapse = " + "),
              paste(colnames(S_mean), collapse = " + "), sep = " + ")
    #fm <- as.formula(paste("Y", fm, sep = " ~ -1 +"))
    fm <- as.formula(paste("Y", fm, sep = " ~ "))

    ### Estimate Correlated RE model

    cre.mod <- plm::plm(fm,
                 data = df, index = c("id", "id2"),
                 model = "random", effect = "individual", random.method = "walhus" )

    # Robust vcov if specified
    if(robust == TRUE){
      vcov2 <- plm::vcovHC(cre.mod, type = "sss")
      cre.mod$vcov <- vcov2
    } else{vcov2 <- cre.mod$vcov}

    ### Perform wald test

    v_mean <- colnames(X_mean)
    v_smean <- colnames(S_mean)

    # FE-RE
    wt_fe <- aod::wald.test(b = coef(cre.mod), Sigma = vcov2,
                     Terms = which(names(cre.mod$coefficients) %in% c(v_mean, v_smean)))
  } else{wt_fe <- NULL; vcov2 <- NULL; cre.mod <- NULL}





  result <- list(wald_feis    = wt_feis,
                 wald_fe      = wt_fe,
                 vcov1        = vcov1,
                 vcov2        = vcov2,
                 CREIS        = creis.mod,
                 CRE          = cre.mod)
  result$call <- cl
  result$robust <- robust
  result$formula <- formula
  result$type <- type

  class(result)  <-  c("feistest")


  return(result)
}
#' @exportClass feistest
