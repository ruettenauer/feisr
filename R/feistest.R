#############################################
#### Artificial Regression Test FEIS - FE ####
#############################################
#' @importFrom Rdpack reprompt
#' @title Artificial Regression Test
#'
#' @description
#' Estimates a regression-based Hausman test for fixed effects individual slope models.
#'
#'
#' @details
#' The Hausman test can be computed by estimating a correlated random effects model
#' \insertCite{@see @Wooldridge.2010.384, pp. 328-334, @Ruttenauer.2020}{feisr}. This is achieved by
#' estimating a Mundlak \insertCite{Mundlak.1978.0}{feisr} specification using random effects models
#' with \code{\link[plm]{plm}}.
#' Subsequently, \code{feistest} tests whether the time-constant variables / slope variables are correlated with
#' the unobserved heterogeneity by using a Wald chi-squared test.
#'
#' \code{type="art1"} estimates an extended regression-based Hausman test comparing fixed effects
#' individual slope models and conventional fixed effects models. For \code{art1} the
#' Mundlak-specification \insertCite{Mundlak.1978.0}{feisr} includes the person-specific averages,
#' but additionally the person-specific slope estimates used for "detrending" in \code{\link[feisr]{feis}}.
#' This allows to test whether we can omit the estimated values based on the slopes and reduce the model
#' to a conventional FE model. The Wald test of \code{type="art1"} is applied to the slope variables only.
#' \code{type="art2"} estimates the conventional regression-based Hausman test
#' \insertCite{@as described in @Wooldridge.2010.384, pp. 328-334}{feisr} comparing conventional
#' fixed effects models against random effects models.
#' \code{type="art3"} estimates a regression-based Hausman test comparing FEIS directly against RE,
#' thereby testing for inconsistency of the RE model due to either heterogeneous slopes or time-constant
#' omitted heterogeneity. For \code{art3} the Mundlak-specification includes only the person-specific
#' slopes, and no averages. This allows to test whether we can omit the estimated values based on
#' the slopes and reduce the model to a conventional RE model.
#' \insertCite{@for a formal description please see @Ruttenauer.2020}{feisr}.
#'
#' Currently, the \code{tol} option in \code{feis()} is only forwarded in bsfeistest,
#' but not in feistest.
#'
#'
#'
#' If specified (\code{robust=TRUE}), \code{feistest} uses panel-robust standard errors.
#'
#' @seealso
#' \code{\link[feisr]{summary.feistest}}, \code{\link[feisr]{bsfeistest}},
#' \code{\link[feisr]{feis}},  \code{\link[plm]{plm}},
#' \code{\link[plm]{phtest}}
#'
#' @param model	an object of class "\code{feis}".
#' @param robust logical. If \code{TRUE} uses cluster robust standard errors (Default is \code{FALSE}).
#' @param type one of "\code{all}" (the Default), "\code{art1}" for test of FEIS against FE only,
#' "\code{art2}" for test of FE against RE only, and "\code{art3}" for test of FEIS against RE only
#' (see also Details).
#' @param terms An optional character vector specifying which coefficients should be jointly tested.
#' By default, all covariates are included in the Wchi-squared test. For "\code{type=art2}", the
#' slope variable is always included in "\code{terms}".
#' @param ...	further arguments.
#'
#' @return An object of class "\code{feistest}", containing the following elements:
#' \item{wald_feis}{an object of class "\code{wald.test}" testing
#' the fixed effects individual slopes model against the conventional fixed effects model
#' (\code{type="art1"}).}
#' \item{wald_fe}{an object of class "\code{wald.test}" testing
#' the fixed effects model against the random effects model (\code{type="art2"}).}
#' \item{wald_re}{an object of class "\code{wald.test}" testing
#' the fixed effects individual slopes model against the random effects model (\code{type="art3"}).}
#' \item{vcov1}{the variance-covariance matrix of CREIS (\code{type="art1"}).}
#' \item{vcov2}{the variance-covariance matrix of CRE (\code{type="art2"}).}
#' \item{vcov3}{the variance-covariance matrix of CREIS without the means (\code{type="art3"}).}
#' \item{CREIS}{an object of class "\code{plm}" (see \code{\link[plm]{plm}}) estimating a Correlated
#' Random Effect Individual Slope model (\code{type="art1"}).}
#' \item{CRE}{an object of class "\code{plm}" (see \code{\link[plm]{plm}}) estimating a Correlated
#' Random Effect model (\code{type="art2"}).}
#' \item{CREIS2}{an object of class "\code{plm}" (see \code{\link[plm]{plm}}) estimating a
#' Correlated Random Effect Individual Slope model without including the covariates' means
#' (\code{type="art3"}).}
#' \item{call}{the matched call.}
#' \item{robust}{logical. If \code{TRUE} cluster robust standard errors were used
#' (Default is \code{FALSE}.}
#' \item{formula}{an object of class "\code{Formula}" describing the model.}
#' \item{type}{the type of performed test(s).}
#' \item{terms}{character vector of covariates are included in the Wchi-squared test.}
#' @references
#' \insertAllCited{}
#'
#' @examples
#' data("mwp", package = "feisr")
#' feis.mod <- feis(lnw ~ marry + enrol | year,
#'                  data = mwp, id = "id", robust = TRUE)
#' ht <- feistest(feis.mod, robust = TRUE, type = "all")
#' summary(ht)
#' # Only for marry coefficient
#' ht2 <- feistest(feis.mod, robust = TRUE, type = "all", terms = c("marry"))
#' summary(ht2)
#'
#' @export
#'
feistest <- function(model = NA, robust = FALSE, type = c("all", "art1", "art2", "art3"),
                     terms = NULL, ...){


  formula  <- model$formula
  data <- model$model
  i <- model$id
  cl <- model$call
  type <- match.arg(type)

  if (!type %in% c("all", "art1", "art2", "art3")){
    stop(paste("type must be one of \"all\", \"art1\", \"art2\", \"art3\""))
  }

  # Names
  sv <- model$slopevars
  cv <- names(model$coefficients)
  cv <- cv[which(cv != "(Intercept)")]
  rv <- all.vars(formula(formula, rhs = 0, lhs = 1))

  # Variables (clean AsIs expressions)
  Y <- as.matrix(model.response(data, "numeric"))

  X <- model.matrix(formula, data, rhs = 1, lhs = 0, cstcovar.rm = "all")
  colnames(X) <- cleani(colnames(X))
  X <- X[, match(cleani(cv), colnames(X)), drop = F]

  S <- model.matrix(formula, data, rhs = 2, lhs = 0, cstcovar.rm = "all")
  S <- S[, -1, drop = FALSE]
  colnames(S) <- cleani(colnames(S))

  # Transformed data
  Xtrans <- model$modeltrans
  colnames(Xtrans) <- cleani(colnames(Xtrans))
  Xtrans <- Xtrans[, match(cleani(cv), colnames(Xtrans)), drop = FALSE]

  # Get weights
  w <- model$weights
  if(all(w == 1)){
    w <- 1
    isw <- FALSE
  }else {
    isw <- TRUE
  }

  # Check if terms fit colnames
  if(!is.null(terms)){
    terms <- cleani(terms)
    incl <- which(terms %in% colnames(X))
    if(length(incl) != length(terms)){
      excl <- terms[which(!terms %in% colnames(X))]
      stop(paste("All terms must be included in model. Could not find:",
                 paste(excl, collapse = ", "), "\n",
                 "Available are:", paste(colnames(X), collapse = ", ")))
    }
  }



  # Transformed variables
  X_hat <- X - Xtrans
  X_hat <- X_hat
  colnames(X_hat) <- cleani(colnames(X_hat))
  X_hat <- X_hat[, which(colnames(X_hat) %in% cleani(cv)), drop = F]
  colnames(X_hat) <- paste(colnames(X_hat), "_hat", sep = "")

  # Generate (weighted) means
  X_mean <- apply(X, 2, function(u) ave_wm(x = u, i = i, w = w))
  if(ncol(X) < 2){
    X_mean <- data.frame(X_mean, row.names = seq_along(X_mean))
  }
  colnames(X_mean) <- paste(colnames(X), "_mean", sep = "")

  S_mean <- apply(S, 2, function(u) ave_wm(x = u, i = i, w = w))
  if(ncol(S) < 2){
    S_mean <- data.frame(S_mean, row.names = seq_along(S_mean))
  }
  colnames(S_mean) <- paste(colnames(S), "_mean", sep = "")

  # If weights, pre-weight data
  X <- X * sqrt(w)
  S <- S * sqrt(w)
  Y <- Y * sqrt(w)
  X_hat <- X_hat * sqrt(w)
  X_mean <- X_mean * sqrt(w)
  S_mean <- S_mean * sqrt(w)

  # Check for and drop NA coef columns
  if(any(is.na(model$coefficients))){
    drop <- which(is.na(model$coefficients))

    X <- X[, -drop, drop = FALSE]
    X_hat <- X_hat[, -drop, drop = FALSE]
    X_mean <- X_mean[, -drop, drop = FALSE]

  }

  # Drop zero weights
  if(isw){
    zerow <- which(w == 0)
    if(length(zerow) > 0){
      oonz <- which(w != 0)
    }else{
      oonz <- 1:nrow(X)
    }
  }else{
    oonz <- 1:nrow(X)
  }

  # Combine (with fake year)
  i2 <- ave(1:length(i), i, FUN = function(u) seq_along(u))
  df <- data.frame(id = i, id2 = i2, Y, X, X_hat, X_mean, S, S_mean)[oonz, ]

  ### FEIS vs FE
  if(!type %in% c("art2", "art3")){
    ### Set up formula

    fm <- paste(paste(colnames(X), collapse = " + "),
              paste(colnames(X_hat), collapse = " + "),
              paste(colnames(X_mean), collapse = " + "),
              paste(colnames(S), collapse = " + "),
              paste(colnames(S_mean), collapse = " + "), sep = " + ")
    #fm <- as.formula(paste("Y", fm, sep = " ~ -1 +"))
    fm <- as.formula(paste("Y", fm, sep = " ~ "))

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

    if(is.null(terms)){
      v_hat <- colnames(X_hat)
    }else{
      v_hat <- paste(terms, "_hat", sep = "")
    }

    wt_feis <- wald.test(b = coef(creis.mod), Sigma = vcov1,
                         Terms = which(names(creis.mod$coefficients) %in% v_hat))
  } else{wt_feis <- NULL; vcov1 <- NULL; creis.mod <- NULL}


  ### FE vs RE
  if(!type %in% c("art1", "art3")){
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

    if(is.null(terms)){
      v_mean <- colnames(X_mean)
      v_smean <- colnames(S_mean)
    }else{
      v_mean <- paste(terms, "_mean", sep = "")
      v_smean <- colnames(S_mean) # Makes sense to always include?
    }

    # FE-RE
    wt_fe <- wald.test(b = coef(cre.mod), Sigma = vcov2,
                       Terms = which(names(cre.mod$coefficients) %in% c(v_mean, v_smean)))
  } else{wt_fe <- NULL; vcov2 <- NULL; cre.mod <- NULL}


  ### FEIS vs RE
  if(!type %in% c("art1", "art2")){
    ### Set up formula

    fm <- paste(paste(colnames(X), collapse = " + "),
                paste(colnames(X_hat), collapse = " + "),
                paste(colnames(S), collapse = " + "), sep = " + ")
    #fm <- as.formula(paste("Y", fm, sep = " ~ -1 +"))
    fm <- as.formula(paste("Y", fm, sep = " ~ "))

    ### Estimate Correlated RE model

    creis2.mod <- plm::plm(fm,
                          data = df, index = c("id", "id2"),
                          model = "random", effect = "individual", random.method = "walhus" )

    # Robust vcov if specified
    if(robust == TRUE){
      vcov3 <- plm::vcovHC(creis2.mod, type = "sss")
      creis2.mod$vcov <- vcov3
    } else{vcov3 <- creis2.mod$vcov}

    ### Perform wald test

    if(is.null(terms)){
      v_hat <- colnames(X_hat)
    }else{
      v_hat <- paste(terms, "_hat", sep = "")
    }

    # FEIS - RE
    wt_re <- wald.test(b = coef(creis2.mod), Sigma = vcov3,
                       Terms = which(names(creis.mod$coefficients) %in% v_hat))
  } else{wt_re <- NULL; vcov3 <- NULL; creis2.mod <- NULL}



  result <- list(wald_feis    = wt_feis,
                 wald_fe      = wt_fe,
                 wald_re      = wt_re,
                 vcov1        = vcov1,
                 vcov2        = vcov2,
                 vcov3        = vcov3,
                 CREIS        = creis.mod,
                 CRE          = cre.mod,
                 CREIS2       = creis2.mod)
  result$call <- cl
  result$robust <- robust
  result$formula <- formula
  result$type <- type
  result$terms <- terms

  class(result)  <-  c("feistest")


  return(result)
}





##########################################
#### Hausman test FEIS - FE bootstrap ####
##########################################
#' @importFrom Rdpack reprompt
#' @title Bootstrapped Regression Test
#'
#' @description
#' Estimates a bootstrapped Hausman test for fixed effects individual slope models.
#'
#'
#' @details
#' The function computes a bootstrapped version of the Hausman test \insertCite{Hausman.1978.0}{feisr}.
#' Pairs cluster bootstrapping \insertCite{Cameron.2008,Ruttenauer.2020}{feisr} is used to obtain the empirical
#' variance-covariance matrix of the estimators, either for FEIS and conventional FE, convention FE and RE,
#' or FEIS and RE.
#'
#' \code{type="bs1"} estimates a bootstrapped Hausman test comparing fixed effects individual slope
#' models and conventional fixed effects models. In this case, \code{bsfeistest} tests for
#' inconsistency of the convetional FE model due to heterogeneous slopes.
#' \code{type="bs2"} estimates a bootstrapped version of the well-known Hausman test comparing conventional
#' fixed effects models against random effects models.
#' \code{type="bs3"} estimates a bootstrapped Hausman directly comparing FEIS against RE, thereby testing
#' for inconsistency of the RE model due to either heterogeneous slopes or time-constant omitted heterogeneity.
#' Bootstrapping is perfomed by resampling with replacement while keeping the number of groups identical to
#' the number of groups in the original dataset. A wald test from aod package is used to perform a Wald
#' chi-squared test on the differences between coefficients.
#'
#' @seealso
#' \code{\link[feisr]{summary.feistest}}, \code{\link[feisr]{feistest}},
#' \code{\link[feisr]{feis}},  \code{\link[plm]{plm}},
#' \code{\link[plm]{phtest}}
#'
#' @param model	an object of class "\code{feis}".
#' @param type one of "\code{all}" (the Default), "\code{bs1}" for test of FEIS against FE only,
#' "\code{bs2}" for test of FE against RE only, and "\code{bs3}" for test of FEIS against RE only
#' (see also Details).
#' @param terms An optional character vector specifying which coefficients should be jointly tested.
#' By default, all covariates are included in the Wchi-squared test. For "\code{type=art2}", the
#' slope variable is always included in "\code{terms}".
#' @param rep the number of repetitions to be used in bootstrapping (default is 500).
#' @param seed the seed used for random sampling in bootstrapping. Needs to be a valid integer.
#' If not specified, the current seed is used.
#' @param prog ...	logical. If \code{TRUE} (the Default) shows the progress in the output window.
#' @param ...	further arguments.
#'
#' @return An object of class "\code{feistest}", containing the following elements:
#' \item{wald_feis}{an object of class "\code{wald.test}"
#'  testing the fixed effects individual slopes model against the conventional fixed effects
#'  model (\code{type="bs1"}).}
#' \item{wald_fe}{an object of class "\code{wald.test}"
#'  testing the fixed effects model against the random effects model (\code{type="bs2"}).}
#' \item{wald_re}{an object of class "\code{wald.test}"
#' testing the fixed effects individual slopes model against the random effects model
#' (\code{type="bs3"}).}
#' \item{vcov1}{the empirical (bootstrapped) variance-covariance matrix of the
#' coefficients obtained from FEIS and FE (\code{type="bs1"}).}
#' \item{vcov2}{the empirical (bootstrapped) variance-covariance matrix of the
#' coefficients obtained from FE and RE (\code{type="bs2"}).}
#' \item{vcov3}{the empirical (bootstrapped) variance-covariance matrix of the
#' coefficients obtained from FEIS and RE (\code{type="bs3"}).}
#' \item{bscoef.feis}{a matrix containing the estimated FEIS coefficients of each bootstrap run.}
#' \item{bscoef.fe}{a matrix containing the estimated FE coefficients of each bootstrap run.}
#' \item{bscoef.re}{a matrix containing the estimated RE coefficients of each bootstrap run.}
#' \item{call}{the matched call.}
#' \item{formula}{an object of class "\code{Formula}" describing the model.}
#' \item{type}{the type of performed test(s).}
#' \item{sample}{a list containing the IDs sampled in each run.}
#' \item{seed}{the seed used for bootstrapping.}
#' \item{terms}{character vector of covariates are included in the Wchi-squared test.}
#' @references
#' \insertAllCited{}
#'
#' @examples
#' data("mwp", package = "feisr")
#' \dontrun{
#' feis.mod <- feis(lnw ~ marry + enrol | year,
#'                  data = mwp, id = "id", robust = TRUE)
#' bsht <- bsfeistest(feis.mod, type = "bs1", rep = 100, seed = 1234)
#' summary(bsht)
#' }
#'
#' @export
#'
bsfeistest <- function(model = NA, type = c("all", "bs1", "bs2", "bs3"),
                       terms = NULL, rep = 500, seed = NULL, prog = TRUE, ...){



  formula  <- model$formula
  data <- model$model
  i <- model$id
  nc <- length(model$coefficients)
  ns <- length(model$slopevars)
  cl <- model$call
  tol <- model$tol
  type <- match.arg(type)

  if (!type %in% c("all", "bs1", "bs2", "bs3")){
    stop(paste("type must be one of \"all\", \"bs1\", \"bs2\", \"bs3\""))
  }

  # Names
  sv <- model$slopevars
  cv <- names(model$coefficients)
  cv <- cv[which(cv != "(Intercept)")]
  rv <- all.vars(formula(formula, rhs = 0, lhs = 1))


  ### Build data frame

  # Variables (clean AsIs expressions)
  Y <- as.matrix(model.response(data, "numeric"))

  X <- model.matrix(formula, data, rhs = 1, lhs = 0, cstcovar.rm = "all")
  colnames(X) <- cleani(colnames(X))
  X <- X[, match(cleani(cv), colnames(X)), drop = F]

  S <- model.matrix(formula, data, rhs = 2, lhs = 0, cstcovar.rm = "all")
  S <- S[, -1, drop = FALSE]
  colnames(S) <- cleani(colnames(S))

  # Check for and drop NA coef columns
  if(any(is.na(model$coefficients))){
    drop <- which(is.na(model$coefficients))

    X <- X[, -drop, drop = FALSE]
  }

  # Get weights
  w <- model$weights
  if(all(w == 1)){
    w <- 1
    isw <- FALSE
  }else {
    isw <- TRUE
  }

  # If weights, pre-weight data
  X <- X * sqrt(w)
  S <- S * sqrt(w)
  Y <- Y * sqrt(w)

  # Drop zero weights
  if(isw){
    zerow <- which(w == 0)
    if(length(zerow) > 0){
      oonz <- which(w != 0)
    }else{
      oonz <- 1:nrow(X)
    }
  }else{
    oonz <- 1:nrow(X)
  }

  # Combine (with fake year)
  i2 <- ave(1:length(i), i, FUN = function(u) seq_along(u))
  df <- data.frame(id = i, id2 = i2, Y, X, S)[oonz, ]
  ids <- unique(df$id)
  i <- df$id
  N <- length(i)
  n <- length(ids)


  ### Set up formulas

  # FE and FEIS
  fm.fe <- paste(paste(colnames(X), collapse = " + "),
              paste(colnames(S), collapse = " + "), sep = " + ")
  fm.feis <- paste(paste(colnames(X), collapse = " + "),
                 paste(colnames(S), collapse = " + "), sep = " | ")

  fm.fe <- as.formula(paste("Y", fm.fe, sep = " ~ "))
  fm.feis <- as.formula(paste("Y", fm.feis, sep = " ~ "))


  ### Coefs from full sample

  # FE
  if(!type == "bs3"){
    fe.mod <- plm::plm(fm.fe, data = df, index = c("id", "id2"),
                       effect = "individual", model = "within")

    coef.fe <- fe.mod$coefficients
  } else {coef.fe <- NA}

  # FEIS
  if(!type == "bs2"){
    feis.mod <- feis(fm.feis, data = df, id = "id",  tol = tol)

    coef.feis <- feis.mod$coefficients
  } else {coef.feis <- NA}

  # RE
  if(!type=="bs1"){
    re.mod <- plm::plm(fm.fe, data = df, index = c("id", "id2"),
                       effect = "individual", model = "random")

    coef.re <- re.mod$coefficients

    if(any(names(coef.re) == "(Intercept)")){
      drop <- which(names(coef.re) == "(Intercept)")
      coef.re <- coef.re[-drop]
    }
  } else {coef.re <- NA}


  ### Set up results matrices for bs coefs

  # Matrix for coefficients
  mat.coef.feis <- matrix(NA, nrow = rep, ncol = ncol(X))
  mat.coef.fe <- matrix(NA, nrow = rep, ncol = (ncol(X) + ncol(S)))
  mat.coef.re <- matrix(NA, nrow = rep, ncol = (ncol(X) + ncol(S)))

  colnames(mat.coef.feis) <- colnames(X)
  colnames(mat.coef.fe) <- colnames(mat.coef.re) <- c(colnames(X), colnames(S))

  # Sample id list
  sample <- list()


  ### Start bootstrap simulation

  # Seed
  if(is.null(seed)){
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) stats::runif(1)
    seed <- .Random.seed
  }

  set.seed(seed)

  # Loop
  for(j in 1:rep){

    ### Select sample
    sids <- sample(ids, n, replace = TRUE)
    sample[[j]] <- sids

    oo <- lapply(sids, function(x) which(df$id %in% x))
    df.tmp <- df[unlist(oo), ]
    w.tmp <- w[unlist(oo)]

    # New ids
    loo <- unlist(lapply(oo, function(x) length(x)))
    df.tmp$sid <- rep(1:length(loo), times = loo)

    ### Run models

    # FE
    if(!type=="bs3"){
      tmp.fe <- plm::plm(fm.fe, data = df.tmp, index = c("sid", "id2"),
                         effect = "individual", model = "within")

      mat.coef.fe[j, ] <- t(tmp.fe$coefficients)
    }

    # FEIS
    if(!type=="bs2"){
      tmp.feis <- feis(fm.feis, data = df.tmp, id = "sid", tol = tol)

      mat.coef.feis[j, ] <- t(tmp.feis$coefficients)
    }

    # RE
    if(!type=="bs1"){
      tmp.re <- plm::plm(fm.fe, data = df.tmp, index = c("sid", "id2"),
                         effect = "individual", model = "random")

      if(any(names(tmp.re$coefficients) == "(Intercept)")){
        drop <- which(names(tmp.re$coefficients) == "(Intercept)")
        tmp.re$coefficients <- tmp.re$coefficients[-drop]
      }

      mat.coef.re[j, ] <- t(tmp.re$coefficients)
    }

    # Print runs
    if(prog == TRUE){
      if(j == 1){
        cat("Simulations completed (by 10):\n")
      }
      if(j %% 100 == 0){
        if(j %% 200 == 0){
          cat(" ", j, fill = TRUE)
        }else cat(" ", j, " ")
      }else if(j %% 10 == 0){
        cat("+")
      }
      if(j == rep){
        cat("\n")
      }
    }



  }


  ### Differences in coefficients

  mat.bdiff.bs1 <- mat.coef.feis - mat.coef.fe[, colnames(mat.coef.feis)]
  mat.bdiff.bs2 <- mat.coef.fe - mat.coef.re
  mat.bdiff.bs3 <- mat.coef.feis - mat.coef.re[, colnames(mat.coef.feis)]

  I <- matrix(1, ncol = 1, nrow = rep)
  mean.feis <- I %*% crossprod(I, mat.coef.feis) / rep
  mean.fe <- I %*% crossprod(I, mat.coef.fe) / rep
  mean.re <- I %*% crossprod(I, mat.coef.re) / rep

  mat.mdiff.bs1 <- mean.feis - mean.fe[, colnames(mean.feis)]
  mat.mdiff.bs2 <- mean.fe - mean.re
  mat.mdiff.bs3 <- mean.feis - mean.re[, colnames(mean.feis)]

  mat.diff.bs1 <- mat.bdiff.bs1 - mat.mdiff.bs1
  mat.diff.bs2 <- mat.bdiff.bs2 - mat.mdiff.bs2
  mat.diff.bs3 <- mat.bdiff.bs3 - mat.mdiff.bs3


  ### Covariance matrix

  V.bs1 <- crossprod(mat.diff.bs1) / (rep - 1)
  V.bs2 <- crossprod(mat.diff.bs2) / (rep - 1)
  V.bs3 <- crossprod(mat.diff.bs3) / (rep - 1)


  ### Test statistic

  bdiff.bs1 <- coef.feis - coef.fe[names(coef.feis)]
  bdiff.bs2 <- coef.fe - coef.re
  bdiff.bs3 <- coef.feis - coef.re[names(coef.feis)]

  # Check if terms fit colnames
  if(!is.null(terms)){
    terms <- cleani(terms)
    incl <- which(terms %in% names(bdiff.bs1))
    if(length(incl) != length(terms)){
      excl <- terms[which(!terms %in% names(bdiff.bs1))]
      stop(paste("All terms must be included in model. Could not find:",
                 paste(excl, collapse = ", "), "\n",
                 "Available are:", paste(names(bdiff.bs1), collapse = ", ")))
    }
  }

  # H.bs1 <- t(bdiff.bs1) %*% solve(V.bs1) %*% bdiff.bs1
  # H.bs1 <- t(bdiff.bs2) %*% solve(V.bs2) %*% bdiff.bs2
  if(!type %in% c("bs2", "bs3")){
    if(!is.null(terms)){
      tt1 <- which(names(bdiff.bs1) %in% terms)
    }else{
      tt1 <- 1:length(bdiff.bs1)
    }
    H.bs1 <- wald.test(b = bdiff.bs1, Sigma = V.bs1,
                       Terms = tt1)
  } else {H.bs1 <- NULL}

  if(!type %in% c("bs1", "bs3")){
    if(!is.null(terms)){
      tt2 <- which(names(bdiff.bs2) %in% c(terms, sv))
    }else{
      tt2 <- 1:length(bdiff.bs2)
    }
    H.bs2 <- wald.test(b = bdiff.bs2, Sigma = V.bs2,
                       Terms = tt2)
  }else{H.bs2 <- NULL}

  if(!type %in% c("bs1", "bs2")){
    if(!is.null(terms)){
      tt3 <- which(names(bdiff.bs3) %in% terms)
    }else{
      tt3 <- 1:length(bdiff.bs3)
    }
    H.bs3 <- wald.test(b = bdiff.bs3, Sigma = V.bs3,
                       Terms = tt3)
  }else{H.bs3 <- NULL}



  ### Gen output

  result <- list(wald_feis    = H.bs1,
                 wald_fe      = H.bs2,
                 wald_re      = H.bs3,
                 vcov.b1      = V.bs1,
                 vcov.b2      = V.bs2,
                 vcov.b3      = V.bs3,
                 bscoef.feis  = mat.coef.feis,
                 bscoef.fe    = mat.coef.fe,
                 bscoef.re    = mat.coef.re)
  result$call    <- cl
  result$formula <- formula
  result$type    <- type
  result$samples <- sample
  result$seed    <- seed
  result$terms   <- terms

  class(result) <- c("bsfeistest")


  return(result)
}





