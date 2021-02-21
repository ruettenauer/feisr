#######################
#### Function FEIS ####
#######################
#' @importFrom Rdpack reprompt
#' @importFrom utils packageVersion
#' @importFrom stats as.formula ave coef coefficients lm model.matrix model.response model.weights printCoefmat pt resid sd terms update var

#' @title Fixed Effects Individual Slope Estimator
#'
#' @description
#' Estimates fixed effects individual slope estimators by applying linear \code{lm} models
#' to "detrended" data.
#'
#' @details
#' \code{feis} is a special function to estimate linear fixed effects models with individual-specific slopes.
#' In contrast to conventional fixed effects models, data are not person "demeaned", but "detrended" by
#' the predicted individual slope of each person
#' \insertCite{Bruderl.2015.387,Ruttenauer.2020,Wooldridge.2010.384}{feisr}.
#'
#' Estimation requires at least \code{q+1} observations per unit, where \code{q} is the number of slope
#' parameters (including a constant).
#' \code{feis} automatically selects only those groups from the current data set which have at least \code{q+1} observations.
#' The function returns a warning if units with \code{<q+1} observations are dropped.
#'
#' The function requires a two-part formula, in which the second part indicates the slope parameter(s).
#' If, for example, the model is \code{y ~ x1 + x2}, with the slope variables \code{x3} and \code{x4},
#' the model can be estimated with:
#' \itemize{
#'   \item \code{formula = y ~ x1 + x2 | x3 + x4}
#' }
#'
#' To estimate a conventional fixed effects model without individual slopes, please use
#' \code{y ~ x1 + x2 | 1} to indicate that the slopes should only contain an individual-specific intercept.
#'
#' If specified, \code{feis} estimates panel-robust standard errors. Panel-robust standard errors are
#' robust to arbitrary forms of serial correlation within groups formed by \code{id} as well as
#' heteroscedasticity across groups \insertCite{@see @Wooldridge.2010.384, pp. 379-381}{feisr}.
#'
#' The model output can be exported using the \code{\link[texreg]{texreg}} package.
#'
#' @seealso \code{\link[feisr]{summary.feis}}, \code{\link[plm]{plm}}, \code{\link[plm]{pvcm}},
#' \code{\link[plm]{pmg}}, \code{\link[feisr]{feistest}}, \code{\link[fixest]{feols}}
#'
#' @param formula	a symbolic description for the model to be fitted (see Details).
#' @param object,x,model	an object of class "\code{feis}".
#' @param data a \code{data.frame} containing the specified variables.
#' @param id the name of a unique group / person identifier (as string).
#' @param weights an optional vector of weights to be used in the fitting process. See \code{\link[stats]{lm}}.
#' @param robust logical. If \code{TRUE} estimates cluster robust standard errors (default is \code{FALSE}).
#' @param intercept logical. If \code{TRUE} estimates the model with an intercept (default is \code{FALSE}).
#' @param dropgroups logical. If \code{TRUE} groups without any within variance on a slope variable are dropped
#'  , if \code{FALSE} those variables are omitted for the respective groups only (default is \code{FALSE}).
#' @param tol	the tolerance for detecting linear dependencies in the residual maker transformation
#' (see \code{\link[base]{solve}}). The argument is forwarded to \code{\link[feisr]{bsfeistest}}.
#' @param lhs,rhs indexes of the left- and right-hand side for the methods formula and terms.
#' @param ...	further arguments.
#'
#' @return An object of class "\code{feis}", containing the following elements:
#' \item{coefficients}{the vector of coefficients.}
#' \item{vcov}{the scaled (if specified, robust) variance-covariance matrix of the coefficients.
#'   See \code{\link[feisr]{vcov.feis}} for unscaled vcov}.
#' \item{residuals}{the vector of residuals (computed from the "detrended" data).}
#' \item{df.residual}{degrees of freedom of the residuals.}
#' \item{formula}{an object of class "\code{Formula}" describing the model.}
#' \item{model}{the original model frame as a \code{data.frame} containing the original
#'   variables used for estimation.}
#' \item{modelhat}{a constructed model frame as a \code{data.frame} containing the predicted
#'   values from the first stage regression using the slope variable(s) as predictor(s).}
#' \item{modeltrans}{a constructed model frame as a \code{data.frame} containing the "detrended"
#'   variables used for the final model estimation.
#'   Note that the weights are already used for detrending if specified.}
#' \item{response}{the vector of the "detrended" response variable.}
#' \item{fitted.values}{the vector of fitted values (computed from the "detrended" data).}
#' \item{id}{a vector containing the unique person identifier.}
#' \item{weights}{a vector containing weights used in fitting, or integer 1 if not speficied in call.}
#' \item{call}{the matched call.}
#' \item{assign}{assign attributes of the formula.}
#' \item{na.omit}{(where relevant) a vector of the omitted observations. The only handling method
#'  of \code{NA}s is "\code{omit}".}
#' \item{contrasts}{(only where relevant) the contrasts used.}
#' \item{arg}{a list containing the used methods. Only "\code{feis}" and "\code{individual}" effects available.}
#' \item{slopevars}{a character vector containing the names of the slope variables.}
#' \item{r2}{R squared of the "detrended" model.}
#' \item{adj.r2}{adjusted R squared of the "detrended" model.}
#' \item{vcov_arg}{a character containing the method used to compute the variance-covariance matrix.}
#' \item{tol}{the tolerance parameter (for use in bsfeistest).}
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' data("mwp", package = "feisr")
#' feis.mod <- feis(lnw ~ marry + enrol + as.factor(yeargr) | exp + I(exp^2),
#'                  data = mwp, id = "id", robust = TRUE)
#' summary(feis.mod)
#'
#' @export
#'
feis <- function(formula, data, id, weights = NULL, robust = FALSE, intercept = FALSE,
                 dropgroups = FALSE, tol = .Machine$double.eps, ...){

  if(!is.character(formula)){
    formula <- Formula::Formula(formula)
  }

  formula <- Formula::as.Formula(formula, update = TRUE)

  dots <- list(...)

  # Save row names
  orig_rownames <- row.names(data)

  # Extract id
  if(!id %in% colnames(data)){
    stop(paste0("ID variable not found in data."))
  }
  i <- data[, which(colnames(data) == id)]

  # eval the model.frame
  if (length(formula)[2] == 2){
    formula  <-  expand.formula(formula)
  }else{
    stop(paste("No individual slopes specified.
               For conventional FE please use 'y ~ x | 1' explicitly."))
  }
  if (! plm::has.intercept(formula)[2]){
    stop(paste("Individual slopes have to be estimated with intercept"))
  }



  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "weights", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels  <-  TRUE
  mf[[1]] <- as.name("model.frame")
  mf$formula <- formula
  new_rownames <- 1:nrow(data)
  row.names(data) <- new_rownames
  mf$data <- data
  mf$weights <- weights

  # Eval
  data <- eval(mf, parent.frame())

  # Subset id
  i <- i[as.numeric(row.names(data))]

  # Subset to obs with N > n_slopes+1
  if(length(formula(formula, rhs = 2, lhs = 0)) == 2 &&
     as.character(formula(formula, rhs = 2, lhs = 0))[2] == "1"){
    ns <- 0
  }else{
    ns <- ncol(attr(terms(formula(formula, rhs = 2, lhs = 0)), "factors"))
  }
  pcount <- ave(c(1:length(i)), i, FUN = function(x) length(x))

  if(any(pcount <= (ns + 1))){
    warning(paste("FEIS needs at least n(slopes)+1 observations per group. \n",
                  "You specified", ns, "slope parameter(s) plus intercept,",
                  "all groups with t <=", ns+1, "dropped", sep=" "), call. = TRUE, immediate. = TRUE)

    # reduce sample
    data <- data[which(pcount > (ns + 1)), ]
    i <- i[which(pcount > (ns + 1))]


  }


  # Check for collinearity in slopes and within variance in slopes (to avoid computationally singular)
  if(ns != 0){
    X1_test <- model.matrix(formula, data, rhs = 2, lhs = 0, cstcovar.rm = "all")
    X1_test_dm <- X1_test[, -1, drop = FALSE] - apply(X1_test[, -1, drop = FALSE], 2,
                                                      FUN = function(u) ave(u, i, FUN = function(z) mean(z)))


    if(qr(X1_test_dm, tol = tol)$rank < ncol(X1_test_dm)){
      stop(paste("Perfect collinearity in slope variables. See 'tol' option"))
    }

    # Check within variance
    wvar <- apply(X1_test[, -1, drop = FALSE], 2, FUN = function(u) ave(u, i, FUN = function(z) sd(z)))
    novar <- apply(wvar, 1, FUN = function(u) any(u == 0))

    if(any(novar) & dropgroups == TRUE){
      nom <- length(unique(i[which(novar)]))
      warning(paste(nom, "groups without any within variance on slope variable(s) dropped"),
              call. = TRUE, immediate. = TRUE)

      # Reduce sample
      data <- data[-which(novar), ]
      i <- i[-which(novar)]

    }
  }


  # Save omitted rows
  omitted <- new_rownames[-as.numeric(row.names(data))]
  names(omitted) <- orig_rownames[-as.numeric(row.names(data))]
  attr(omitted, "class") <- "omit"

  # Preserve original row.names
  row.names(data)  <-  orig_rownames[as.numeric(row.names(data))]

  # Names
  #sv <- attr(terms(formula(formula, rhs = 2, lhs = 0)), "term.labels")
  cv <- attr(terms(formula(formula, rhs = 1, lhs = 0)), "term.labels")
  rv <- all.vars(formula(formula, rhs = 0, lhs = 1))

  ### Apply weights
  w <- model.weights(data)
  if (!is.null(w)) {
    if (!is.numeric(w)) {
      stop("'weights' must be a numeric vector")
    }
    isw <- TRUE
  }
  else {
    w <- 1
    isw <- FALSE
  }

  ### First level (individual slope) regression
  X1 <- model.matrix(formula, data, rhs = 2, lhs = 0, cstcovar.rm = "all")
  sv <- colnames(X1)[-1]

  Y1 <- as.matrix(model.response(data, "numeric"))
  colnames(Y1) <- all.vars(formula(formula, rhs = 0, lhs = 1))
  #colnames(Y1) <- rownames(attr(terms(formula), "factors"))[attr(terms(formula), "response")]
  Y1 <- cbind(Y1, model.matrix(formula, data, rhs = 1, lhs = 0, cstcovar.rm = "all")[, -1, drop = FALSE])

  ny <- ncol(Y1)
  nx <- ncol(X1)

  df_step1 <- cbind(X1, Y1, w)

  # # Set up multisession
  # if(parallel == TRUE){
  #   suppressWarnings({pc1 <- require("future"); pc2 <- require("future.apply")})
  #   if(!pc1 | !pc2){
  #     stop(paste("Parallel computing requires packages future and future.apply."))
  #   }
  #   if(is.null(workers)){
  #     workers <- future::availableCores(logical = FALSE)/2
  #   }
  #   future::plan(multisession, workers = workers)
  # }
  # if(parallel == TRUE){
  #   dhat <- future.apply::future_by(df_step1, i, FUN = function(u)
  #     data.frame(hatm(y = u[, (nx + 1):(nx + ny), drop = FALSE],
  #                     x = u[, 1:nx, drop = FALSE],
  #                     weights = u[, (nx + ny + 1)],
  #                     checkcol = !dropgroups, tol = tol, isw = isw)),
  #     simplify = FALSE)
  #
  # }

  # Make hat matrix
  dhat <- by(df_step1, i, FUN = function(u)
    data.frame(hatm(y = u[, (nx + 1):(nx + ny), drop = FALSE],
                    x = u[, 1:nx, drop = FALSE],
                    weights = u[, (nx + ny + 1)],
                    checkcol = !dropgroups, tol = tol, isw = isw)),
    simplify = FALSE)


  if(utils::packageVersion("dplyr") >= "1.0.0"){
    dhat <- dplyr::bind_rows(rbind(dhat), .id = NULL) # only for version dplyr >= 1.0.0 keeps rownames
  }else{
    dhat <- do.call(rbind, lapply(dhat, as.matrix)) # use dplyr for more efficiency
  }

  # Keep orig col names
  colnames(dhat) <- colnames(df_step1)[(nx + 1):(nx + ny)]

  # Ensure original order
  dhat <- as.matrix(dhat[match(rownames(data), rownames(dhat)), ])


  ### De-trend Data

  X <- model.matrix(formula, data, rhs = 1, lhs = 0, cstcovar.rm = "all")
  ass_X <- attr(X, "assign")
  cont_X <- attr(X, "contrasts")

  # # Test for computationally singular (including slope vars) # Instead use dhat == x later
  # novar <- nowithinvar(X1, X, i)
  # drop <- colnames(X)[novar]
  #
  # if(any(novar[which(names(novar) != "(Intercept)")])){
  #   drop <- drop[which(drop != "(Intercept)")]
  #   X <- X[, -which(colnames(X) %in% drop)]
  #   cv <- cv[-which(cv %in% drop)]
  #   warning(paste0("Dropped the following variables because of no variance within slope(s): ",
  #                  paste(drop, collapse = ", ")),
  #           call. = TRUE, immediate. = TRUE)
  # }

  # Omit intercept
  if(plm::has.intercept(formula)[1] & intercept == FALSE){
    X <- X[, -1, drop = FALSE]
    ass_X <- ass_X[-1]
  }


  # Update covariates
  if(length(cont_X) != 0){
    cv <- colnames(X)
    if(intercept == TRUE){
      cv <- cv[-which(cv == "(Intercept)")]
    }
  }

  # Subtract dhat
  o1 <- match(cv, colnames(X))
  o2 <- match(cv, colnames(dhat))
  X[, o1] <- X[, o1] - dhat[, o2]

  # Test within slope variance in X
  zero <- rep(0, nrow(X))
  novar <- sapply(1:ncol(X), function(z) all.equal(X[, z], zero, tol = 1e-12,
                                                   check.attributes = FALSE, check.names = FALSE))
  if(any(novar == TRUE)){
    drop <- which(novar == TRUE)
    X <- X[, - drop]
    warning(paste0("Dropped the following variables because of no variance within slope(s): ",
                   paste(cv[drop], collapse = ", ")),
            call. = TRUE, immediate. = TRUE)
    cv <- cv[-(drop - ifelse(intercept, 1, 0))]
    ass_X <- ass_X[-drop]
  }


  Y <- as.matrix(model.response(data, "numeric"))
  Y <- as.vector(Y - dhat[, which(colnames(dhat) == rv)])


  # Store transformed data
  if(intercept){
    transformed <- data.frame(Y, X[, -1, drop = TRUE])
  }else{
    transformed <- data.frame(Y, X)
  }

  # colnames(transformed)[1] <- rv
  colnames(transformed) <- c(rv, cv)


  ### Coefficents

  # Run lm model
  # #beta <- solve(t(mx)%*%mx)%*%t(mx)%*%my
  # if(intercept == TRUE){
  #   f <- paste(rv, "~", 1, "+ .", sep=" ")
  # }else{
  #   f <- paste(rv, "~", -1, "+ .", sep=" ")
  # }
  #
  # result  <-  lm(f, data = data.frame(transformed))

  if(!isw){
    result <- stats::lm.fit(X, Y, ...)
    oonz <- 1:nrow(X)
  }else{

    # Account for zero weights
    zerow <- which(w == 0)
    if(length(zerow) > 0){
      warning(paste(length(zerow), "observations with zero weights dropped"),
              call. = TRUE, immediate. = TRUE)

      oonz <- which(w != 0)
    }else{
      oonz <- 1:nrow(X)
    }

    result <- stats::lm.wfit(X, Y, w = w, ...)
    # result <- stats::lm.wfit(X[oonz, ], Y[oonz], w = w[oonz, ], ...)
  }

  # Check rank
  r <- result$qr$rank
  p <- result$qr$pivot[1:r]
  if(r < ncol(X)){
    dv <- colnames(X)[-p]
    warning(paste("Variable(s) dropped because of collinearity:", paste(dv, collapse = ", ")),
            call. = TRUE, immediate. = TRUE)
  }

  # Exract coefs
  beta <- result$coefficients[p]
  # aliased <- result$aliased

  # Extract residuals
  u <- resid(result)
  k <- length(unique(i[oonz])) * ncol(X1) + r
  df <- length(u[oonz]) - k

  # Extract Rsquared
  r.squared <- r.sq.feis(result, adj = FALSE, intercept = intercept)
  adj.r.squared <- r.sq.feis(result, adj = TRUE, intercept = intercept)
  # One could also pass the overall dfs, but: tss and mss are net of FE and slopes,
  # do we need need FEs and slopes in df correction for within R2 then?
  # adj.r.squared <- r.sq.feis(result, adj = TRUE, df = df, intercept = intercept)

  # Fitted values (similar fitted values as plm for FE)
  fitted <- result$fitted.values

  names(fitted) <- rownames(X)
  names(Y) <- rownames(X)
  names(u) <- rownames(X)

  ### Standard errors
  uw <- u[oonz]
  if(isw){
    ww <- w[oonz]
  }else{
    ww <- w
  }


  ### Standard errors
  Xn <- result$qr$qr[oonz, 1:r, drop = FALSE]
  R <- chol2inv(Xn)


  if(!robust){
    sigma <- sum(ww * uw^2, na.rm = TRUE) / (df)
    vcov <- sigma * chol2inv(Xn)
  }

  # Cluster robust SEs
  if(robust){
    mxu <- X[oonz, p, drop = FALSE] * uw * ww
    e <- rowsum(mxu, i[oonz])
    dfc <- ((length(unique(i[oonz])) / (length(unique(i[oonz])) - 1))
            * ((length(i[oonz]) - 1) / (length(i[oonz]) - (ncol(X1) + ncol(Xn)))))
    vcovCL <- dfc * R %*% crossprod(e) %*% R

    vcov <- vcovCL
  }
  colnames(vcov) <- rownames(vcov) <- names(beta)


  ### Output

  result <- list(coefficients = beta,
               vcov          = vcov,
               residuals     = u,
               df.residual   = df,
               formula       = formula,
               model         = data,
               modelhat      = dhat,
               modeltrans    = transformed,
               response      = Y,
               fitted.values = fitted,
               id            = i,
               weights       = w)
  result$call <- cl
  result$assign <- ass_X
  result$na.action <- omitted
  result$contrasts <- cont_X
  result$arg <- list(model = "feis", effect = "individual")
  # result$aliased <- aliased
  result$slopevars <- sv
  result$r2 <- r.squared
  result$adj.r2 <- adj.r.squared

  if(robust){
    result$vcov_arg <- "Cluster robust standard errors"
  }else{result$vcov_arg <- "Normal standard errors"}

  result$tol <- tol

  class(result)  <-  c("feis")

  return(result)
}




#########################
#### Function slopes ####
#########################

# Slope maker
slpmk <- function(Y=NA, X=NA, Z=NA, beta=NA, checkcol = TRUE){

  Y <- as.matrix(Y)
  X <- as.matrix(X)
  Z <- as.matrix(Z)
  beta <- as.vector(beta)

  # Fill zero in case of collinearity
  res <- as.vector(rep(0, ncol(Z)))
  names(res) <- colnames(Z)

  # Check for perfect collinearity within groups
  if(checkcol == TRUE){
    z.qr <- qr(Z)
    if(z.qr$rank < ncol(Z)){
      vars <- z.qr$pivot[1:z.qr$rank]
      Z <- Z[, vars]
    }else{
      vars <- 1:ncol(Z)
    }
  }


  res[vars] <- as.vector(solve(crossprod(Z), crossprod(Z, (Y - X %*% beta))))
  return(t(res))
}


# Extract slopes

#' @title Extract individual slopes
#'
#' @description
#' Extracts the individual slopes (\code{alpha_i}) from a \code{feis} object created by
#' \code{\link[feisr]{feis}}.
#'
#' @details
#' The function extracts a matrix containing the individual slope parameters (\code{alpha_i}),
#' which equals the coefficient(s) of regressing the depenent variable on the slope parameter(s).
#'
#' If slope variables are perfectly collinear within a cluster, one variable is dropped
#' and the function returns \code{0} for the respective slope and cluster.
#'
#'
#' @param model	an object of class "\code{feis}".
#' @param ...	further arguments.
#'
#' @return An \code{N x J} matrix containing the individual slopes for each cluster unit \code{N}
#' and slope variable \code{J}. Rownames indicate the cluster id.
#'
#' @examples
#' data("Produc", package = "plm")
#' feis.mod <- feis("log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp | year",
#'                  data = Produc, id = "state", robust = TRUE)
#' slps <- slopes(feis.mod)
#'
#' @export
#'
slopes <- function(model=NA, ...){

  data <- model$model
  fm <- model$formula
  coefs <- model$coefficients
  names(coefs) <- dimnames(model$vcov)[[1]]

  Z <- model.matrix(fm, data, rhs = 2, lhs = 0, cstcovar.rm = "all")
  Y <- model.response(data)
  X <- model.matrix(fm, data, rhs = 1, lhs = 0, cstcovar.rm = "all")

  # Omit intercept
  if(plm::has.intercept(fm)[1]){
    X <- X[, -1, drop = FALSE]
  }

  # Check for and drop NA coef columns
  if(any(is.na(coefs))){
    drop <- which(is.na(coefs))

    X <- X[, -drop, drop = FALSE]
    coefs <- coefs[-drop, drop = FALSE]
  }

  i <- model$id

  # Test order
  oo <- match(colnames(X), names(coefs))
  coefs <- as.vector(coefs[oo])

  # Combine to df
  df <- cbind(Y, X, Z)

  nx <- ncol(X)
  nz <- ncol(Z)

  slps <- by(df, i, FUN = function(u) slpmk(Y = u[, 1], X = u[, 2:(nx + 1)],
                                          Z = u[, (nx + 2):(nx + 1 + nz)], beta = coefs,
                                          checkcol = TRUE))

  nslps <- names(slps)
  slps <- do.call(rbind, lapply(slps, as.matrix))
  rownames(slps) <- nslps

  return(slps)

}








