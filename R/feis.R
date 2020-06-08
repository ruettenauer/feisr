#######################
#### Function FEIS ####
#######################
#' @importFrom Rdpack reprompt
#' @importFrom utils packageVersion
#' @importFrom stats as.formula ave coef coefficients lm model.matrix model.response printCoefmat pt resid sd terms update var

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
#' If the second part is not specified (and individual "slopes" are estimated only by an intercept),
#' the model reduces to a conventional fixed effects (within) model. In this case please use
#' the well-established \code{\link[plm]{plm}} (\code{model="within"}) function instead of \code{feis}.
#'
#' If specified, \code{feis} estimates panel-robust standard errors. Panel-robust standard errors are
#' robust to arbitrary forms of serial correlation within groups formed by \code{id} as well as
#' heteroscedasticity across groups \insertCite{@see @Wooldridge.2010.384, pp. 379-381}{feisr}.
#'
#' The model output can be exported using the \code{\link[texreg]{texreg}} package.
#'
#' @seealso \code{\link[feisr]{summary.feis}}, \code{\link[plm]{plm}}, \code{\link[plm]{pvcm}},
#' \code{\link[plm]{pmg}}, \code{\link[feisr]{feistest}}
#'
#' @param formula	a symbolic description for the model to be fitted (see Details).
#' @param object,x	an object of class "\code{feis}".
#' @param data a \code{data.frame} containing the specified variables.
#' @param id the name of a unique group / person identifier (as string).
#' @param robust logical. If \code{TRUE} estimates cluster robust standard errors (default is \code{FALSE}).
#' @param intercept logical. If \code{TRUE} estimates the model with an intercept (default is \code{FALSE}).
#' @param dropgroups logical. If \code{TRUE} groups without any within variance on a slope variable are dropped
#'  , if \code{FALSE} those variables are omitted for the respective groups only (default is \code{FALSE}).
#' @param tol	the tolerance for detecting linear dependencies in slopes (see \code{\link[base]{solve}}).
#' @param newdata the new data set for the predict method.
#' @param lhs,rhs indexes of the left- and right-hand side for the methods formula and terms.
#' @param ...	further arguments.
#'
#' @return An object of class "\code{feis}", containing the following elements:
#' \item{coefficients}{the vector of coefficients.}
#' \item{vcov}{the scaled variance-covariance matrix of the coefficients.}
#' \item{residuals}{the vector of residuals (computed from the "detrended" data).}
#' \item{df.residual}{degrees of freedom of the residuals.}
#' \item{formula}{an object of class "\code{Formula}" describing the model.}
#' \item{model}{the original model frame as a \code{data.frame} containing the original
#'   variables used for estimation.}
#' \item{modelhat}{a constructed model frame as a \code{data.frame} containing the predicted
#'   values from the first stage regression using the slope variable(s) as predictor(s).}
#' \item{modeltrans}{a constructed model frame as a \code{data.frame} containing the "detrended"
#'   variables used for the final model estimation and the untransformed slope variables.}
#' \item{response}{the vector of the "detrended" response variable.}
#' \item{fitted.values}{the vector of fitted values (computed from the "detrended" data).}
#' \item{id}{a vector containing the unique person identifier.}
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
feis <- function(formula, data, id, robust = FALSE, intercept = FALSE,
                 dropgroups = FALSE, tol = .Machine$double.eps, ...){

  if(!is.character(formula)){
    formula <- Formula::Formula(formula)
  }

  formula <- Formula::as.Formula(formula, update = TRUE)

  dots <- list(...)

  # Save row names
  orig_rownames <- row.names(data)

  # Extract id
  i <- data[, which(colnames(data) == id)]

  # eval the model.frame
  if (length(formula)[2] == 2){
    formula  <-  expand.formula(formula)
  }else{
    stop(paste("No individual slopes specified. Please use plm"))
  }
  if (! plm::has.intercept(formula)[2]){
    stop(paste("Individual slopes have to be estimated with intercept"))
  }

  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels  <-  TRUE
  mf[[1]] <- as.name("model.frame")
  mf$formula <- formula
  new_rownames <- 1:nrow(data)
  row.names(data) <- new_rownames
  mf$data <- data

  # Eval
  data <- eval(mf, parent.frame())

  # Subset id
  i <- i[as.numeric(row.names(data))]

  # Subset to obs with N > n_slopes+1
  ns <- ncol(attr(terms(formula(formula, rhs = 2, lhs = 0)), "factors"))
  pcount <- ave(c(1:length(i)), i, FUN = function(x) length(x))

  if(any(pcount<=(ns+1))){
    warning(paste("FEIS needs at least n(slopes)+1 observations per group. \n",
                  "You specified", ns, "slope parameter(s) plus intercept,",
                  "all groups with t <=", ns+1, "dropped", sep=" "), call. = TRUE, immediate. = TRUE)

    # reduce sample
    data <- data[which(pcount > (ns + 1)), ]
    i <- i[which(pcount > (ns + 1))]


  }

  # Check for collinearity in slopes and within variance in slopes (to avoid computationally singular)
  X1_test <- model.matrix(formula, data, rhs = 2, lhs = 0, cstcovar.rm = "all")
  X1_test_dm <- X1_test[, -1, drop = FALSE] - apply(X1_test[, -1, drop = FALSE], 2, FUN =
                                                function(u) ave(u, i, FUN = function(z) mean(z)))

  if(qr(X1_test_dm)$rank < ncol(X1_test_dm)){
    stop(paste("Perfect collinearity in slope variables"))
  }

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


  ### First level (individual slope) regression

  X1 <- model.matrix(formula, data, rhs = 2, lhs = 0, cstcovar.rm = "all")
  sv <- colnames(X1)[-1]

  Y1 <- as.matrix(model.response(data, "numeric"))
  colnames(Y1) <- all.vars(formula(formula, rhs = 0, lhs = 1))
  #colnames(Y1) <- rownames(attr(terms(formula), "factors"))[attr(terms(formula), "response")]
  Y1 <- cbind(Y1, model.matrix(formula, data, rhs = 1, lhs = 0, cstcovar.rm = "all")[, -1, drop = FALSE])

  ny <- ncol(Y1)
  nx <- ncol(X1)

  df_step1 <- cbind(X1, Y1)

  dhat <- by(df_step1, i, FUN = function(u) data.frame(hatm(y = u[, (nx + 1):(nx + ny)], x = u[, 1:nx],
                                               checkcol = !dropgroups, tol = tol)))

  if(utils::packageVersion("dplyr") >= "1.0.0"){
    dhat <- dplyr::bind_rows(rbind(dhat), .id = NULL) # only for version dplyr >= 1.0.0 keeps rownames
  }else{
    dhat <- do.call(rbind, lapply(dhat, as.matrix)) # use dplyr for more efficiency
  }

  # rn <- unlist(lapply(dhat, FUN = function(x) rownames(x))) # Rownames preserved in dplyr 1.0.0
  # rownames(dhat) <- rn

  colnames(dhat) <- colnames(df_step1)[(nx + 1):(nx + ny)] # Keep orig col names

  # Ensure original order
  dhat <- as.matrix(dhat[match(rownames(data), rownames(dhat)), ])


  ### De-trend Data

  X <- model.matrix(formula, data, rhs = 1, lhs = 0, cstcovar.rm = "all")
  ass_X <- attr(X, "assign")
  cont_X <- attr(X, "contrasts")

  # Test for computationally singular (including slope vars)
  novar <- nowithinvar(X1, X, i)
  drop <- colnames(X)[novar]

  if(any(novar[which(names(novar) != "(Intercept)")])){
    drop <- drop[which(drop != "(Intercept)")]
    X <- X[, -which(colnames(X) %in% drop)]
  }

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

  # Substract dhat
  o1<-match(cv, colnames(X))
  o2<-match(cv, colnames(dhat))
  X[, o1] <- X[, o1] - dhat[, o2]


  Y <- as.matrix(model.response(data, "numeric"))
  Y <- as.vector(Y - dhat[, which(colnames(dhat) == rv)])


  # Store transformed data
  transformed <- data.frame(Y, X)
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

  result <- stats::lm.fit(X, Y, ...)

  # Exract coefs
  beta <- result$coefficients
  # aliased <- result$aliased

  # Extract residuals
  u <- resid(result)
  k <- length(unique(i)) * ncol(X1) + ncol(X)
  df <- length(u) - k

  # Extract Rsquared
  r.squared <- r.sq.feis(result, adj = FALSE, intercept = intercept)
  adj.r.squared <- r.sq.feis(result, adj = TRUE, intercept = intercept)

  # Fitted values (similar fitted values as plm for FE)
  fitted <- as.vector(Y - u)

  names(fitted) <- rownames(X)
  names(Y) <- rownames(X)
  names(u) <- rownames(X)

  ### Standard errors


  # Check for NAs in beta, and drop vars from for SE calculation
  if(any(is.na(beta))){
    Xn <- X[, -which(is.na(beta))]

    vcov <- matrix(NA, ncol = ncol(X), nrow = ncol(X))
    colnames(vcov) <- colnames(X)
    rownames(vcov) <- colnames(X)

    if(!robust){
      sigma <- sum((u * u)) / (df)
      tmp <- sigma * solve(crossprod(Xn))
      vcov[rownames(tmp), colnames(tmp)] <- tmp
      # se <- sqrt(diag(vcov))
    }

    # Cluster robust SEs
    if(robust){
      mxu <- Xn * u
      e <- rowsum(mxu, i)
      dfc <- ((length(unique(i)) / (length(unique(i)) - 1))
              * ((length(i) - 1) / (length(i) - (ncol(X1) + ncol(Xn)))))
      vcovCL <- dfc * (solve(crossprod(Xn)) %*% crossprod(e) %*% solve(crossprod(Xn)))

      vcov[rownames(vcovCL), colnames(vcovCL)] <- vcovCL

      # se <- sqrt(diag(vcov))
    }

  }else{
    if(!robust){
      sigma <- sum((u * u)) / (df)
      vcov <- sigma * solve(crossprod(X))
      # se <- sqrt(diag(vcov))
    }

    # Cluster robust SEs
    if(robust){
      mxu <- X * u
      e <- rowsum(mxu, i)
      dfc <- ((length(unique(i)) / (length(unique(i)) - 1))
              * ((length(i) - 1) / (length(i) - (ncol(X1) + ncol(X)))))
      vcovCL <- dfc * (solve(crossprod(X)) %*% crossprod(e) %*% solve(crossprod(X)))
      # se <- sqrt(diag(vcovCL))

      vcov <- vcovCL
    }
  }


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
               id            = i)
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








