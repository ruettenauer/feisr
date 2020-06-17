#############################
#### S3 methods for feis ####
#############################
#' @importFrom stats delete.response formula model.frame terms residuals df.residual naresid coef vcov deviance nobs fitted predict sigma hatvalues
# #' @importFrom sandwich estfun


#' @rdname feis
#' @export
formula.feis <- function(x, lhs = NULL, rhs = NULL, ...){
  formula(x$formula, lhs = lhs, rhs = rhs)
}


#' @rdname feis
#' @export
terms.feis <- function(x, lhs = NULL, rhs = NULL, ...){
  terms(formula(x, lhs = lhs, rhs = rhs))
}


#' @rdname feis
#' @export
residuals.feis <- function(object,...){
  object$residual
}


#' @rdname feis
#' @export
df.residual.feis <- function(object,...){
  object$df.residual
}


#' @rdname feis
#' @export
coef.feis <- function(object,...){
  object$coefficients
}


#' @rdname feis
#' @export
sigma.feis <- function(object,...){
  sqrt(sum(residuals(object)^2) / df.residual(object))
}


#' @title Calculate Variance-Covariance Matrix for a feis object
#'
#' @description
#' Returns the variance-covariance matrix of the main parameters of an object of class "\code{feis}".
#' By default, this is the unscaled variance-covariance matrix.
#'
#' @details
#' By default, \code{vcov()} return the unscaled variance-covariance matrix of the fitted FEIS model.
#' If set to \code{scale = TRUE}, the vcov is scaled by the nuisance parameter sigma^2
#' (as is \code{object$vcov}). Note that corrections for clustering (i.e. \code{robust = TRUE}
#' in the fitted model) are ignored in \code{vcov()}. In this case, \code{object$vcov} will return
#' the vcov with corrections for clustering.
#'
#'
#' @seealso \code{\link[feisr]{feis}}, \code{\link[stats]{vcov}}, \code{\link[stats]{sigma}}
#'
#' @param object an object of class "\code{feis}", fitted model.
#' @param scale logical. If \code{TRUE} returns scaled vcov by sigma^2 (default is \code{FALSE}).
#' @param ...	further arguments.
#'
#' @return A matrix of the estimated covariances between the parameter estimates in the fitted FEIS model.
#'
#' @examples
#' data("mwp", package = "feisr")
#' feis.mod <- feis(lnw ~ marry + enrol | exp,
#'                  data = mwp, id = "id")
#' vcov(feis.mod)
#' all.equal(vcov(feis.mod), feis.mod$vcov) # FALSE: not equal, because vcov() unscaled
#' all.equal(vcov(feis.mod, scale = TRUE), feis.mod$vcov) # equal
#'
#' @export
#'
vcov.feis <- function(object,..., scale = FALSE){
  vcov_arg <- object$vcov_arg
  sigma_sq <- sigma(object)^2
  if(vcov_arg == "Normal standard errors"){
    svcov <- object$vcov
    if(scale == FALSE){
      res <- svcov / sigma_sq
    }else if(scale == TRUE){
      res <- svcov
    }
  }
  if(vcov_arg == "Cluster robust standard errors"){
    xmat <- model.matrix(object)
    if(any(alias <- is.na(coef(object)))) xmat <- xmat[, !alias, drop = FALSE]
    if(scale == FALSE){
      res <- solve(crossprod(xmat))
    }else if(scale == TRUE){
      res <- res * sigma_sq
    }
  }
  return(res)
}


#' @rdname feis
#' @export
deviance.feis <- function(object,...){
  as.numeric(crossprod(residuals(object)))
}


#' @rdname feis
#' @export
nobs.feis <- function(object,...){
  return(length(object$residuals))
}


# Fitted values for feis
# Based on transformed data: Y_det - residuals
#' @rdname feis
#' @export
fitted.feis <- function(object,...){
 object$fitted.values
}


#' @rdname feis
#' @export
predict.feis <- function(object, newdata = NULL, ...){
  f <- formula(object, rhs = 1, lhs = 0)
  tt <- terms(f)
  if (is.null(newdata)){
    result <- fitted(object, ...)
  }
  else{
    av <- match(all.vars(tt), colnames(newdata))
    # Stop if variable in terms not included in newdata
    # TR: handle NA coefficients (see predict.lm)?
    if(any(is.na(av))){
      nf <- which(is.na(av))
      stop(paste("Variable(s)", paste(all.vars(tt)[nf], collapse = ", "),
                 "not found in newdata"))
    }
    m <- model.frame(tt, newdata)
    X <- model.matrix(f, m, rhs = 1, lhs = 0, cstcovar.rm = "all")
    oo <- match(object$assign, attr(X, "assign"))
    X <- X[, oo, drop = FALSE]
    beta <- coef(object)
    result <- as.numeric(crossprod(beta, t(X)))
    names(result) <- rownames(X)
  }
  result
}



#' @title model.matrix for feis objects
#'
#' @description
#' Methods to extract transformed model matrix for "\code{feis}" objects.
#'
#' @details
#' \code{model.matrix} for \code{feis} objects returns the model or design matrix
#' of the respective FEIS model. This is the transformed (detrended) data,
#' which is used for estimation of the model in \code{lm()}.
#'
#'
#' @seealso \code{\link[feisr]{feis}}, \code{\link[stats]{model.matrix}}
#'
#' @param object an object of class "\code{feis}".
#' @param ...	further arguments.
#'
#' @return
#' An object of class "\code{matrix}" for \code{model.matrix}.
#'
#' @examples
#' data("mwp", package = "feisr")
#' feis.mod <- feis(lnw ~ marry + as.factor(yeargr) | exp,
#'                  data = mwp, id = "id")
#' mm <- model.matrix(feis.mod)
#'
#' @export
model.matrix.feis <- function(object, ...){
  resp <- all.vars(terms(object, lhs = 1, rhs = 0))
  data <- as.matrix(object$modeltrans)
  data <- data[, which(colnames(data) != resp), drop = FALSE]
  return(data)
}



### Methods for integration with vcovHC from package sandwich


# #' @rdname feis
# #' do not export, as sandwich::vcovHC uses incorrect df
estfun.feis <- function(x, ...)
{
  xmat <- model.matrix(x)
  xmat <- naresid(x$na.action, xmat)
  if(any(alias <- is.na(coef(x)))) xmat <- xmat[, !alias, drop = FALSE]
  # wts <- weights(x)
  # if(is.null(wts)) wts <- 1
  res <- residuals(x)
  rval <- as.vector(res) * xmat
  attr(rval, "assign") <- NULL
  attr(rval, "contrasts") <- NULL
  attr(rval, "id") <- x$id
  return(rval)
}


#' @rdname feis
#' @export
hatvalues.feis <- function(model, ...)
{
  xmat <- model.matrix(model)
  xmat <- naresid(model$na.action, xmat)
  if(any(alias <- is.na(coef(model)))) xmat <- xmat[, !alias, drop = FALSE]
  qr <- qr.default(xmat)
  Q <- qr.qy(qr, diag(1, nrow = nrow(qr$qr), ncol = qr$rank))
  hat <- diag(tcrossprod(Q))
  names(hat) <- rownames(xmat)

  return(hat)
}


#' #' @rdname feis
#' #' @export
#' bread.feis <- function(x, ...){
#'   vcov_arg <- x$vcov_arg
#'   if(vcov_arg == "Normal standard errors"){
#'     res <- residuals(x)
#'     n <- nobs(x)
#'     df <- df.residual(x)
#'     sigma_sq <- sum(res^2) / df
#'     rval <- (x$vcov / sigma_sq) * n
#'   }else if(vcov_arg == "Cluster robust standard errors"){
#'     xmat <- model.matrix(x)
#'     if(any(alias <- is.na(coef(x)))) xmat <- xmat[, !alias, drop = FALSE]
#'     rval <- solve(crossprod(xmat)) * n
#'   }
#'
#'   return(rval)
#' }



