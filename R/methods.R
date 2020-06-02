#############################
#### S3 methods for feis ####
#############################
#' @importFrom stats delete.response formula model.frame terms residuals df.residual coef vcov deviance nobs fitted predict


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
vcov.feis <- function(object,...){
  object$vcov
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
  f <- formula(object$formula, rhs = 1, lhs = 0)
  tt <- terms(f)
  if (is.null(newdata)){
    result <- fitted(object, ...)
  }
  else{
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata)
    X <- model.matrix(f, m, rhs = 1, lhs = 0, cstcovar.rm = "all")
    oo <- match(object$assign, attr(X, "assign"))
    X <- X[, oo, drop = FALSE]
    beta <- coef(object)
    result <- as.numeric(crossprod(beta, t(X)))
    names(result) <- rownames(X)
  }
  result
}

