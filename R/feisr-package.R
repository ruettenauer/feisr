#' Estimating Fixed Effects Individual Slope Models
#'
#' The main purpose of the package \code{feisr} is the estimation of fixed
#' effects individual slopes models and respective test statistics.
#' The fixed effects individual slopes (FEIS) estimator is a more general
#' version of the well-known fixed effects estimator (FE), which allows to
#' control for heterogeneous slopes in addition to time-constant heterogeneity
#' \insertCite{Bruderl.2015.387,Ruttenauer.2020,Wooldridge.2010.384}{feisr}.
#' This is done by running an \code{lm()} model on pre-transformed data, where
#' we (1) estimate the individual-specific predicted values for the dependent
#' variable and each covariate based on an individual intercept and the additional
#' slope variables, (2) detrend the original data by these individual-specific
#' predicted values, and (3) run an OLS model on the residual data. The package
#' also provides two specification test for heterogeneous slopes
#' \insertCite{@more details and examples can be found in @Ruttenauer.2020}{feisr}.
#'
#' The main functions of the \code{feisr} package are:
#'
#' - \code{feis()}: fixed effects individual slopes estimator by
#' applying \code{lm} to detrended data.
#'
#' - \code{feistest()}: regression-based Hausman test for fixed effects
#' individual slope models.
#'
#' - \code{bsfeistest()}: bootstrapped Hausman test for fixed effects
#' individual slope models.
#'
#' The functions included in the R package \code{feisr} are also available in the
#' xtfeis ado (\url{https://ideas.repec.org/c/boc/bocode/s458045.html})
#' for Stata. The \code{\link[plm]{plm-package}} provides functions
#' for estimation of related models, like the mean group (MG) or common
#' correlated effects mean groups (CCEMG) estimator via \code{\link[plm]{pmg}} or
#' models with variable coefficients via \code{\link[plm]{pvcm}}.
#'
#' @author Tobias Ruettenauer
#' @author Volker Ludwig
#'
#' @seealso \code{\link[plm]{plm}}, \code{\link[plm]{pvcm}}, \code{\link[plm]{pmg}}
#'
#' @references
#' \insertAllCited{}
#'
#'
#' _PACKAGE
#' @name feisr-package
NULL
