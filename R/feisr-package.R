#' Estimating Fixed Effects Individual Slope Models
#'
#' \pkg{feisr} provides the function `feis()` to estimate fixed effects individual
#' slopes (FEIS) models. The FEIS model constitutes a more general version of
#' the often-used fixed effects (FE) panel model, as implemented in the
#' package \code{\link[plm]{plm}} . In FEIS models, data are not only person
#' demeaned like in conventional FE models, but detrended by the predicted
#' individual slope of each person or group. Estimation is performed by
#' applying least squares `lm()` to the transformed data.
#' To test consistency of conventional FE
#' and random effects estimators against heterogeneous slopes, the package
#' also provides the functions `feistest()` for an artificial regression test
#' and `bsfeistest()` for a bootstrapped version of the Hausman test.
#'
#' @author Tobias Ruettenauer
#' @author Volker Ludwig
#'
#' @seealso \code{\link[plm]{plm}}, \code{\link[plm]{pvcm}}, \code{\link[plm]{pmg}}
#'
#' @references Ruettenauer T, Ludwig V (2020).
#'   Fixed Effects Individual Slopes: Accounting and Testing for Heterogeneous
#'   Effects in Panel Data or Other Multilevel Models.
#'   Sociological Methods and Research. Forthcoming.
#'   \url{https://www.doi.org/10.1177/0049124120926211}.
#'
#' @docType package
#' @name feisr-package
NULL
