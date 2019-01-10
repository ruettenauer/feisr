##################
#### MWP Data ####
##################

#' Panel data including wages and family status
#'
#' A random sample from the National Longitudinal Survey of Youth
#' \insertCite{NLSY79.2012}{feisr}. It contains information on wages,
#' family status, and work experience for a random sample of men.
#' For a description of the original dataset and variable construction
#' see \insertCite{Ludwig.2018.0;textual}{feisr}.
#'
#' @format A data frame with 3100 observations and 17 variables:
#' \describe{
#'  \item{id}{unique person identifier}
#'  \item{year}{survey year}
#'  \item{lnw}{natural log of hourly wage rate}
#'  \item{exp}{work experience in current job, in years}
#'  \item{expq}{work experience in current job squared}
#'  \item{marry}{family status (=0 if not married, =1 if married)}
#'  \item{evermarry}{indicator if ever married (=0 if never married, =1 if married at some point)}
#'  \item{enrol}{current erolment in education (=0 not enroled, =1 enroled)}
#'  \item{yeduc}{years of formal education}
#'  \item{age}{respondents current age}
#'  \item{cohort}{respondents birth cohort}
#'  \item{yeargr}{grouped year (1=1979-1980, 2=1981-1985, 3=1986-1990, 4=1991-1995, 5=1996-2000)}
#'  \item{yeargr1}{dummy indicating grouped year=1}
#'  \item{yeargr2}{dummy indicating grouped year=2}
#'  \item{yeargr3}{dummy indicating grouped year=3}
#'  \item{yeargr4}{dummy indicating grouped year=4}
#'  \item{yeargr5}{dummy indicating grouped year=5}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @source \insertCite{Ludwig.2018.0;textual}{feisr}
"mwp"
