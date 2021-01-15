
<!-- README.md is generated from README.Rmd. Please edit that file -->

# feisr

<!-- badges: start -->

[![R-CMD-check](https://github.com/ruettenauer/feisr/workflows/R-CMD-check/badge.svg)](https://github.com/ruettenauer/feisr/actions/)
[![Build
Status](https://travis-ci.com/ruettenauer/feisr.svg?branch=master)](https://travis-ci.com/ruettenauer/feisr/)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](https://www.gnu.org/licenses/gpl-2.0.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/feisr)](https://CRAN.R-project.org/package=feisr/)
[![cran
checks](https://cranchecks.info/badges/worst/feisr)](https://cran.r-project.org/web/checks/check_results_feisr.html)
<!-- badges: end -->

The packages feisr provides a function to estimate fixed effects
individual slope (FEIS) models in R. FEIS models constitute a more
general version of the often used conventional fixed effects (FE) panel
models. In contrast to conventional fixed effects models, data are not
person ‘demeaned’, but ‘detrended’ by the predicted individual slope of
each person, which relaxes the assumptions of parallel trends between
treated and untreated groups. For more information see Brüderl and
Ludwig (2015); Frees (2001); Polachek and Kim (1994); Rüttenauer and
Ludwig (2020); Wooldridge (2010).

## Installation

You can install feisr from cran with:

``` r
install.packages("feisr")
```

You can install the latest development version from github with:

``` r
# install.packages("devtools")
devtools::install_github("ruettenauer/feisr")
```

## Example

The following example investigates the ‘marriage wage premium’: we test
whether marriage leads to an increase in the hourly wage for men. The
packages provides the function feis to estimate fixed effects individual
slope models, which control for the hypothesis that those men who marry
earlier also have a steeper wage growth over time. Similar to the plm
function, feis requires to indicate a unique person / group identifier.
To include individual-specific slopes, feis uses two-part formulas (expr
| slope\_expr), where slope\_expr gives the expression for modelling the
individual slopes. In our example, we use work experience (exp) and
squared work experience as the slope variables:

``` r
library(feisr)
data("mwp", package = "feisr")
feis.mod <- feis(lnw ~ marry + enrol + yeduc + as.factor(yeargr)
                   | exp + I(exp^2), data = mwp, id = "id")
summary(feis.mod)
#> 
#> 
#> Call:
#> feis(formula = lnw ~ marry + enrol + yeduc + as.factor(yeargr) | 
#>     exp + I(exp^2), data = mwp, id = "id")
#> 
#> 
#> Residuals:
#>       Min.    1st Qu.     Median    3rd Qu.       Max. 
#> -2.0790815 -0.1050450  0.0046876  0.1112708  1.9412090 
#> 
#> Coefficients:
#>                      Estimate Std. Error t-value  Pr(>|t|)    
#> marry               0.0134582  0.0273006  0.4930    0.6221    
#> enrol              -0.1181725  0.0234275 -5.0442 4.913e-07 ***
#> yeduc              -0.0020607  0.0137673 -0.1497    0.8810    
#> as.factor(yeargr)2 -0.0464504  0.0352096 -1.3193    0.1872    
#> as.factor(yeargr)3 -0.0189333  0.0510825 -0.3706    0.7109    
#> as.factor(yeargr)4 -0.1361305  0.0616378 -2.2086    0.0273 *  
#> as.factor(yeargr)5 -0.1868589  0.0769889 -2.4271    0.0153 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Normal standard errors
#> Slope parameters:  exp, I(exp^2) 
#> Total Sum of Squares:    190.33
#> Residual Sum of Squares: 185.64
#> R-Squared:      0.024626
#> Adj. R-Squared: 0.022419
```

The package also comes with an artificial regression test, which
performs a Hausman-like test comparing FEIS against FE, FEIS against
random effects (RE), and FE against RE models. The function feistest can
be used with cluster-robust standard errors:

``` r
ht <- feistest(feis.mod, robust = TRUE, type = "all")
summary(ht)
#> 
#> 
#> Call:
#> feis(formula = lnw ~ marry + enrol + yeduc + as.factor(yeargr) | 
#>     exp + I(exp^2), data = mwp, id = "id")
#> 
#> Robust Artificial Regression Test 
#> 
#> FEIS vs. FE:
#> ------------
#> H0: FEIS and FE estimates consistent 
#> Alternative H1: FE inconsistent 
#> Model constraints: marry_hat, enrol_hat, yeduc_hat, as_factor_yeargr_2_hat, 
#> as_factor_yeargr_3_hat, as_factor_yeargr_4_hat, as_factor_yeargr_5_hat = 0 
#> 
#> Chi-squared test:
#> Chisq = 49.558, df = 7, P(> X2) = 1.7639e-08
#> 
#> 
#> FE vs. RE:
#> ------------
#> H0: FE and RE estimates consistent 
#> Alternative H1: RE inconsistent 
#> Model constraints: marry_mean, enrol_mean, yeduc_mean, as_factor_yeargr_2_mean, 
#> as_factor_yeargr_3_mean, as_factor_yeargr_4_mean, as_factor_yeargr_5_mean, 
#> exp_mean, exp_2_mean = 0 
#> 
#> Chi-squared test:
#> Chisq = 13.087, df = 9, P(> X2) = 0.15872
#> 
#> 
#> FEIS vs. RE:
#> ------------
#> H0: FEIS and RE estimates consistent 
#> Alternative H1: RE inconsistent 
#> Model constraints: marry_hat, enrol_hat, yeduc_hat, as_factor_yeargr_2_hat, 
#> as_factor_yeargr_3_hat, as_factor_yeargr_4_hat, as_factor_yeargr_5_hat = 0 
#> 
#> Chi-squared test:
#> Chisq = 55.231, df = 7, P(> X2) = 1.342e-09
```

## References

Brüderl J, Ludwig V (2015). “Fixed-Effects Panel Regression.” In H Best,
C Wolf (eds.), The Sage Handbook of Regression Analysis and Causal
Inference, pp. 327-357. Sage, Los Angeles. ISBN 1446252442.

Frees EW (2001). “Omitted Variables in Longitudinal Data Models.”
Canadian Journal of Statistics, 29(4), 573-595.
<https://doi.org/10.2307/3316008>.

Polachek SW, Kim MK (1994). “Panel Estimates of the Gender Earnings
Gap.” Journal of Econometrics, 61(1), 23-42.
<https://doi.org/10.1016/0304-4076(94)90075-2>.

Rüttenauer T, Ludwig V (2020). “Fixed Effects Individual Slopes:
Accounting and Testing for Heterogeneous Effects in Panel Data or Other
Multilevel Models.” Sociological Methods and Research, OnlineFirst.
<https://doi.org/10.1177/0049124120926211>.

Wooldridge JM (2010). “Econometric Analysis of Cross Section and Panel
Data.” MIT Press, Cambridge, Mass. ISBN 0262294354.
