#########################################################
#### Compare Results to Stata ado xtfeis (V. Ludwig) ####
#########################################################
library(plm)
context("Compare estimation results to Stata")


### Produc

# xtfeis:
# --------------------------------------------------------------
#               est1         est2         est3         est4
# --------------------------------------------------------------
# log_pcap   -0.173323*** -0.162646*** -0.173323**  -0.162646**
#             (0.030299)    (0.033480)    (0.055937)    (0.060209)
# log_pc      0.114673***  0.102881***  0.114673**   0.102881*
#             (0.026200)    (0.027303)    (0.040617)    (0.042124)
# log_emp     0.720712***  0.759895***  0.720712***  0.759895***
#             (0.030181)    (0.034054)    (0.048101)    (0.051977)
# unemp      -0.007920***              -0.007920***
#             (0.000822)                 (0.001256)
# --------------------------------------------------------------
#   R-sq
# adj. R-sq
# AIC                .            .            .            .
# BIC                .            .            .            .
# N_clust
# N                816          816          816          816
# --------------------------------------------------------------
#   Standard errors in parentheses
# * p<0.05, ** p<0.01, *** p<0.001
coef1_1 <- c(-0.173323, 0.114673, 0.720712, -0.007920)
coef1_2 <- c(-0.162646, 0.102881, 0.759895)

se1_1 <- c(0.030299, 0.026200, 0.030181, 0.000822)
se1_2 <- c(0.033480, 0.027303, 0.034054)
se1_3 <- c(0.055937, 0.040617, 0.048101, 0.001256)
se1_4 <- c(0.060209, 0.042124, 0.051977)


# Estimation
data("Produc", package = "plm")


feis1.mod <- feis(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp | year,
                  data = Produc, id = "state", robust = F)
feis2.mod <- feis(log(gsp) ~ log(pcap) + log(pc) + log(emp)  | year + unemp,
                  data = Produc, id = "state", robust = F)
feis3.mod <- feis(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp | year,
                  data = Produc, id = "state", robust = T)
feis4.mod <- feis(log(gsp) ~ log(pcap) + log(pc) + log(emp)  | year + unemp,
                  data = Produc, id = "state", robust = T)


# Check against Stata results

test_that("Coefficients test 1 (Produc)", {
  expect_equal(unname(feis1.mod$coefficients), coef1_1, tolerance = .000001)
  expect_equal(unname(feis2.mod$coefficients), coef1_2, tolerance = .000001)
})

test_that("Standard errors test 1 (Produc)", {
  expect_equal(unname(summary(feis1.mod)$coefficients[, 2]), se1_1, tolerance = .000001)
  expect_equal(unname(summary(feis2.mod)$coefficients[, 2]), se1_2, tolerance = .000001)
})

test_that("Robust standard errors test 1 (Produc)", {
  expect_equal(unname(summary(feis3.mod)$coefficients[, 2]), se1_3, tolerance = .000001)
  expect_equal(unname(summary(feis4.mod)$coefficients[, 2]), se1_4, tolerance = .000001)
})



### Wages

# --------------------------------------------------------------
#               est1         est2         est3         est4
# --------------------------------------------------------------
# bluecol2   -0.005671    -0.022280    -0.005671    -0.022280
#             (0.016014)    (0.017456)    (0.019828)    (0.019841)
# ind         0.007234    -0.003553     0.007234    -0.003553
#             (0.017844)    (0.019381)    (0.020979)    (0.021978)
# smsa2      -0.059085*   -0.047451    -0.059085    -0.047451
#             (0.025263)    (0.027591)    (0.031789)    (0.038077)
# married2   -0.038912    -0.044336    -0.038912    -0.044336
#             (0.023418)    (0.025135)    (0.029696)    (0.033032)
# wks         0.000437                  0.000437
#             (0.000673)                 (0.001184)
# --------------------------------------------------------------
#   R-sq
# adj. R-sq
# AIC                .            .            .            .
# BIC                .            .            .            .
# N_clust
# N               3696         3696         3696         3696
# --------------------------------------------------------------
coef2_1 <- c(-0.005671, 0.007234, -0.059085, -0.038912, 0.000437)
coef2_2 <- c(-0.022280, -0.003553, -0.047451, -0.044336)

se2_1 <- c(0.016014, 0.017844, 0.025263, 0.023418, 0.000673)
se2_2 <- c(0.017456, 0.019381, 0.027591, 0.025135)
se2_3 <- c(0.019828, 0.020979, 0.031789, 0.029696, 0.001184)
se2_4 <- c(0.019841, 0.021978, 0.038077, 0.033032)


# Estimation
data("Wages", package = "plm")

Wages$id<-rep(c(1:595), each = 7)
Wages$year<-rep(c(1976:1982), times = 595)


feis2_1.mod <- feis(lwage ~  bluecol + ind + smsa +  married + wks | year,
                  data = Wages[Wages$sex == "male",], id = "id", robust = F)
feis2_2.mod <- feis(lwage ~  bluecol + ind + smsa +  married | year + wks,
                    data = Wages[Wages$sex == "male",], id = "id", robust = F)
feis2_3.mod <- feis(lwage ~  bluecol + ind + smsa +  married + wks | year,
                    data = Wages[Wages$sex == "male",], id = "id", robust = T)
feis2_4.mod <- feis(lwage ~  bluecol + ind + smsa +  married | year + wks,
                    data = Wages[Wages$sex == "male",], id = "id", robust = T)


# Check against Stata results

test_that("Coefficients test 2 (Wages)", {
  expect_equal(unname(feis2_1.mod$coefficients), coef2_1, tolerance = .000001)
  expect_equal(unname(feis2_2.mod$coefficients), coef2_2, tolerance = .000001)
})

test_that("Standard errors test 2 (Wages)", {
  expect_equal(unname(summary(feis2_1.mod)$coefficients[, 2]), se2_1, tolerance = .000001)
  expect_equal(unname(summary(feis2_2.mod)$coefficients[, 2]), se2_2, tolerance = .000001)
})

test_that("Robust standard errors test 2 (Wages)", {
  expect_equal(unname(summary(feis2_3.mod)$coefficients[, 2]), se2_3, tolerance = .000001)
  expect_equal(unname(summary(feis2_4.mod)$coefficients[, 2]), se2_4, tolerance = .000001)
})



### Hedonic

# --------------------------------------------------------------
#               est1         est2         est3         est4
# --------------------------------------------------------------
# crim       -0.004089*** -0.004035*** -0.004089**  -0.004035**
#             (0.001064)    (0.001096)    (0.001224)    (0.001433)
# chas2      -0.072240*   -0.088125*   -0.072240*   -0.088125**
#             (0.032299)    (0.035182)    (0.032497)    (0.032416)
# nox        -0.005019**  -0.005347**  -0.005019    -0.005347
#             (0.001604)    (0.001703)    (0.002620)    (0.002852)
# rm          0.009703***  0.008968***  0.009703*    0.008968*
#             (0.001250)    (0.001357)    (0.004230)    (0.004278)
# age        -0.002050*** -0.001981**  -0.002050**  -0.001981*
#             (0.000548)    (0.000690)    (0.000741)    (0.000868)
# dis        -0.065229    -0.107342    -0.065229    -0.107342
#             (0.079555)    (0.090868)    (0.105092)    (0.108309)
# blacks      0.550558***               0.550558***
#             (0.105897)                 (0.136688)
# --------------------------------------------------------------
#   R-sq
# adj. R-sq
# AIC                .            .            .            .
# BIC                .            .            .            .
# N_clust
# N                459          429          459          429
# --------------------------------------------------------------
coef3_1 <- c(-0.004089, -0.072240, -0.005019, 0.009703, -0.002050, -0.065229, 0.550558)
coef3_2 <- c(-0.004035, -0.088125, -0.005347, 0.008968, -0.001981, -0.107342)

se3_1 <- c(0.001064, 0.032299, 0.001604, 0.001250, 0.000548, 0.079555, 0.105897)
se3_2 <- c(0.001096, 0.035182, 0.001703, 0.001357, 0.000690, 0.090868)
se3_3 <- c(0.001224, 0.032497, 0.002620, 0.004230, 0.000741, 0.105092, 0.136688)
se3_4 <- c(0.001433, 0.032416, 0.002852, 0.004278, 0.000868, 0.108309)


# Estimation
data("Hedonic", package = "plm")
# Fake year
Hedonic$fyear<-ave(Hedonic$townid, Hedonic$townid, FUN = function(x) 1:length(x))

# Warning for goups with n <= n(slopes)+1
expect_warning(feis3_1.mod <- feis(mv ~ crim + chas + nox + rm + age + dis + blacks | lstat,
                  data = Hedonic, id = "townid", robust = F))
expect_warning(feis3_2.mod <- feis(mv ~ crim + chas + nox + rm + age + dis | lstat + blacks,
                    data = Hedonic, id = "townid", robust = F))
expect_warning(feis3_3.mod <- feis(mv ~ crim + chas + nox + rm + age + dis + blacks | lstat,
                    data = Hedonic, id = "townid", robust = T))
expect_warning(feis3_4.mod <- feis(mv ~ crim + chas + nox + rm + age + dis | lstat + blacks,
                    data = Hedonic, id = "townid", robust = T))

# Check against Stata results

test_that("Coefficients test 3 (Hedonic)", {
  expect_equal(unname(feis3_1.mod$coefficients), coef3_1, tolerance = .000001)
  expect_equal(unname(feis3_2.mod$coefficients), coef3_2, tolerance = .000001)
})

test_that("Standard errors test 3 (Hedonic)", {
  expect_equal(unname(summary(feis3_1.mod)$coefficients[, 2]), se3_1, tolerance = .000001)
  expect_equal(unname(summary(feis3_2.mod)$coefficients[, 2]), se3_2, tolerance = .000001)
})

test_that("Robust standard errors test 3 (Hedonic)", {
  expect_equal(unname(summary(feis3_3.mod)$coefficients[, 2]), se3_3, tolerance = .000001)
  expect_equal(unname(summary(feis3_4.mod)$coefficients[, 2]), se3_4, tolerance = .000001)
})




### Test extract method (use texreg test to check compatibility)

test_that("extract feis (for texreg)", {
  data("mwp", package = "feisr")
  feis1.mod <- feis(lnw ~ marry | exp, data = mwp, id = "id")
  feis2.mod <- feis(lnw ~ marry + enrol + as.factor(yeargr) | exp,
                    data = mwp,
                    id = "id")
  tr <- feisr:::extract.feis(feis1.mod)
  expect_equivalent(tr@coef, 0.056, tolerance = 1e-3)
  expect_equivalent(tr@se, 0.0234, tolerance = 1e-3)
  expect_equivalent(tr@pvalues, 0.0165, tolerance = 1e-3)
  expect_equivalent(tr@gof, c(0.002, 0.002, 3100, 268, 0.312), tolerance = 1e-3)
  expect_length(tr@gof.names, 5)
  tr2 <- feisr:::extract.feis(feis2.mod)
  expect_length(tr2@coef, 6)
  expect_length(which(tr2@pvalues < 0.05), 2)
  expect_length(which(tr2@gof.decimal), 3)
})

