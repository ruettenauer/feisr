######################################################################
#### Test feis / feistest creis / manual creis against each other ####
######################################################################
library(plm)
context("Compare CRE + CREIS results with feis")


### Produc

# Estimation feis + ht
data("Produc", package = "plm")

feis1.mod <- feis(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp | year,
                 data = Produc, id="state", robust = F)

ht1 <- feistest(feis1.mod, robust = F, type = "both")

ht1.cre <- ht1$CRE
ht1.creis <- ht1$CREIS

# Test for sufficient coefficients
coefs <- feisr:::cleani(names(feis1.mod$coefficients))
coefs1 <- c(coefs, paste(coefs, "mean", sep = "_"), feis1.mod$slopevars) # year_mean constant (dropped)
coefs2 <- c(coefs, paste(coefs, "hat", sep = "_"), paste(coefs, "mean", sep = "_"),
            feis1.mod$slopevars) # year_mean constant (dropped)

test_that("CRE + CREIS specification (Produc)", {
  expect_identical(names(ht1.cre$coefficients)[-1], coefs1)
  expect_identical(names(ht1.creis$coefficients), coefs2)
})


# Estimation within model with plm
plm1.mod <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp + as.numeric(year),
               data = Produc, index = c("state", "year"),
               effect = "individual", model = "within")


# Estimation of CREIS with plm

Produc$log_pcap<-log(Produc$pcap)
Produc$log_pc<-log(Produc$pc)
Produc$log_emp<-log(Produc$emp)

dhat<-by(cbind(Produc$log_pcap,
               Produc$log_pc,
               Produc$log_emp,
               Produc$unemp,
               rep(1,nrow(Produc)), Produc$year),
         Produc$state,
         FUN= function(u) hatm(y = u[, 1:4],x = u[,5:6]))
dhat<-do.call(rbind, lapply(dhat, as.matrix))

Produc2<-cbind(Produc, dhat)

plm.creis1.mod<-plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp
               + V1 + V2 + V3 + V4,
               data = Produc2,
               index = c("state", "year"),
               model = "random", effect = "individual", random.method = "walhus" )


# Compare results

test_that("CRE vs. plm within (Produc)", {
  expect_equal(unname(ht1.cre$coefficients)[2:5], unname(plm1.mod$coefficients)[1:4], tolerance = .000001)
})

test_that("CREIS ht vs. plm CREIS", {
  expect_equal(unname(ht1.creis$coefficients)[1:4], unname(plm.creis1.mod$coefficients[2:5]), tolerance = .000001)
})

test_that("CREIS ht vs. feis", {
  expect_equal(unname(ht1.creis$coefficients)[1:4], unname(feis1.mod$coefficients), tolerance = .000001)
  expect_equal(unname(plm.creis1.mod$coefficients[2:5]), unname(feis1.mod$coefficients), tolerance = .000001)

})




### Hedonic

# Estimation feis + ht

data("Hedonic", package = "plm")
# Fake year
Hedonic$fyear <- ave(Hedonic$townid, Hedonic$townid, FUN = function(x) 1:length(x))
Hedonic$pys <- ave(Hedonic$mv, Hedonic$townid, FUN = function(x) length(x[!is.na(x)]))

# Warning for goups with n <= n(slopes)+1
expect_warning(feis2.mod <- feis(mv ~ crim + chas + nox + rm + age + dis + blacks | lstat,
                                   data = Hedonic, id = "townid", robust = F))

ht2 <- feistest(feis2.mod, robust = F, type = "both")

ht2.cre <- ht2$CRE
ht2.creis <- ht2$CREIS

# Test for sufficient coefficients
coefs <- cleani(names(feis2.mod$coefficients))
coefs1 <- c(coefs, paste(coefs, "mean", sep = "_"), feis2.mod$slopevars, paste(feis2.mod$slopevars, "mean", sep = "_"))
coefs2 <- c(coefs, paste(coefs, "hat", sep = "_"), paste(coefs, "mean", sep = "_"),
            feis2.mod$slopevars, paste(feis2.mod$slopevars, "mean", sep = "_"))

test_that("CRE + CREIS specification (Hedonic)", {
  expect_identical(names(ht2.cre$coefficients)[-1], coefs1)
  expect_identical(names(ht2.creis$coefficients), coefs2)
})



# Estimation within model with plm
plm2.mod <- plm(mv ~ crim + chas + nox + rm + age + dis + blacks + lstat,
                data = Hedonic[Hedonic$pys > 2, ], index = c("townid", "fyear"),
                effect = "individual", model = "within")


# Estimation of CREIS with plm
dhat<-by(cbind(Hedonic$crim,
               Hedonic$chas,
               Hedonic$nox,
               Hedonic$rm,
               Hedonic$age,
               Hedonic$dis,
               Hedonic$blacks,
               rep(1,nrow(Hedonic)), Hedonic$lstat),
         Hedonic$townid,
         FUN= function(u) hatm(y = u[, 1:7],x = u[, 8:9]))
dhat<-do.call(rbind, lapply(dhat, as.matrix))

Hedonic2<-cbind(Hedonic, dhat)

plm.creis2.mod<-plm(mv ~ crim + chas + nox + rm + age + dis + blacks + lstat
                    + V1 + V2 + V3 + V4 + V5 + V6 + V7,
                    data = Hedonic2[Hedonic2$pys > 2, ],
                    index = c("townid", "fyear"),
                    model = "random", effect = "individual", random.method = "walhus" )


# Compare results

test_that("CRE vs. plm within (Hedonic)", {
  expect_equal(unname(ht2.cre$coefficients)[2:8], unname(plm2.mod$coefficients)[1:7], tolerance = .000001)
})

test_that("CREIS ht vs. plm CREIS", {
  expect_equal(unname(ht2.creis$coefficients)[1:7], unname(plm.creis2.mod$coefficients[2:8]), tolerance = .000001)
})

test_that("CREIS ht vs. feis", {
  expect_equal(unname(ht2.creis$coefficients)[1:7], unname(feis2.mod$coefficients), tolerance = .000001)
  expect_equal(unname(plm.creis2.mod$coefficients[2:8]), unname(feis2.mod$coefficients), tolerance = .000001)

})
