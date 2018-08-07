###############################
#### Test order / rownames ####
###############################
library(plm)
context("Test identical rownames of output data")


### Produc

# Estimation
data("Produc", package = "plm")

orignames1<-rownames(Produc)

feis1.mod <- feis(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp | year,
                  data = Produc, id = "state", robust = F)

# Estimation with re-ordered data
set.seed(1234)
Produc2 <- Produc[sample(1:nrow(Produc), nrow(Produc)),]
orignames2 <- rownames(Produc2)

feis2.mod <- feis(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp | year,
                  data = Produc2, id = "state", robust = F)




# Check identical rownames

test_that("Identical rownames residuals", {
  expect_identical(names(feis1.mod$residuals), orignames1)
  expect_identical(names(feis2.mod$residuals), orignames2)
})

test_that("Identical rownames transformed data", {
  expect_identical(rownames(feis1.mod$modeltrans), orignames1)
  expect_identical(rownames(feis2.mod$modeltrans), orignames2)
})


### Produc with NAs

Produc3 <- Produc
Produc3$gsp[sample(1:nrow(Produc), nrow(Produc)*0.1)] <- NA
Produc3$pys <- ave(Produc3$gsp, Produc3$state, FUN = function(x) length(x[!is.na(x)]))

orignames3 <- rownames(na.omit(Produc3[which(Produc3$pys>2),]))

feis3.mod <- feis(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp | year,
                  data = Produc3, id = "state", robust = F)


# Check identical rownames

test_that("Identical rownames with missings", {
  expect_identical(names(feis3.mod$residuals), orignames3)
  expect_identical(rownames(feis3.mod$modeltrans), orignames3)
  expect_identical(names(feis3.mod$na.action), orignames1[-which(orignames1 %in% orignames3)])
  expect_identical(as.vector(unname(feis3.mod$na.action)), which(is.na(Produc3$gsp) | Produc3$pys <= 2))
})


