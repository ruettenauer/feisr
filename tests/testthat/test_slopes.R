#####################################
#### Test feis individual slopes ####
#####################################

context("Test individual slopes estimations")

###################
#### Example 1 ####
###################

data("Produc", package = "plm")

### FEIS model
feis1.mod <- feis(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp | year,
                  data = Produc, id="state", robust=T)

summary(feis1.mod)

slps <- slopes(feis1.mod)


## Compare to Stata

test_that("Slopes test 1 (Produc)", {
  expect_equal(mean(slps[, 1]), -15.31751, tolerance = .0001)
  expect_equal(mean(slps[, 2]), .0107765, tolerance = .00001)

  expect_equal(slps[1, 1], -29.178385, tolerance = .0001)
  expect_equal(slps[1, 2], .01774419, tolerance = .00001)

  expect_equal(slps[48, 1], 1.1313534, tolerance = .0001)
  expect_equal(slps[48, 2], .00240872, tolerance = .00001)
})




###################
#### Example 2 ####
###################



# Estimation
data("Wages", package = "plm")

Wages$id<-rep(c(1:595), each = 7)
Wages$year<-rep(c(1976:1982), times = 595)


feis2_4.mod <- feis(lwage ~  bluecol + ind + smsa +  married | year + wks,
                    data = Wages[Wages$sex == "male",], id = "id", robust = T)


slps <- data.frame(slopes(feis2_4.mod))

### For weighting use number of obs per id
# slps2 <- Wages [, "id", drop=F]
# slps$id <- rownames(slps)
# slps2 <- plyr::join(slps2, slps, by="id", type="left", match="all")


### Note!!! Estimation of average partial effects needs to be checked!
#     What to do with collinear variables: which of them is set to zero?
#     Now: estimated with intercept and collinear var = zero effect

test_that("Slopes test 2 (Wages)", {
  # expect_equal(mean(slps2[, 2], na.rm=T), -181.2175, tolerance = .0001)
  # expect_equal(mean(slps2[, 3], na.rm=T), .0965158, tolerance = .00001)
  # expect_equal(mean(slps2[, 4], na.rm=T), -.05833, tolerance = .00001)

  #expect_equal(slps[1, 1], -216.69705, tolerance = .0001)
  expect_equal(slps[1, 2], .11245089, tolerance = .0001)
  expect_equal(slps[1, 3], .00749049, tolerance = .0001)

  #expect_equal(slps[rownames(slps)==594, 1], -206.97881, tolerance = .0001)
  expect_equal(slps[rownames(slps)==594, 2], .1078836, tolerance = .0001)
  expect_equal(slps[rownames(slps)==594, 3], .00863863, tolerance = .0001)

  #expect_equal(slps[rownames(slps)==423, 1], -57.550059, tolerance = .0001)
  expect_equal(slps[rownames(slps)==423, 2], .03272696, tolerance = .0001)
  expect_equal(slps[rownames(slps)==423, 3], -.00594366, tolerance = .0001)
})


