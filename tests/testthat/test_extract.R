library(feisr)
library(texreg)

setMethod("extract", signature = className("feis", "feisr"),
          definition = feisr:::extract.feis)

data("mwp", package = "feisr")
feis1.mod <- feis(lnw ~ marry
                  | exp, data = mwp, id = "id")
feis2.mod <- feis(lnw ~ marry + enrol + as.factor(yeargr)
                  | exp, data = mwp, id = "id")

sr1 <- screenreg(list(feis1.mod, feis2.mod), digits = 3)
tr1 <- texreg(list(feis1.mod, feis2.mod), digits = 3)

# saveRDS(sr1, file = "./tests/files/feisr_screenreg.rds")
# saveRDS(tr1, file = "./tests/files/feisr_texreg.rds")

expect_equal(sr1,
             readRDS("../files/feisr_screenreg.RDS"))
expect_equal(tr1,
             readRDS("../files/feisr_texreg.RDS"))
