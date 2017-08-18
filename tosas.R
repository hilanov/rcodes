setwd("~/R/Using_R_and_RStudio_for_DM_SA")

# install.packages("readxl")
library(readxl)

# library(gdata)

# ds = read.xls("http://www.amherst.edu/~nhorton/r2/datasets/help.xlsx",
#              sheet=1)

# ds = read_xlsx("http://www.amherst.edu/~nhorton/r2/datasets/help.xlsx",
#             sheet=1)


ds <- read_xlsx("C:/Users/T.Hirano/Documents/R/Using_R_and_RStudio_for_DM_SA/help.xlsx",
              sheet=1)

tosas <- data.frame(ds)
library(foreign)
write.dbf(tosas,"C:/Users/T.Hirano/Documents/R/Using_R_and_RStudio_for_DM_SA/tosas.dbf")
ds2 <- read.dbf("C:/Users/T.Hirano/Documents/R/Using_R_and_RStudio_for_DM_SA/to_r.dbf")

# install.packages("sas7bdat")
library(sas7bdat)
