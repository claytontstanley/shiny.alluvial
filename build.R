library(devtools)
library(data.table)
library(knitr)
library(markdown)

sessionTbl = data.table(rID=1:100000)
sessionTbl[, rID := 1:.N]
sessionTbl[, iDateTime := as.POSIXct('2017-01-01T00:00:01', format="%Y-%m-%dT%H:%M:%OS", tz='GMT')]
sessionTbl[, iDateTime := iDateTime + rID ^ (1.1)]
sessionTbl
dtTbl = sessionTbl[, IDateTime(iDateTime)]
sessionTbl[, c('idate', 'itime') := dtTbl]
sessionTbl[, sID := ceiling(rID / 3)]
sessionTbl[, nS := 1:.N, sID]
sessionTbl[, NS := .N, sID]
sessionTbl[, uuid := sID + 30]
sessionTbl[, rID := NULL]
sessionTbl[, label := sample(c('a', 'b', 'c', 'd', 'e', 'f', 'g'), NS, replace=T, prob=1:7), sID]
sessionTbl


library(shiny.alluvial)
runAppSK(sessionTbl)
devtools::use_package("inline")
devtools::use_package("magrittr")

devtools::use_data(sessionTbl, overwrite=T)

devtools::load_all()

runAppSK(sessionTbl)

devtools::document()

devtools::use_readme_rmd()

knit('README.Rmd')
