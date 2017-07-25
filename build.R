library(devtools)
library(data.table)


devtools::document()

zTbl = data.table(rID=1:10000)
zTbl
zTbl[, iDateTime := as.POSIXct('2017-01-01T00:00:01', format="%Y-%m-%dT%H:%M:%OS", tz='GMT')]
dtTbl = zTbl[, IDateTime(iDateTime)]
zTbl[, c('idate', 'itime') := dtTbl]
zTbl[, sID := ceiling(rID / 3)]
zTbl[, nS := 1:.N, sID]
zTbl[, NS := .N, sID]
zTbl[, uuid := sID + 30]
zTbl[, rID := NULL]
zTbl[, label := sample(c('a', 'b', 'c'), NS, replace=T), sID]



