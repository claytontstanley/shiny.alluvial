library(data.table)
library(shiny)
library(testthat)
library(assertthat)
library(Rcpp)
library(inline)
library(stringr)
library(stringi)
library(rCharts)
library(devtools)

runAppSK <- function(zTbl, ...) {
	PATH = system.file(package="shiny.alluvial")
	.sessionTbl <<- copy(zTbl)
	addResourcePath('shiny_alluvial', .PATH) 
	shiny::runApp(PATH, host="0.0.0.0", port=3343, ...)
}

newRChart <- function(...) {
	PATH = system.file(package="shiny.alluvial")
	newPlot = rCharts$new(...)
	newPlot$setLib(PATH)
	newPlot$setTemplate(script = sprintf("%s/layouts/chart.html"))
	newPlot
}

getSKChart <- function(tbl, exactTimeP=F) {
	ixs = tbl[, .I[moving == 'backward']]
	resTbl = copy(tbl)
	resTbl[, grep('^cID', colnames(resTbl), value=T) := NULL]
	resTbl

	resTbl[ixs, `:=`(tTarget=abs(tTarget), tSource=abs(tSource), dt=abs(dt))]
	capMinTimes(resTbl)
	if (exactTimeP == F) {
		resTbl = relaxTimesToSteps(resTbl)
	}
	resTbl
	resTbl[grepl('^ *$', target), `:=`(dt=0, tTarget=tSource)]
	resTbl[ixs, `:=`(tTarget=max(tTarget) - tTarget, tSource=max(tTarget) - tSource)]
	swapVals <- function(nm1, nm2) {
		resTbl[ixs, nm.orig := get(nm1)
		       ][ixs, (nm1) := get(nm2)
		       ][ixs, (nm2) := nm.orig
		       ][, nm.orig := NULL
		       ]
	}
	swapVals('source', 'target')
	swapVals('sourceCID', 'targetCID')
	swapVals('tSource', 'tTarget')
	swapVals('countRowSource', 'countRowTarget')
	swapVals('valueRowSource', 'valueRowTarget')

	makeUniqueTS(resTbl)
	sankeyPlot = newRChart()
	sankeyPlot$set(data = resTbl,
		       nodeWidth = 25,
		       nodePadding = 30,
		       layout = 32,
		       width = 3840,
		       height = 2160,
		       labelFormat = ".1%")
	sankeyPlot
}

getSankeyTblTime <- function(zTbl, stepMax=3, groupField='', normalizeGroupsP=F) {
	getSankeyTblTimeGrp <- function(e2Tbl, stepMax, labelGroup) {
		eTbl = addSessionEnd(e2Tbl)
		if (labelGroup != '') {
			eTbl = addSessionStart(eTbl, labelGroup=labelGroup)
		}
		setorder(eTbl, sID, nS)
		eTbl[, tStart := iDateTime[1], sID]
		eTbl[, dt := as.numeric(iDateTime - tStart, units='secs')]
		eTbl[, tStart := NULL]
		skTbl = eTbl[, .(sID, nS, label, iDateTime, dt, NS)]
		addCIDs(skTbl, stepMax)
		resTbl = getSkSumTbl(skTbl)
		updateDrops(resTbl)
		resTbl[, moving := ifelse(min(tTarget) < 0, 'backward', 'forward')]
		resTbl
	}
	e2Tbl = copy(zTbl)
	if (groupField == '') {
		e2Tbl[, labelGroup := '']
	} else {
		e2Tbl[, labelGroup := get(groupField)]
	}
	resTbl = e2Tbl[, getSankeyTblTimeGrp(.SD, stepMax, labelGroup), .(labelGroup)]
	resTbl[, countGroup := sum(count[step == 2]), labelGroup]
	resTbl[order(-countGroup, labelGroup), rankGroup := rleid(labelGroup)]
	incrementCIDs <- function(val, cRank) {
		cNames = grep('cid', colnames(resTbl), value=T, ignore.case=T)
		for (cName in cNames) {
			resTbl[rankGroup == cRank, (cName) := get(cName) + val]
		}
		resTbl
	}
	if (resTbl[, max(rankGroup)] > 1) {
		for (cRank in 2:resTbl[, max(rankGroup)]) {
			incrementCIDs(resTbl[rankGroup == cRank - 1, max(targetCID)], cRank)
		}
	}
	resTbl = addCountRow(resTbl, normalizeGroupsP)
}

getSankeyTblTimeAnchor <- function(xTbl, anchor, direction='after', ...) {
	if (direction == 'both') {
		return(getSankeyTblTimeAnchorBoth(xTbl, anchor, ...))
	}
	yTbl = copy(xTbl)
	zTbl = data.table()
	yTbl[, .SD
	     ][label == anchor
	     ][, .(nSMin=min(nS), nSMax=max(nS)), .(sID, NS)
	     ][direction == 'after', nSMax := NS
	     ][direction == 'before', nSMin :=1
	     ][, yTbl[.SD, `:=`(sActiveP=T, snSMin=i.nSMin, snSMax=i.nSMax), on='sID']
	     ][sActiveP == T
	     ][nS >= snSMin & nS <= snSMax
	     ][, nS := 1:.N, sID
	     ][, NS := .N, sID
	     ][, updateDtPrev(copy(.SD))
	     ][, {zTbl <<- copy(.SD); .SD}
	     ]
	if (direction == 'before') {
		zTbl[, nS := rev(nS), sID]
		setorder(zTbl, uuid, sID, nS)
		updateDtPrev(zTbl)
	}
	getSankeyTblTime(zTbl, ...)
}

getSankeyTblTimeAnchorBoth <- function(xTbl, anchor, ...) {
	xbTbl = getSankeyTblTimeAnchor(xTbl, anchor, 'before', ...)
	xaTbl = getSankeyTblTimeAnchor(xTbl, anchor, 'after', ...)
	xaTbl[, step := step - min(step) + 1 + xbTbl[, max(step)]]
	xaTbl[, tSource := tSource + xbTbl[, max(abs(tTarget))]]
	xaTbl[, tTarget := tTarget + xbTbl[, max(abs(tTarget))]]
	xbTbl[step == 2]
	assert_that(all(xbTbl[step == 2, sourceCID] == 1))
	assert_that(all(xaTbl[step == min(step), sourceCID] == 1))
	adjVal = xbTbl[, max(targetCID)] - 1
	xaTbl[step > min(step), sourceCID := sourceCID + adjVal]
	xaTbl[, targetCID := targetCID + adjVal]
	xaTbl[, cID1 := NULL]
	cNames = grep('^cID', colnames(xaTbl), value=T)
	newNames = sprintf('cID%s', seq(from=xbTbl[, max(step)] + 1, length.out=length(cNames)))
	setnames(xaTbl, cNames, newNames)
	for (cName in newNames) {
		xaTbl[, (cName) := get(cName) + adjVal]
	}
	rbind(xbTbl, xaTbl, fill=T)
}

addSessionStart <- function(zTbl, labelGroup) {
	bTbl = copy(zTbl)
	assert_that(bTbl[, isRowSorted(as.matrix(.SD[, .(sID, nS)]))] == T)
	bTbl[, nS := nS + 1]
	bTbl[, NS := NS + 1]
	bTbl[, .SD
	     ][, .(nS=1:(.N+1), NS=.N+1), sID
	     ][, bTbl[copy(.SD), on=c('sID', 'nS', 'NS')]
	     ][order(sID, nS)
	     ][, `:=`(iDateTimeMin=iDateTime[2], itimeMin=itime[2], idateMin=idate[2]), .(sID, NS)
	     ][nS == 1, `:=`(iDateTime=iDateTimeMin, itime=itimeMin, idate=idateMin)
	     ][, updateDtPrev(copy(.SD))
	     ][nS == 1, label := labelGroup
	     ][, c('iDateTimeMin', 'itimeMin', 'idateMin') := NULL
	     ]
}

addSessionEnd <- function(bTbl, endLabel='') {  
	assert_that(bTbl[, isRowSorted(as.matrix(.SD[, .(sID, nS)]))] == T) 
	bTbl[, .(nS=1:(.N+1), NS=.N), sID 
	     ][, bTbl[.SD, on=c('sID', 'nS', 'NS')] 
	     ][order(sID, nS) 
	     ][, `:=`(iDateTimeMax=iDateTime[NS], itimeMax=itime[NS], idateMax=idate[NS]), .(sID, NS) 
	     ][, NS := .N, sID 
	     ][nS == NS, `:=`(iDateTime=iDateTimeMax, itime=itimeMax, idate=idateMax) 
	     ][, updateDtPrev(copy(.SD)) 
	     ][nS == NS, label := endLabel 
	     ][, c('iDateTimeMax', 'itimeMax', 'idateMax') := NULL 
	     ] 
} 

# http://stackoverflow.com/questions/7599146/testing-if-rows-of-a-matrix-or-data-frame-are-sorted-in-r
isRowSorted <- cxxfunction(signature(A="numeric"), body='
			   Rcpp::NumericMatrix Am(A);
			   for(int i = 1; i < Am.nrow(); i++) {
				   for(int j = 0; j < Am.ncol(); j++) {
					   if( Am(i-1,j) < Am(i,j) ) { break; }
					   if( Am(i-1,j) > Am(i,j) ) { return(wrap(false)); }
				   }
			   }
			   return(wrap(true));
			   ', plugin="Rcpp")

updateDtPrev <- function(fTbl) {
	assert_that(fTbl[, isRowSorted(as.matrix(.SD[, .(sID, nS)]))] == T)
	fTbl[, dtPrev := iDateTime - c(iDateTime[1], head(iDateTime, -1))]
	fTbl[nS == 1, dtPrev := NA]
	fTbl
}

updateDtNext <- function(fTbl) {
	fTbl[, assert_that(isRowSorted(as.matrix(.SD[, .(sID, nS)])) == T)]
	fTbl[, dtNext := c(tail(iDateTime, -1), tail(iDateTime, 1)) - iDateTime]
	fTbl[nS == NS, dtNext := NA]
	fTbl
}

addCIDs <- function(skTbl, maxStep) {
	skTbl[, cIDCur := 1]
	for (step in 1:maxStep) {
		addCID(skTbl, step)
	}
	makeUniqueCID(skTbl)
}

addCID <- function(skTbl, step) {
	skTbl[, .SD
	      ][nS <= step
	      ][, dcast(.SD, cIDCur + sID ~ nS, value.var='label')
	      ][, `:=`(cIDNew=.GRP, NSIDs=.N), by=eval(c('cIDCur', as.character(c(1:step))))
	      ][order(-NSIDs, cIDNew)
	      ][, {skTbl[.SD, `:=`(cIDNew=as.numeric(i.cIDNew)), on='sID']; .SD}
	      ]
	skTbl[, cIDCur := cIDNew]
	setnames(skTbl, 'cIDNew', sprintf('%s%s', 'cID', step))
	skTbl
}

makeUniqueCID <- function(skTbl) {
	cNames = sort(setdiff(grep('^cID', colnames(skTbl), value=T), 'cIDCur'))
	cNames
	cID = 0
	for (cName in cNames) {
		jTbl = skTbl[, .(cIDNew=.GRP + cID), by=eval(cName)]
		jTbl
		skTbl
		cName
		skTbl[jTbl, (cName) := i.cIDNew, on=cName]
		cID = cID + skTbl[, max(get(cName))]
	}
	skTbl[, cIDCur := get(tail(cNames, 1))]
	skTbl
}

getSkSumTbl <- function(skTbl) {
	steps = grep('^cID', colnames(skTbl), value=T) %>% setdiff(c('cIDCur', 'cID1')) %>% str_extract('[0-9]+$') %>% as.numeric
	resTbl = rbindlist(lapply(as.list(steps), function(c) getTsAtStep(skTbl, c)), use.names=T, fill=T)
	resTbl
	resTbl[, .(target, targetCID, tTarget)][resTbl[, .(source, sourceCID, tSource)], on=c(target='source', targetCID='sourceCID'), nomatch=0][tSource != tTarget][, nrow(.SD) == 0]
	resTbl[, .(source, sourceCID, tSource)][resTbl[, .(target, targetCID, tTarget)], on=c(source='target', sourceCID='targetCID'), nomatch=0][tSource != tTarget][, nrow(.SD) == 0]
	resTbl
}

getTsAtStep <- function(skTbl, step) {
	byCols = c(sprintf('%s%s', 'cID', 1:step))
	butlastByCols = head(byCols, -1)
	lastByCol = tail(byCols, 1)
	nextToLastByCol = tail(butlastByCols, 1)
	tTbl = data.table()
	skTbl[nS == step
	      ][, c(list(target=label, targetCID=get(lastByCol), dtTarget=dt, sID=sID), .SD[, byCols, with=F])
	      ][, {tTbl <<- copy(.SD); .SD}
	      ]
	sTbl = data.table()
	skTbl[nS == (step - 1)
	      ][, c(list(source=label, sourceCID=get(nextToLastByCol), dtSource=dt, sID=sID), .SD[, butlastByCols, with=F])
	      ][, {sTbl <<- copy(.SD); .SD}
	      ]
	sTbl
	tTbl
	tTbl[sTbl, dtSource := i.dtSource, on='sID']
	sTbl = sTbl[, .(dtSource=mean(dtSource)), eval(c(butlastByCols, 'sourceCID', 'source'))]
	resTbl = tTbl[, .(tTarget=mean(dtTarget), sdDt=sd(dtTarget-dtSource), count=.N, step), by=eval(c(byCols, 'target', 'targetCID'))]
	resTbl
	resTbl[sTbl, `:=`(source=i.source, tSource=dtSource, sourceCID=i.sourceCID), on=butlastByCols]
	resTbl[, dt := tTarget - tSource]
	cIDCols = grep('cID', colnames(resTbl), value=T)
	setcolorder(resTbl, c(setdiff(colnames(resTbl), cIDCols), cIDCols))
	resTbl
}

updateDrops <- function(resTbl) {
	resTbl[target == '', `:=`(tTarget=tSource, dt=0)]
	resTbl
}

addCountRow <- function(zTbl, normalizeGroupsP) {       
	addCountRowGrp <- function(resTbl, labelGroup) {
		cIDCols = grep('^cID', colnames(resTbl), value=T)
		resTbl[, countRowTarget := 0]
		resTbl[, endP := grepl('^ *$', target)]
		resTbl[, .(sSum=sum(count)), .(step, source)
		       ][order(-sSum)
		       ][, sRank := 1:.N
		       ][, resTbl[.SD, sRank := i.sRank, on=c('step', 'source')]
		       ]
		setorder(resTbl, step, sRank, endP, -count)
		resTbl[, sRank := NULL]
		resTbl[, endP := NULL]
		for (i in 1:(length(cIDCols))) {
			byCols = cIDCols[1:i]
			nextCol = cIDCols[i+1]
			resTbl[, .SD
			       ][(i == 1 & step <= i + 1) | step == i, .(count=sum(count)), by=eval(byCols)
			       ][!is.na(get(tail(byCols, n=1)))
			       ][, V3 := shift(count, fill=0), by=eval(head(byCols, n=-1))
			       ][, V4 := cumsum(V3), by=eval(head(byCols, n=-1))
			       ][, resTbl[.SD, `:=`(countRowTarget=countRowTarget + i.V4), on=byCols]
			       ]
		}
		wTbl = resTbl[, rbind(data.table(label=target, labelID=targetCID, countRowTarget=countRowTarget),
				      data.table(label=source, labelID=sourceCID, countRowTarget=countRowTarget))]
		wTbl[, .(countRowSource=min(countRowTarget)), .(label, labelID)
		     ][, wTbl <<- copy(.SD)
		     ]
		resTbl[wTbl, countRowSource := i.countRowSource, on=c(source='label', sourceCID='labelID')]
		resTbl[order(countRowSource, countRowTarget)]
	}
	zTbl
	resTbl = zTbl[, addCountRowGrp(copy(.SD), labelGroup), labelGroup]
	incrementCounts <- function(val, cRank) {
		cNames = grep('countRow', colnames(resTbl), value=T, ignore.case=T)
		for (cName in cNames) {
			resTbl[rankGroup == cRank, (cName) := get(cName) + val]
		}
		resTbl
	}
	if (resTbl[, max(rankGroup)] > 1) {
		for (cRank in 2:resTbl[, max(rankGroup)]) {
			incrementCounts(resTbl[rankGroup == cRank - 1 & step == 2, sum(count) + min(countRowSource)], cRank)
		}
	}
	updateSKValues(resTbl, normalizeGroupsP)
}

capMinTimes <- function(tbl, padProp=.01) {
	tPad = tbl[, max(tTarget) * padProp]
	while (tbl[tTarget < (tSource + tPad), .N] > 0) {
		tbl[tTarget < (tSource + tPad)
		    ][, .(i.targetCID=targetCID, i.tSource=tSource)
		    ][, {tbl[.SD, tTarget := i.tSource + tPad, on=c(targetCID='i.targetCID')]; tbl[.SD, tSource := i.tSource + tPad, on=c(sourceCID='i.targetCID')]}
		    ]
	}
}

relaxTimesToSteps <- function(tbl) {
	tbl[, .N, step]
	tbl[, dt := 1]
	tbl[, tTarget := step - 1]
	tbl[, tSource := step - 2]
	tbl[, .N, .(tSource, tTarget)]
	tbl[grepl('^ *$', target), `:=`(dt=0, tTarget=tSource)]
	tbl
}

updateSKValues <- function(resTbl, normalizeGroupsP) {
	NSs = resTbl[step == 2, sum(count)]
	resTbl[, value := count/NSs]
	resTbl[, valueRowSource := countRowSource/NSs]
	resTbl[, valueRowTarget := countRowTarget/NSs]
	if (normalizeGroupsP == T) {
		normalizeSKValues(resTbl)
	}
	resTbl
}

normalizeSKValues <- function(resTbl) {
	resTbl[, .SD
	       ][step == 2
	       ][, .(count=sum(count)), rankGroup
	       ][, resTbl[.SD, NSs := i.count, on=.(rankGroup)]
	       ]
	resTbl[, .SD
	       ][step == 2
	       ][, .(count=min(countRowSource)), rankGroup
	       ][, resTbl[.SD, BSs := i.count, on=.(rankGroup)]
	       ]
	resTbl[, value := count / NSs / uniqueN(rankGroup)]
	resTbl[, max(value), rankGroup]
	resTbl[, valueRowSource := (countRowSource - BSs) / NSs / uniqueN(rankGroup) + (rankGroup - 1) / uniqueN(rankGroup)]
	resTbl[, valueRowTarget := (countRowTarget - BSs) / NSs / uniqueN(rankGroup) + (rankGroup - 1) / uniqueN(rankGroup)]
	resTbl[, max(valueRowSource), rankGroup]
	resTbl[, c('NSs', 'BSs') := NULL]
	resTbl
}

makeUniqueTS <- function(resTbl) {
	spTbl = data.table()
	resTbl[, rbind(data.table(label=target, labelCID=targetCID), data.table(label=source, labelCID=sourceCID))
	       ][, unique(.SD, by=c('label', 'labelCID'))
	       ][, .(labelCID, grp=0:(.N-1)), .(label)
	       ][, spTbl <<- copy(.SD)
	       ]
	spTbl
	resTbl
	resTbl[spTbl, sourceSp := i.grp, on=c(source='label', sourceCID='labelCID')]
	resTbl[spTbl, targetSp := i.grp, on=c(target='label', targetCID='labelCID')]
	resTbl[, rowID := 1:.N]
	resTbl[, target := sprintf('%s%s', target, insertNB(targetSp)), rowID]
	resTbl[, source := sprintf('%s%s', source, insertNB(sourceSp)), rowID]
	resTbl[, `:=`(sourceSp=NULL, targetSp=NULL, rowID=NULL)]
	resTbl
}

insertNB <- function(n) paste(rep(' ', n), collapse='', sep='')

