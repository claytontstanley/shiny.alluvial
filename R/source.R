library(data.table)
library(shiny)
library(testthat)
library(assertthat)
library(inline)
library(stringr)
library(stringi)
library(magrittr)
library(R.utils)
library(Rcpp)

.SAPATH = system.file(package="shiny.alluvial")

#' @import data.table shiny testthat assertthat inline stringr stringi magrittr R.utils Rcpp
NULL

#' @exportPattern ^[^\.]
NULL

#' Launch a webpage that shows the alluvial visualization 
#'
#' @param zTbl timestamped, sessionized table in proper format
#' @param ... Additional arguments passed to shiny::runApp
#'
#' @return None
#'
#' @examples
#' runAppSK(sessionTbl)
#'
#' @export
runAppSK <- function(zTbl, ...) {
	reassignInPackage("runApp", "shiny", runAppMod)
	.sessionTbl <<- copy(zTbl)
	addResourcePath('shiny_alluvial', .SAPATH) 
	shiny::runApp(.SAPATH, host="0.0.0.0", port=3343, ...)
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
        sankeyPlot = list()
        sankeyPlot$params = 
                list(
                     data = resTbl,
                     nodeWidth = 25,
                     nodePadding = 30,
                     layout = 32,
                     width = 3840,
                     height = 2160,
                     labelFormat = ".1%"
                     )
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

updateDtPrev <- function(fTbl) {
	fTbl[, dtPrev := iDateTime - c(iDateTime[1], head(iDateTime, -1))]
	fTbl[nS == 1, dtPrev := NA]
	fTbl
}

updateDtNext <- function(fTbl) {
	fTbl[, dtNext := c(tail(iDateTime, -1), tail(iDateTime, 1)) - iDateTime]
	fTbl[nS == NS, dtNext := NA]
	fTbl
}

addCIDs <- function(skTbl, maxStep) {
	skTbl[, cIDCur := 1]
	for (step in 1:(min(c(maxStep, skTbl[, max(NS)])))) {
		addCID(skTbl, step)
	}
	makeUniqueCID(skTbl)
}

addCID <- function(skTbl, step) {
        #print(sprintf("addCID %s", step))
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
        #print(sprintf("getSkSumTbl"))
	steps = grep('^cID', colnames(skTbl), value=T) %>% setdiff(c('cIDCur', 'cID1')) %>% str_extract('[0-9]+$') %>% as.numeric
	resTbl = rbindlist(lapply(as.list(steps), function(c) getTsAtStep(skTbl, c)), use.names=T, fill=T)
	resTbl
	resTbl[, .(target, targetCID, tTarget)][resTbl[, .(source, sourceCID, tSource)], on=c(target='source', targetCID='sourceCID'), nomatch=0][tSource != tTarget][, nrow(.SD) == 0]
	resTbl[, .(source, sourceCID, tSource)][resTbl[, .(target, targetCID, tTarget)], on=c(source='target', sourceCID='targetCID'), nomatch=0][tSource != tTarget][, nrow(.SD) == 0]
	resTbl
}

getTsAtStep <- function(skTbl, step) {
        #print(sprintf("getTsAtStep", step))
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
        #print(sprintf("addCountRow %s", normalizeGroupsP))
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
        #print(sprintf("updateSKValues %s", normalizeGroupsP))
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

addSessionRolling <- function(fTbl, keyv, maxDt=15) {
        setorderv(fTbl, keyv)
        fTbl[, sIDOuter := .GRP, by=eval(head(keyv, -1))]
        fTbl[, dtPrev := iDateTime - shift(iDateTime)]
        fTbl[, dsIDPrev := sIDOuter - shift(sIDOuter)]
        fTbl[!dsIDPrev %in% c(NA, 0, 1)][, assert_that(nrow(.SD) == 0)]
        fTbl[, sID := cumsum(is.na(dtPrev) | dtPrev >= maxDt | dsIDPrev == 1)]
        fTbl[, dsIDPrev := NULL]
        fTbl[, sIDOuter := NULL]
        fTbl[, NS := .N, sID]
        fTbl[, nS := rowid(sID)]
        updateDtPrev(fTbl)
        fTbl
}

updateDtPrev <- function(fTbl) {
        fTbl[, dtPrev := iDateTime - c(iDateTime[1], head(iDateTime, -1))]
        fTbl[nS == 1, dtPrev := NA]
        fTbl
}

updateDtNext <- function(fTbl) {
        fTbl[, dtNext := c(tail(iDateTime, -1), tail(iDateTime, 1)) - iDateTime]
        fTbl[nS == NS, dtNext := NA]
        fTbl
}

test_that("addSessionRolling", {
        cTbl = data.table(uuid=c(1,1,2,2,3,3), iDateTime=as.Date(c(1,10,1,2,2,3), origin='1970-01-01'))
        addSessionRolling(cTbl, c('uuid', 'iDateTime'), 1.5)[, assert_that(all(sID == c(1, 2, 3, 3, 4, 4)))]
        cTbl = data.table(uuid=c(1,1,1,2,3,3), iDateTime=as.Date(c(1,10,1,2,2,3), origin='1970-01-01'))
        addSessionRolling(cTbl, c('uuid', 'iDateTime'), 1.5)
        cTbl[, assert_that(all(sID == c(1, 1, 2, 3, 4, 4)))]
        cTbl[, assert_that(all(uuid == c(1,1,1,2,3,3)))]
        cTbl[, assert_that(all(iDateTime == as.Date(c(1,1,10,2,2,3), origin='1970-01-01')))]
        cTbl[, expect_identical(dtPrev, as.difftime(c(NA, 0, NA, NA, NA, 1), units='days'))]
        cTbl
})

addSLabels <- function(zTbl) {
        zTbl[grepl(',', label)][, assert_that(nrow(.SD) == 0)]
        zTbl[, sLabel := paste0(sprintf(",%s", label), collapse=''), sID]
        zTbl[, .zPatt := strrep(",[^,]+", nS)]
        zTbl[, .zPatt := sprintf('^%s', .zPatt)]
        zTbl[, .SD
             ][, .N, .(sLabel, .zPatt)
             ][, sUptoLabel := str_extract(sLabel, .zPatt)
             ][, sUptoLabel := gsub('^,', '', sUptoLabel)
             ][, zTbl[.SD, sUptoLabel := i.sUptoLabel, on=.(sLabel, .zPatt)]
             ]
        zTbl[, sLabel := gsub('^,', '', sLabel)]
        zTbl[, .zPatt := NULL]
        zTbl
}

ensureSLabels <- function(zTbl) {
        if (! 'sUptoLabel' %in% colnames(zTbl)) {
                addSLabels(zTbl)
        }
        zTbl
}

grepForSessions <- function(zTbl, patt, invert=F, col='sUptoLabel') {
#        print(sprintf('grepping for: %s, %s, %s', patt, invert, col))
        zTbl[, assert_that("sKey" %in% colnames(zTbl))]
        key = fromJSON(zTbl[, sKey[1]])$key
        maxDt = fromJSON(zTbl[, sKey[1]])$maxDt
        zTbl[, .SD
             ][grepl(patt, get(col)) != invert
             ][, addSessionRolling(copy(.SD), key, maxDt=maxDt)
             ][, {tempTbl <<- copy(.SD); .SD}
             ][, .SD
             ]
}

runAppMod <- function (appDir = getwd(), port = getOption("shiny.port"), launch.browser = getOption("shiny.launch.browser", 
    interactive()), host = getOption("shiny.host", "127.0.0.1"), 
    workerId = "", quiet = FALSE, display.mode = c("auto", "normal", 
        "showcase"), test.mode = getOption("shiny.testmode", 
        FALSE), queryString="") 
{
    on.exit({
        handlerManager$clear()
    }, add = TRUE)
    if (.globals$running) {
        stop("Can't call `runApp()` from within `runApp()`. If your ", 
            "application code contains `runApp()`, please remove it.")
    }
    .globals$running <- TRUE
    on.exit({
        .globals$running <- FALSE
    }, add = TRUE)
    oldOptionSet <- .globals$options
    on.exit({
        .globals$options <- oldOptionSet
    }, add = TRUE)
    ops <- options(warn = max(1, getOption("warn", default = 1)), 
        pool.scheduler = scheduleTask)
    on.exit(options(ops), add = TRUE)
    appParts <- as.shiny.appobj(appDir)
    appOps <- appParts$options
    findVal <- function(arg, default) {
        if (arg %in% names(appOps)) 
            appOps[[arg]]
        else default
    }
    if (missing(port)) 
        port <- findVal("port", port)
    if (missing(launch.browser)) 
        launch.browser <- findVal("launch.browser", launch.browser)
    if (missing(host)) 
        host <- findVal("host", host)
    if (missing(quiet)) 
        quiet <- findVal("quiet", quiet)
    if (missing(display.mode)) 
        display.mode <- findVal("display.mode", display.mode)
    if (missing(test.mode)) 
        test.mode <- findVal("test.mode", test.mode)
    if (is.null(host) || is.na(host)) 
        host <- "0.0.0.0"
    workerId(workerId)
    if (inShinyServer()) {
        ver <- Sys.getenv("SHINY_SERVER_VERSION")
        if (utils::compareVersion(ver, .shinyServerMinVersion) < 
            0) {
            warning("Shiny Server v", .shinyServerMinVersion, 
                " or later is required; please upgrade!")
        }
    }
    setShowcaseDefault(0)
    .globals$testMode <- test.mode
    if (test.mode) {
        message("Running application in test mode.")
    }
    if (is.character(appDir)) {
        desc <- file.path.ci(if (tolower(tools::file_ext(appDir)) == 
            "r") 
            dirname(appDir)
        else appDir, "DESCRIPTION")
        if (file.exists(desc)) {
            con <- file(desc, encoding = checkEncoding(desc))
            on.exit(close(con), add = TRUE)
            settings <- read.dcf(con)
            if ("DisplayMode" %in% colnames(settings)) {
                mode <- settings[1, "DisplayMode"]
                if (mode == "Showcase") {
                  setShowcaseDefault(1)
                  if ("IncludeWWW" %in% colnames(settings)) {
                    .globals$IncludeWWW <- as.logical(settings[1, 
                      "IncludeWWW"])
                    if (is.na(.globals$IncludeWWW)) {
                      stop("In your Description file, `IncludeWWW` ", 
                        "must be set to `True` (default) or `False`")
                    }
                  }
                  else {
                    .globals$IncludeWWW <- TRUE
                  }
                }
            }
        }
    }
    if (is.null(.globals$IncludeWWW) || is.na(.globals$IncludeWWW)) {
        .globals$IncludeWWW <- TRUE
    }
    display.mode <- match.arg(display.mode)
    if (display.mode == "normal") {
        setShowcaseDefault(0)
    }
    else if (display.mode == "showcase") {
        setShowcaseDefault(1)
    }
    require(shiny)
    if (is.null(port)) {
        for (i in 1:20) {
            if (!is.null(.globals$lastPort)) {
                port <- .globals$lastPort
                .globals$lastPort <- NULL
            }
            else {
                while (TRUE) {
                  port <- p_randomInt(3000, 8000)
                  if (!port %in% c(3659, 4045, 6000, 6665:6669, 
                    6697)) {
                    break
                  }
                }
            }
            tmp <- try(startServer(host, port, list()), silent = TRUE)
            if (!inherits(tmp, "try-error")) {
                stopServer(tmp)
                .globals$lastPort <- port
                break
            }
        }
    }
    on.exit({
        .globals$onStopCallbacks$invoke()
        .globals$onStopCallbacks <- Callbacks$new()
    }, add = TRUE)
    unconsumeAppOptions(appParts$appOptions)
    if (!is.null(appParts$onStop)) 
        on.exit(appParts$onStop(), add = TRUE)
    if (!is.null(appParts$onStart)) 
        appParts$onStart()
    server <- startApp(appParts, port, host, quiet)
    on.exit({
        stopServer(server)
    }, add = TRUE)
    if (!is.character(port)) {
        browseHost <- if (identical(host, "0.0.0.0")) 
            "127.0.0.1"
        else host
        appUrl <- paste("http://", browseHost, ":", port, sep = "")
        if (queryString != "") {
                appUrl = sprintf("%s/?%s", appUrl, queryString)
        }
        if (is.function(launch.browser)) 
            launch.browser(appUrl)
        else if (launch.browser) 
            utils::browseURL(appUrl)
    }
    else {
        appUrl <- NULL
    }
    callAppHook("onAppStart", appUrl)
    on.exit({
        callAppHook("onAppStop", appUrl)
    }, add = TRUE)
    .globals$reterror <- NULL
    .globals$retval <- NULL
    .globals$stopped <- FALSE
    ..stacktraceoff..(captureStackTraces({
        while (!.globals$stopped) {
            ..stacktracefloor..(serviceApp())
            Sys.sleep(0.001)
        }
    }))
    if (isTRUE(.globals$reterror)) {
        stop(.globals$retval)
    }
    else if (.globals$retval$visible) 
        .globals$retval$value
    else invisible(.globals$retval$value)
}
