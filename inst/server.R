library(shiny)
library(data.table)

shinyServer(function(input, output, session) {
		    `%||%` <- function(x, y){
			    if (!is.null(x)) x else y
		    }
		    print("Processing Request")
		    observe({
			    query = parseQueryString(session$clientData$url_search)
			    anchor = query$anchor %||% .sessionTbl$.anchor[1] 
			    stepMax = as.numeric(query$stepMax %||% .sessionTbl$.stepMax[1] %||% 3)
			    direction = query$direction %||% .sessionTbl$.direction[1] %||% 'after'
			    exactTimeP = as.logical(query$exactTimeP %||% .sessionTbl$.exactTimeP[1] %||% F)
			    groupField = query$groupField %||% .sessionTbl$.groupField[1] %||% ''
			    normalizeGroupsP = as.logical(query$normalizeGroupsP %||% .sessionTbl$.normalizeGroupsP[1] %||% F)
			    sampleBy = as.numeric(query$sampleBy %||% .sessionTbl$.sampleBy[1] %||% 1)
                            grepFor = query[["grepFor"]] %||% ""
                            grepOut = query[["grepOut"]] %||% ""
                            grepForUpto = query[["grepForUpto"]] %||% ""
                            grepOutUpto = query[["grepOutUpto"]] %||% ""
                            grepArgs = c(grepFor, grepOut, grepForUpto, grepOutUpto)
                            sKeyKey = query$sKeyKey %||% ""
                            sKeyMaxDt = query$sKeyMaxDt %||% "0"
                            
                            zTbl = copy(.sessionTbl)
                            zTbl = zTbl[uuid %% sampleBy == 0]

                            if (sKeyKey != "") {
                                    key = eval(parse(text=sKeyKey))
                                    maxDt = eval(parse(text=sKeyMaxDt))
                                    zTbl[, sKey := toJSON(list(key=key, maxDt=maxDt))]
                                    addSessionRolling(zTbl, key, maxDt)
                                    zTbl[, sLabel := NULL]
                                    zTbl[, sUptoLabel := NULL]
                            }

                            if (! all(grepArgs == "")) {
                                    ensureSLabels(zTbl)
                            }
                            if (grepFor != "") {
                                    zTbl = grepForSessions(zTbl, grepFor, col='sLabel')
                            }
                            if (grepOut != "") {
                                    zTbl = grepForSessions(zTbl, grepOut, invert=T, col='sLabel')
                            }
                            if (grepForUpto != "") {
                                    zTbl = grepForSessions(zTbl, grepForUpto, col='sUptoLabel')
                            }
                            if (grepOutUpto != "") {
                                    zTbl = grepForSessions(zTbl, grepOutUpto, invert=T, col='sUptoLabel')
                            }

			    print("Computing SK Chart")
			    if (is.null(anchor)) {
				    sankeyPlot = getSKChart(getSankeyTblTime(zTbl, stepMax=stepMax, groupField=groupField, normalizeGroupsP=normalizeGroupsP), exactTimeP=exactTimeP)
			    } else {
				    sankeyPlot = getSKChart(getSankeyTblTimeAnchor(zTbl, anchor=anchor, direction=direction, stepMax=stepMax, groupField=groupField, normalizeGroupsP=normalizeGroupsP), exactTimeP=exactTimeP)
			    }

			    print("Sending Results")
			    session$sendCustomMessage(type='testmessage', message=sankeyPlot$params)
		    })
})
