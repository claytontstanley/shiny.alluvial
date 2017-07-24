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
			    print("Computing SK Chart")
			    if (is.null(anchor)) {
				    sankeyPlot = getSKChart(getSankeyTblTime(.sessionTbl[uuid %% sampleBy == 0], stepMax=stepMax, groupField=groupField, normalizeGroupsP=normalizeGroupsP), exactTimeP=exactTimeP)
			    } else {
				    sankeyPlot = getSKChart(getSankeyTblTimeAnchor(.sessionTbl[uuid %% sampleBy == 0], anchor=anchor, direction=direction, stepMax=stepMax, groupField=groupField, normalizeGroupsP=normalizeGroupsP), exactTimeP=exactTimeP)
			    }
			    print("Sending Results")
			    session$sendCustomMessage(type='testmessage', message=sankeyPlot$params)
		    })
})
