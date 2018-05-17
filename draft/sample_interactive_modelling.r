library(calibragem); library(ophct)
data('swift_sample_data')
ms <- createSubareaSimulation(dataId='MMH', '1990-01-01', '2005-12-31', 'GR4J', 'daily', 'P', 'E')
runoffId <- 'subarea.Subarea.runoff'
recordState(ms, runoffId)
execSimulation(ms)
obsRunoff <- sampleSeries('MMH', 'flow') #actually, runoff depth
obsRunoff[which(obsRunoff < -1)] <- NA
s <- start(obsRunoff)
w <- s + years(2)
e <- end(obsRunoff)
setSimulationSpan(ms, s, e)
objective <- createObjective(ms, runoffId, obsRunoff, 'NSE', w, e)

defineGr4jScaledParameter <- function( refArea=250, timeSpan=3600, pSpecGr4j) {
  timeSpan <- as.integer(timeSpan)
  parameterizer <- gr4jScaledParameterizer(refArea, timeSpan)
  if(missing(pSpecGr4j))
  {
    pSpecGr4j <- getFreeParams('GR4J')
    pSpecGr4j$Value <- c(3.21137E+00, 6.95511E+00, 2.06740E+00, 2.02033E+00)
    pSpecGr4j$Min <- c(1.0E+00, -2.70E+01, 1.0E+00, 1.0E+00)
    pSpecGr4j$Max <- c(6.0E+03,  2.70E+01, 1.0E+03, 2.4E+02)
  }
  setHyperCube(parameterizer, pSpecGr4j, TRUE)
  parameterizer <- wrapTransform(parameterizer)
  addTransform(parameterizer, 'log_x4', 'x4', 'log10')
  addTransform(parameterizer, 'log_x1', 'x1', 'log10')
  addTransform(parameterizer, 'log_x3', 'x3', 'log10')
  addTransform(parameterizer, 'asinh_x2', 'x2', 'asinh')
  
  return(parameterizer)
}
p <- defineGr4jScaledParameter(refArea=1.0, timeSpan=86400L)

term <- getMarginalTermination(tolerance = 1e-05, cutoffNoImprovement = 25, maxHours = 0.5) 
optimizer <- createSceOptimSwift(objective, p, termination=term)

calibLogger <- setCalibrationLogger(optimizer)
calibResults <- executeOptimization(optimizer)

## ------------------------------------------------------------------------
logMh <- mh::processLogger(optimizer, fitness='NSE')
geomOps <- mh::subsetByMessage(logMh)
str(geomOps@data)

pVarIds <- toDotSeparators((pSetAsDataFrame(p))$Name)
pVarIds


boundFitness <- function(logMh, objLims=NULL) {
  d <- logMh@data
  if(!is.null(objLims)) {
    d <- boundValuesDf(d, logMh@fitness, objLims)
  }
  d
}

plotParamEvolution <- function(logMh, paramName, objLims=NULL, title="Evolution of parameter values", xlab="Logged point", ylab=paramName) {
  d <- boundFitness(logMh, objLims)
  ggplot(d, aes_string(x = numColname, y = paramName, colour=logMh@fitness)) + 
    geom_point() + labs( title=title, x=xlab, y=ylab) +
    scale_colour_continuous(low="blue", high="red")
}

## ------------------------------------------------------------------------
print(plotParamEvolution(geomOps, pVarIds[1], objLims=c(0,1)))


####################################
## More concise sample code for screen capture

library(calibragem); library(ophct)
data('swift_sample_data')
ms <- createSubareaSimulation(dataId='MMH', '1990-01-01',
   '2005-12-31', 'GR4J', 'daily', 'P', 'E')
runoffId <- 'subarea.Subarea.runoff'
recordState(ms, runoffId)
obsRunoff <- sampleSeries('MMH', 'flow') 
s <- start(obsRunoff); e <- end(obsRunoff)
w <- s + years(2); setSimulationSpan(ms, s, e)
objective <- createObjective(ms, runoffId, obsRunoff, 'NSE', w, e)


pSpecGr4j <- getFreeParams('GR4J')
setHyperCube(parameterizer, pSpecGr4j, TRUE)
parameterizer <- wrapTransform(parameterizer)
addTransform(parameterizer, 'asinh_x2', 'x2', 'asinh')



library(swift)
runoffModel='GR4J'

nodeIds=paste0('n', 1:6)
linkIds = paste0('lnk', 1:5)
defn <- list(
	nodeIds=nodeIds,
	nodeNames = paste0(nodeIds, '_name'),
	linkIds=linkIds,
	linkNames = paste0(linkIds, '_name'),
	fromNode = paste0('n', c(2,5,4,3,1)),
	toNode = paste0('n', c(6,2,2,4,4)),
	areasKm2 = c(1.2, 2.3, 4.4, 2.2, 1.5),
	runoffModel = runoffModel
)
simulation <- createCatchment(defn$nodeIds, defn$nodeNames, defn$linkIds, defn$linkNames, defn$fromNode, defn$toNode, defn$runoffModel, defn$areasKm2)

evapIds <- paste( 'subarea', getSubareaIds(simulation), 'E', sep='.')
dataLibrary <- uchronia::getEnsembleDataSet('f:/tmp/blah.yaml')
playInputs(simulation, dataLibrary, precipIds, rep('rain_obs', length(precipIds)))
playInputs(simulation, dataLibrary, evapIds, rep('pet_obs', length(evapIds)), 'daily_to_hourly')

recordState(simulation, 'Catchment.StreamflowRate')

snapshot <- snapshotState(simulation)
snapshot$time
snapshot$states

simulation <- loadSimulation('f:/swift/sample/south_esk.json')
dataLibrary <- uchronia::getEnsembleDataSet('f:/swift/sample/fcast.yaml')
precipIds <- paste( 'subarea', getSubareaIds(simulation), 'P', sep='.')
inputMap <- list(rain_fcast_ens=precipIds)
ems <- createEnsembleForecastSimulation(simulation, dataLibrary, 
      '2003-01-01', '2003-01-03', inputMap, leadTime=10)
execSimulation(ems)






