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
