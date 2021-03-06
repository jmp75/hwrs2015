# Create tree-catchments, to have a measurement baseline in O(n^2) to check how runtime scales
# daily and hourly operation

- system size
- time steps
- level of parallel runs (multi-threading)


library(ophct)
library(microbenchmark)
library(dplyr)
library(DiagrammeR)


numLinks <- function(streamOrder=1) {
  sum(2**(1:streamOrder))
}

nReps <- 10

defineNetwork <- function(streamOrder=1, runoffModel='AWBM') {
# o    o  o    o 
#  \  /    \  /    stream order 2
#    o      o
#     \    /      stream order 1
#       o
  # numLinks(0) = 0
  # numLinks(1) = 2
  # numLinks(2) = 4
  # numLinks(n) = numLinks(n-1)*2
  # so:
  # numLinks = sum(1->n)(2**n)

  # numNodes(0) = 1
  # numNodes(1) = 2
  # numNodes(2) = 4
  # numNodes(n) = numNodes(n-1)*2
  # numNodes = sum(1->n)(2**n) + 1

  nl <- numLinks(streamOrder)
  linkIds <- paste0("link_id_", 1:nl)
  nodeIds <- paste0("node_id_", 1:(nl+1))

  defn <- list(
    nodeIds=nodeIds,
    nodeNames = paste0(nodeIds, '_name'),
    linkIds=linkIds,
    linkNames = paste0(linkIds, '_name'),
    fromNode = nodeIds[2:length(nodeIds)], # 2,3,4,5,6,7,8,...
    toNode = rep(nodeIds, each=2)[1:(length(nodeIds)-1)], # 1,1,2,2,3,3,4,4...
    areasKm2 = rep(1.0, nl),
    runoffModel = runoffModel
  )
  defn
}

mkModelStructure <- function(defn) {
  createCatchment(
      defn$nodeIds, defn$nodeNames, 
      defn$linkIds, defn$linkNames, 
      defn$fromNode, defn$toNode, 
      defn$runoffModel, defn$areasKm2)
}

simulStart <- '2000-01-01'
simulEnd <- '2009-12-31'
sSpan <- paste0(simulStart, '/', simulEnd)

rain <- sampleSeries('Ovens', 'rain')[sSpan]
evap <- sampleSeries('Ovens', 'evap')[sSpan]
flow <- sampleSeries('Ovens', 'flow')[sSpan]

mkModel <- function(streamOrder, tstep='hourly') {
  simulation <- mkModelStructure(defineNetwork(streamOrder))
  setSimulationSpan(simulation, start(rain), end(rain))
  setSimulationTimeStep(simulation, tstep)
  simulation <- swapModel(simulation, 'MuskingumNonLinear', 'channel_routing')
  return(simulation)
}

cpSimul <- function(s) {
  return(clrCall(s, 'CloneModel'))
}

# Assess the impact of adding input time series and output time series.

baseline <- mkModel(3) 

s1 <- cpSimul(baseline)
sIds <- getSubareaIds(s1)

for (s in sIds) {
  playSubareaInput(s1, input=rain, s, 'Rainfall')
}
s2 <- cpSimul(s1)
for (s in sIds) {
  playSubareaInput(s2, input=evap, s, 'Evapotranspiration')
}

s3 <- cpSimul(s2)
for (s in sIds) { recordState(s3, paste('subarea', s, 'Runoff', sep='.')) }

s4 <- cpSimul(s3)
for (s in sIds) { recordState(s4, paste('subarea', s, 'Baseflow', sep='.')) }

systems <- list(baseline, s1, s2, s3, s4)
f <- function(i) {execSimulation(systems[[i]])}

mb <- microbenchmark(f(1), f(2), f(3), f(4), f(5), times=nReps, control = list(order='block'))
plot(mb, ylab = 'time (ns)')

getMeanTime <- function(mbench) {
  m <- as.data.frame(mbench)
  m_g <- group_by(m, expr)
  summarise(m_g, time = mean(time, na.rm = TRUE))
}

mkTypical <- function(streamOrder, tstep='daily') {
  sim <- mkModel(streamOrder) 
  sIds <- getSubareaIds(sim)
  for (s in sIds) {
    playSubareaInput(sim, input=rain, s, 'Rainfall')
    playSubareaInput(sim, input=evap, s, 'Evapotranspiration')
  }
  recordState(sim, 'Catchment.StreamflowRate')
  return(sim)
}

systems <- lapply( 1:4, function(i) { mkTypical(i) } )

mb <- microbenchmark(f(1),f(2),f(3),f(4), times=nReps, control = list(order='block'))

plot(mb, ylab = 'time (ns)')
m <- getMeanTime(mb)
plot( sqrt(time) ~ expr, data=m)

d <- getCatchmentDotGraph(s4)
DiagrammeR(d)
