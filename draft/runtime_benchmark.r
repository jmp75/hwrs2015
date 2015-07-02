library(ophct)

# Create tree-catchments, to have a measurement baseline in O(n^2) to check how runtime scales

# daily and hourly operation

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

  numLinks <- sum(2**(1:streamOrder))
  linkIds <- paste0("link_id_", 1:numLinks)
  nodeIds <- paste0("node_id_", 1:(numLinks+1))

  defn <- list(
    nodeIds=nodeIds,
    nodeNames = paste0(nodeIds, '_name'),
    linkIds=linkIds,
    linkNames = paste0(linkIds, '_name'),
    fromNode = nodeIds[2:length(nodeIds)], # 2,3,4,5,6,7,8,...
    toNode = rep(nodeIds, each=2)[1:(length(nodeIds)-1)], # 1,1,2,2,3,3,4,4...
    areasKm2 = rep(1.0, numLinks),
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


simulStart<-'1990-01-01'
simulEnd<-'2005-12-31'
sSpan <- paste0(simulStart, '/', simulEnd)

rain <- sampleSeries('MMH', 'rain')[sSpan]
evap <- sampleSeries('MMH', 'evap')[sSpan]
flow <- sampleSeries('MMH', 'flow')[sSpan]

mkModel <- function(streamOrder, tstep='daily') {
  simulation <- mkModelStructure(defineNetwork(streamOrder))
  setSimulationSpan(simulation, start(rain), end(rain))
  setSimulationTimeStep(simulation, tstep)
  simulation <- swapModel(simulation, 'MuskingumNonLinear', 'channel_routing')
  return(simulation)
}

baseline <- mkModel(3) 

s1 <- clrCall(baseline, 'CloneModel')
sIds <- getSubareaIds(s1)

# sw = clrNew('System.Diagnostics.Stopwatch')
# clrReflect('sw')
# clrReflect(sw)
# clrGet(sw, 'Elapsed')
# timer

# install.packages('microbenchmark')

library(microbenchmark)
for (s in sIds) {
  playSubareaInput(s1, input=rain, s, 'Rainfall')
}
s2 <- clrCall(s1, 'CloneModel')
for (s in sIds) {
  playSubareaInput(s2, input=evap, s, 'Evapotranspiration')
}

s3 <- clrCall(s2, 'CloneModel')
for (s in sIds) { recordState(s3, paste('subarea', s, 'Runoff', sep='.')) }

s4 <- clrCall(s3, 'CloneModel')
for (s in sIds) { recordState(s4, paste('subarea', s, 'Baseflow', sep='.')) }

f_0 <- function() {execSimulation(baseline)}
f_1 <- function() {execSimulation(s1)}
f_2 <- function() {execSimulation(s2)}
f_3 <- function() {execSimulation(s3)}
f_4 <- function() {execSimulation(s4)}

mb <- microbenchmark(f_0(), f_1(), f_2(), f_3(), f_4(), times=100, control = list(order='block'))

# plot(getRecorded(s2, getRecordedVarnames(s2)[1]))

plot(mb, ylab = 'time (ns)')

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

s1 <- mkTypical(1)
s2 <- mkTypical(2)
s3 <- mkTypical(3)
s4 <- mkTypical(4)
f_1 <- function() {execSimulation(s1)}
f_2 <- function() {execSimulation(s2)}
f_3 <- function() {execSimulation(s3)}
f_4 <- function() {execSimulation(s4)}
mb <- microbenchmark(f_1(), f_2(), f_3(), f_4(), times=100, control = list(order='block'))
plot(mb, ylab = 'time (ns)')


library(DiagrammeR)
d <- getCatchmentDotGraph(s4)
DiagrammeR(d)
