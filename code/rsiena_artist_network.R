# Load libraries
library(igraph)
library(RSiena)
library(methods)
library(parallel)

# Generate a cluster
no_cores <- detectCores() - 1
siena_cl <- makeCluster(no_cores)
print(no_cores)

# Set Beginning year of the period
args <- commandArgs(trailingOnly=TRUE)
period <- args[1]

# Set working directory
dpath <- paste(c('../data/all_unique_id/p', period), collapse='')
setwd(dpath)

# Generate market, professional graph
m1 <- graph.data.frame(read.table("market_all_1.dat", header=F), directed=F)
m1 <- add_vertices(m1, 1000-gorder(m1))
m2 <- graph.data.frame(read.table("market_all_2.dat", header=F), directed=F)
m2 <- add_vertices(m2, 1000-gorder(m2))
m3 <- graph.data.frame(read.table("market_all_3.dat", header=F), directed=F)
m3 <- add_vertices(m3, 1000-gorder(m3))
m4 <- graph.data.frame(read.table("market_all_4.dat", header=F), directed=F)
m4 <- add_vertices(m4, 1000-gorder(m4))
m5 <- graph.data.frame(read.table("market_all_5.dat", header=F), directed=F)
m5 <- add_vertices(m5, 1000-gorder(m5))

p1 <- graph.data.frame(read.table("pro_all_1.dat", header=F), directed=F)
p1 <- add_vertices(p1, 1000-gorder(p1))
p2 <- graph.data.frame(read.table("pro_all_2.dat", header=F), directed=F)
p2 <- add_vertices(p2, 1000-gorder(p2))
p3 <- graph.data.frame(read.table("pro_all_3.dat", header=F), directed=F)
p3 <- add_vertices(p3, 1000-gorder(p3))
p4 <- graph.data.frame(read.table("pro_all_4.dat", header=F), directed=F)
p4 <- add_vertices(p4, 1000-gorder(p4))
p5 <- graph.data.frame(read.table("pro_all_5.dat", header=F), directed=F)
p5 <- add_vertices(p5, 1000-gorder(p5))


# To get the adjacency matrix from the network(unweighted; Siena does not allow weighted matrix)
mg1 = as.matrix(as_adjacency_matrix(m1, attr=NULL, sparse=T))
mg2 = as.matrix(as_adjacency_matrix(m2, attr=NULL, sparse=T))
mg3 = as.matrix(as_adjacency_matrix(m3, attr=NULL, sparse=T))
mg4 = as.matrix(as_adjacency_matrix(m4, attr=NULL, sparse=T))
mg5 = as.matrix(as_adjacency_matrix(m5, attr=NULL, sparse=T))

pg1 = as.matrix(as_adjacency_matrix(p1, attr=NULL, sparse=T))
pg2 = as.matrix(as_adjacency_matrix(p2, attr=NULL, sparse=T))
pg3 = as.matrix(as_adjacency_matrix(p3, attr=NULL, sparse=T))
pg4 = as.matrix(as_adjacency_matrix(p4, attr=NULL, sparse=T))
pg5 = as.matrix(as_adjacency_matrix(p5, attr=NULL, sparse=T))

# Put network data in an array with dimensions N x N x T where:
# N is the number of nodes in the network (here, # of artists 100) 
# and T is the number of time points/kind of network (here 2)

mnet <-array(c(mg1, mg2, mg3, mg4, mg5), dim=c(1000,1000,5))
pnet <-array(c(pg1, pg2, pg3, pg4, pg5), dim=c(1000,1000,5))

# Prepare covariates data for RSiena
# age.vc <- varCovar(age)
ranking <- varCovar(as.matrix(read.table("ranking_all.dat")))
soloshow <- varCovar(as.matrix(read.table("soloshow_all.dat")))
award <- varCovar(as.matrix(read.table("award_all.dat")))
meanprice <- varCovar(as.matrix(read.table("meanprice_all.dat")))
festbiennal <- varCovar(as.matrix(read.table("festbiennal_all.dat")))
publicinst <- varCovar(as.matrix(read.table("publicinst_all.dat")))
countyear <- varCovar(as.matrix(read.table("countyear_all.dat")))
groupshow <- varCovar(as.matrix(read.table("groupshow_all.dat")))

# Create a Siena network object with sienaNet()
mnet <- sienaDependent(mnet)
pnet <- sienaDependent(pnet)

#Data Create
data <- sienaDataCreate(mnet, pnet, award, meanprice, ranking, soloshow, 
                                   festbiennal, publicinst, countyear, groupshow)

#get an outline of the data set with some basic descriptives from
print(data)

####   Running the model and looking at results                           ####
####                                                                      ####

# For the model specification we need to create the effects object
effs <- getEffects(data)

#adding between-networks(multiple networks) effects
effs <- includeEffects(effs, crprod, name="mnet",interaction1="mnet")
effs <- includeEffects(effs, crprod, name="pnet",interaction1="pnet")

#adding covariate effects
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="ranking", name="mnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="soloshow", name="mnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="award", name="mnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="meanprice", name="mnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="festbiennal", name="mnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="publicinst", name="mnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="countyear", name="mnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="groupshow", name="mnet")

effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="ranking", name="pnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="soloshow", name="pnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="award", name="pnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="meanprice", name="pnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="festbiennal", name="pnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="publicinst", name="pnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="countyear", name="pnet")
effs <- includeEffects(effs, egoPlusAltX, simX, interaction1="groupshow", name="pnet")

#model running
proj = paste(c('two_net_model_period_', period), collapse='')
outf_html = paste(c('result_two_net_model_period_', period, '.html'), collapse='')
outf_tex = paste(c('result_two_net_model_period_', period, '.tex'), collapse='')
result <- sienaAlgorithmCreate(projname = proj)
stime = Sys.time()
print(stime)
result <- siena07(result, data=data, effects=effs, batch=TRUE, cl=siena_cl)
xtable(result, file=outf_html, type="html")
xtable(result, file=outf_tex, type="latex")
etime = Sys.time()
print(etime)
print(etime-stime)




