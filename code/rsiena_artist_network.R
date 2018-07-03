#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# Import libraries
library(igraph)
library(abind)
library(RSiena)

# Test if there are two arguments
if (length(args)!=2) {
    stop("Two arguments (all or top) and (1, 2, or 3 for period) 
         are required.", call.=FALSE)
    }

# Setting depending on argument values
# Top or All
if (args[1]=='top') {N = 85} 
else {N = 1000}

# Period
if (args[2]==1){yrs=c('01', '02', '03', '04', '05')}
else if (args[2]==1){yrs=c('06', '07', '08', '09', '10')}
else {yrs=c('11', '12', '13', '14', '15')}

# Generate market network
mk_list <- list()
for(i in yrs) {
  dname <- paste(c("mk_", args[1], "_20", i, ".dat"), collapse = '')
  edgelist <- as.matrix(read.table(dname, header=F))
  
  g = make_empty_graph(n=N, directed = FALSE)
  g = graph_from_edgelist(edgelist, directed = FALSE)
  adj_mat <- get.adjacency(g, sparse = FALSE, type = 'both')
  mat <- as.matrix(adj_mat)
  market_list[[i]] <- mat
}
market_net <- abind(market_list, along=3)

# Generate professional network
prof_list <- list()
for(i in yrs) {
  dname <- paste(c("pro_", args[1], "_20", i, ".dat"), collapse = '')
  edgelist <- as.matrix(read.table(dname, header=F))
  
  g = make_empty_graph(n=85, directed = FALSE)
  g = graph_from_edgelist(edgelist, directed = FALSE)
  adj_mat <- get.adjacency(g, sparse = FALSE, type = 'both')
  mat <- as.matrix(adj_mat)
  prof_list[[i]] <- mat
}
prof_net <- abind(prof_list, along=3)

# Prepare covariates data for RSiena
age <- varCovar(as.matrix(read.table("age_top100_period1.dat")))
ranking <- varCovar(as.matrix(read.table("ranking_top100_period1.dat")))
soloshow <- varCovar(as.matrix(read.table("soloshow_top100_period1.dat")))
award <- varCovar(as.matrix(read.table("award_top100_period1.dat")))
meanprice <- varCovar(as.matrix(read.table("meanprice_top100_period1.dat")))
festbiennal <- varCovar(as.matrix(read.table("festbiennal_top100_period1.dat")))
publicinst <- varCovar(as.matrix(read.table("publicinst_top100_period1.dat")))
countyear <- varCovar(as.matrix(read.table("countyear_top100_period1.dat")))
groupshow <- varCovar(as.matrix(read.table("groupshow_top100_period1.dat")))

# Create a Siena network object with sienaNet()
m_net <- sienaDependent(market_net)
p_net <- sienaDependent(prof_net)

#Data Create
v_list <- sienaDataCreate(m_net, p_net, age, award, meanprice, ranking, soloshow, 
                                    festbiennal, publicinst, countyear, groupshow)

# For the model specification we need to create the effects object
effs <- getEffects(v_list)

