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
else {
    cat <- args[1]
    period <- args[2]
}

# Set working directory
dpath <- paste(c('~/GitRepo/artist_network/rsiena/', cat, '_unique_id/p', period),
               collapse='')
setwd(dpath)

# Setting depending on argument values
# Top or All
if (args[1]=='top') {N = 100} 
else {N = 1000}

# Period
if (args[2]==1){yrs=c('2001', '2002', '2003', '2004', '2005')}
else if (args[2]==2){yrs=c('2006', '2007', '2008', '2009', '2010')}
else {yrs=c('2011', '2012', '2013', '2014', '2015')}


# Data Prep ------------------------------------------------------------------------
# Generate market network
mk_list <- list()
for(yr in yrs) {
  dname <- paste(c("market_", cat "_", yr, ".dat"), collapse = '')
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
for(yr in yrs) {
  dname <- paste(c("pro_", cat "_", yr, ".dat"), collapse = '')
  edgelist <- as.matrix(read.table(dname, header=F))
  
  g = make_empty_graph(n=N, directed = FALSE)
  g = graph_from_edgelist(edgelist, directed = FALSE)
  adj_mat <- get.adjacency(g, sparse = FALSE, type = 'both')
  mat <- as.matrix(adj_mat)
  prof_list[[i]] <- mat
}
prof_net <- abind(prof_list, along=3)

# Siena ------------------------------------------------------------------------
# Prepare covariates data for RSiena
generate_var <- function(varname){
    fname <- paste(c(varname, "_", cat, ".dat"), collapse = '')
    return varCovar(as.matrix(read.table(fname)))
}
age <- generate_var("age")
ranking <- generate_var("ranking")
soloshow <- generate_var("soloshow")
award <- generate_var("award")
meanprice <- generate_var("meanprice")
festbiennal <- generate_var("festbiennal")
publicinst <- generate_var("publicinst")
countyear <- generate_var("countyear")
groupshow <- generate_var("groupshow")

# Create a Siena network object with sienaNet()
m_net <- sienaDependent(market_net)
p_net <- sienaDependent(prof_net)

#Data Create
v_list <- sienaDataCreate(m_net, p_net, age, award, meanprice, ranking, soloshow, 
                          festbiennal, publicinst, countyear, groupshow)
print01Report(v_list)

# For the model specification we need to create the effects object
effs <- getEffects(v_list)

# Adding between-networks(multiple networks) effects
effs <- includeEffects(effs, crprod, name='m_net', interacton='p_net')
effs <- includeEffects(effs, crprod, name='p_net', interacton='m_net')

# Adding covariates
for (net in c("m_net", "p_net")) {
    for (covar in c("age", "ranking", "soloshow", "award", "meanprice", 
                   "festbiennal", "publicinst", "countyear", "groupshow")) {
       effs <- includeEffects(effs, egoPlusAltX, simX, interaction1=covar, name=net)
    }
 }

# Running Siena -----------------------------------------------------------
prj_name = paste(c('siena_',cat,'_',period), collapse='')
siena_model <- sienaAlgorithmCreate(projname=prj_name, n3=1000, nsub=3)
siena_result <- siena07(siena_model, data=v_list, useClust=TRUE, nbrNodes=28,
                        initC=TRUE, effects=effs)
xtable(siena_result, file=paste(c(prj_name, '.html'), collapse=''), type='html')
