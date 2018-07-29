library(igraph)
m2001 <- read.table("market_2001.dat",header=F)
m2002 <- read.table("market_2002.dat",header=F)
m2003 <- read.table("market_2003.dat",header=F)
m2004 <- read.table("market_2004.dat",header=F)
m2005 <- read.table("market_2005.dat",header=F)

p2001 <- read.table("pro_2001.dat",header=F)
p2002 <- read.table("pro_2002.dat",header=F)
p2003 <- read.table("pro_2003.dat",header=F)
p2004 <- read.table("pro_2004.dat",header=F)
p2005 <- read.table("pro_2005.dat",header=F)

age1 <- as.matrix(read.table("age_period1.dat"))
ranking1 <- as.matrix(read.table("ranking_period1.dat"))
soloshow1 <- as.matrix(read.table("soloshow_period1.dat"))
award1 <- as.matrix(read.table("award_period1.dat"))
meanprice1 <- as.matrix(read.table("meanprice_period1.dat"))
#painting1 <- as.matrix(read.table("painting_period1.dat"))
festbiennal1 <- as.matrix(read.table("festbiennal_period1.dat"))
publicinst1 <- as.matrix(read.table("publicinst_period1.dat"))
countyear1 <- as.matrix(read.table("countyear_period1.dat"))
groupshow1 <- as.matrix(read.table("groupshow_period1.dat"))


#To get the adjacency matrix from the network(unweighted; Siena does not allow weighted matrix)
m2001_data=graph.data.frame(m2001, directed=F)
m2001_adj=as.matrix(as_adjacency_matrix(m2001_data,attr=NULL, sparse=T))
m2002_data=graph.data.frame(m2002, directed=F)
m2002_adj=as.matrix(as_adjacency_matrix(m2002_data,attr=NULL, sparse=T))
m2003_data=graph.data.frame(m2003, directed=F)
m2003_adj=as.matrix(as_adjacency_matrix(m2003_data,attr=NULL, sparse=T))
m2004_data=graph.data.frame(m2004, directed=F)
m2004_adj=as.matrix(as_adjacency_matrix(m2004_data,attr=NULL, sparse=T))
m2005_data=graph.data.frame(m2005, directed=F)
m2005_adj=as.matrix(as_adjacency_matrix(m2005_data,attr=NULL, sparse=T))

p2001_data=graph.data.frame(p2001, directed=F)
p2001_adj=as.matrix(as_adjacency_matrix(p2001_data,attr=NULL, sparse=T))
p2002_data=graph.data.frame(p2002, directed=F)
p2002_adj=as.matrix(as_adjacency_matrix(p2002_data,attr=NULL, sparse=T))
p2003_data=graph.data.frame(p2003, directed=F)
p2003_adj=as.matrix(as_adjacency_matrix(p2003_data,attr=NULL, sparse=T))
p2004_data=graph.data.frame(p2004, directed=F)
p2004_adj=as.matrix(as_adjacency_matrix(p2004_data,attr=NULL, sparse=T))
p2005_data=graph.data.frame(p2005, directed=F)
p2005_adj=as.matrix(as_adjacency_matrix(p2005_data,attr=NULL, sparse=T))


# Put network data in an array with dimensions N x N x T where:
# N is the number of nodes in the network (here, # of artists 1000) 
# and T is the number of time points/kind of network (here 2)
marketnet1 <-array(c(m2001_adj, m2002_adj, m2003_adj, m2004_adj, m2005_adj), dim=c(363,363,5))
pronet1 <-array(c(p2001_adj, p2002_adj, p2003_adj, p2004_adj, p2005_adj), dim=c(363,363,5))

# Prepare covariates data for RSiena
library(RSiena)
age1.vc <- varCovar(age1)
ranking1.vc <- varCovar(ranking1)
soloshow1.vc <- varCovar(soloshow1)
award1.vc <- varCovar(award1)
meanprice1.vc <- varCovar(meanprice1)
festbiennal1.vc <- varCovar(festbiennal1)
publicinst1.vc <- varCovar(publicinst1)
countyear1.vc <- varCovar(countyear1)
groupshow1.vc <- varCovar(groupshow1)

# Create a Siena network object with sienaNet()
marketnetwork1 <- sienaDependent(marketnet1)
pronetwork1 <- sienaDependent(pronet1)

#Data Create
multidata_trial1 <- sienaDataCreate(marketnetwork1, pronetwork1)#, age1.vc, award1.vc, meanprice1.vc,ranking1.vc, soloshow1.vc, festbiennal1.vc, publicinst1.vc, countyear1.vc, groupshow1.vc)

#get an outline of the data set with some basic descriptives from
print01Report(multidata_trial1)

####   Running the model and looking at results                           ####
####                                                                      ####

# For the model specification we need to create the effects object
multieff1 <- getEffects(multidata_trial1)

#include other within-network structural effects

multieff1 <- includeEffects(multieff1, between, name="marketnetwork1")
multieff1 <- includeEffects(multieff1, between, name="pronetwork1")

#adding between-networks(multiple networks) effects
multieff1 <- includeEffects(multieff1, crprod, name="marketnetwork1",interaction1="pronetwork1")
multieff1 <- includeEffects(multieff1, crprod, name="pronetwork1",interaction1="marketnetwork1")

#adding covariates
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="age1.vc", name="marketnetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="countyear1.vc", name="marketnetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="ranking1.vc", name="marketnetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="meanprice1.vc", name="marketnetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="award1.vc", name="marketnetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="soloshow1.vc", name="marketnetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="festbiennal1.vc", name="marketnetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="publicinst1.vc", name="marketnetwork1")

multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="age1.vc", name="pronetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="groupshow1.vc", name="pronetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="ranking1.vc", name="pronetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="meanprice1.vc", name="pronetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="award1.vc", name="pronetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="soloshow1.vc", name="pronetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="festbiennal1.vc", name="pronetwork1")
multieff1 <- includeEffects(multieff1, egoPlusAltX, simX, interaction1="publicinst1.vc", name="pronetwork1")

#model running

multiAlg1 <- sienaAlgorithmCreate(projname = 'two_art_networks1', n3=500, nsub=3)
result_period1full_between <- siena07(multiAlg1, data = multidata_trial1, nbrNodes=4, 
                                      useCluster = TRUE, initC = TRUE, effects = multieff1)
xtable(result_period1full_between, file="result_period1full_between.htm", type="html")


