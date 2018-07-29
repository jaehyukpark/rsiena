library(igraph)
m2011 <- read.table("market_2011.dat",header=F)
m2012 <- read.table("market_2012.dat",header=F)
m2013 <- read.table("market_2013.dat",header=F)
m2014 <- read.table("market_2014.dat",header=F)
m2015 <- read.table("market_2015.dat",header=F)

p2011 <- read.table("pro_2006.dat",header=F)
p2012 <- read.table("pro_2007.dat",header=F)
p2013 <- read.table("pro_2008.dat",header=F)
p2014 <- read.table("pro_2009.dat",header=F)
p2015 <- read.table("pro_2010.dat",header=F)

age3 <- as.matrix(read.table("age_period3.dat"))
ranking3 <- as.matrix(read.table("ranking_period3.dat"))
soloshow3 <- as.matrix(read.table("soloshow_period3.dat"))
award3 <- as.matrix(read.table("award_period3.dat"))
meanprice3 <- as.matrix(read.table("meanprice_period3.dat"))
painting3 <- as.matrix(read.table("painting_period3.dat"))
festbiennal3 <- as.matrix(read.table("festbiennal_period3.dat"))
publicinst3 <- as.matrix(read.table("publicinst_period3.dat"))
countyear3 <- as.matrix(read.table("countyear_period3.dat"))
groupshow3 <- as.matrix(read.table("groupshow_period3.dat"))


#To get the adjacency matrix from the network(unweighted; Siena does not allow weighted matrix)
m2011_data=graph.data.frame(m2011, directed=F)
m2011_adj=as.matrix(as_adjacency_matrix(m2011_data,attr=NULL, sparse=T))
m2012_data=graph.data.frame(m2012, directed=F)
m2012_adj=as.matrix(as_adjacency_matrix(m2012_data,attr=NULL, sparse=T))
m2013_data=graph.data.frame(m2013, directed=F)
m2013_adj=as.matrix(as_adjacency_matrix(m2013_data,attr=NULL, sparse=T))
m2014_data=graph.data.frame(m2014, directed=F)
m2014_adj=as.matrix(as_adjacency_matrix(m2014_data,attr=NULL, sparse=T))
m2015_data=graph.data.frame(m2015, directed=F)
m2015_adj=as.matrix(as_adjacency_matrix(m2015_data,attr=NULL, sparse=T))

p2011_data=graph.data.frame(p2011, directed=F)
p2011_adj=as.matrix(as_adjacency_matrix(p2011_data,attr=NULL, sparse=T))
p2012_data=graph.data.frame(p2012, directed=F)
p2012_adj=as.matrix(as_adjacency_matrix(p2012_data,attr=NULL, sparse=T))
p2013_data=graph.data.frame(p2013, directed=F)
p2013_adj=as.matrix(as_adjacency_matrix(p2013_data,attr=NULL, sparse=T))
p2014_data=graph.data.frame(p2014, directed=F)
p2014_adj=as.matrix(as_adjacency_matrix(p2014_data,attr=NULL, sparse=T))
p2015_data=graph.data.frame(p2015, directed=F)
p2015_adj=as.matrix(as_adjacency_matrix(p2015_data,attr=NULL, sparse=T))


# Put network data in an array with dimensions N x N x T where:
# N is the number of nodes in the network (here, # of artists 1000) 
# and T is the number of time points/kind of network (here 2)
marketnet1 <-array(c(m2011_adj, m2012_adj, m2013_adj, m2014_adj, m2015_adj), dim=c(672,672,5))
pronet1 <-array(c(p2011_adj, p2012_adj, p2013_adj, p2014_adj, p2015_adj), dim=c(672,672,5))

# Prepare covariates data for RSiena
library(RSiena)
age3.vc <- varCovar(age3)
ranking3.vc <- varCovar(ranking3)
soloshow3.vc <- varCovar(soloshow3)
award3.vc <- varCovar(award3)
meanprice3.vc <- varCovar(meanprice3)
festbiennal3.vc <- varCovar(festbiennal3)
publicinst3.vc <- varCovar(publicinst3)
countyear3.vc <- varCovar(countyear3)
groupshow3.vc <- varCovar(groupshow3)

# Create a Siena network object with sienaNet()
marketnetwork3 <- sienaDependent(marketnet3)
pronetwork3 <- sienaDependent(pronet3)

#Data Create
multidata_trial3 <- sienaDataCreate(marketnetwork3, pronetwork3, age3.vc, award3.vc, meanprice3.vc,ranking3.vc, soloshow3.vc, festbiennal3.vc, publicinst3.vc, countyear3.vc, groupshow3.vc)

#get an outline of the data set with some basic descriptives from
print01Report(multidata_trial3)

####   Running the model and looking at results                           ####
####                                                                      ####

# For the model specification we need to create the effects object
multieff3 <- getEffects(multidata_trial3)

#include other within-network structural effects

multieff3 <- includeEffects(multieff3, between, name="marketnetwork3")
multieff3 <- includeEffects(multieff3, between, name="pronetwork3")

#adding between-networks(multiple networks) effects
multieff3 <- includeEffects(multieff3, crprod, name="marketnetwork3",interaction1="pronetwork3")
multieff3 <- includeEffects(multieff3, crprod, name="pronetwork3",interaction1="marketnetwork3")

#adding covariates
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="age3.vc", name="marketnetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="countyear3.vc", name="marketnetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="ranking3.vc", name="marketnetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="meanprice3.vc", name="marketnetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="award3.vc", name="marketnetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="soloshow3.vc", name="marketnetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="festbiennal3.vc", name="marketnetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="publicinst3.vc", name="marketnetwork3")

multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="age3.vc", name="pronetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="groupshow3.vc", name="pronetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="ranking3.vc", name="pronetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="meanprice3.vc", name="pronetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="award3.vc", name="pronetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="soloshow3.vc", name="pronetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="festbiennal3.vc", name="pronetwork3")
multieff3 <- includeEffects(multieff3, egoPlusAltX, simX, interaction1="publicinst3.vc", name="pronetwork3")

#model running

multiAlg3 <- sienaAlgorithmCreate(projname = 'two_art_networks3', n3=500, nsub=3)
result_period3full_between <- siena07(multiAlg3, data = multidata_trial3, nbrNodes=20, effects = multieff3)
xtable(result_period3full_between, file="result_period3full_between.htm", type="html")


