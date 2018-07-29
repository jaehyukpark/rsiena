library(igraph)
m2006 <- read.table("market_2006.dat",header=F)
m2007 <- read.table("market_2007.dat",header=F)
m2008 <- read.table("market_2008.dat",header=F)
m2009 <- read.table("market_2009.dat",header=F)
m2010 <- read.table("market_2010.dat",header=F)

p2006 <- read.table("pro_2006.dat",header=F)
p2007 <- read.table("pro_2007.dat",header=F)
p2008 <- read.table("pro_2008.dat",header=F)
p2009 <- read.table("pro_2009.dat",header=F)
p2010 <- read.table("pro_2010.dat",header=F)

age2 <- as.matrix(read.table("age_period2.dat"))
ranking2 <- as.matrix(read.table("ranking_period2.dat"))
soloshow2 <- as.matrix(read.table("soloshow_period2.dat"))
award2 <- as.matrix(read.table("award_period2.dat"))
meanprice2 <- as.matrix(read.table("meanprice_period2.dat"))
painting2 <- as.matrix(read.table("painting_period2.dat"))
festbiennal2 <- as.matrix(read.table("festbiennal_period2.dat"))
publicinst2 <- as.matrix(read.table("publicinst_period2.dat"))
countyear2 <- as.matrix(read.table("countyear_period2.dat"))
groupshow2 <- as.matrix(read.table("groupshow_period2.dat"))


#To get the adjacency matrix from the network(unweighted; Siena does not allow weighted matrix)
m2006_data=graph.data.frame(m2006, directed=F)
m2006_adj=as.matrix(as_adjacency_matrix(m2006_data,attr=NULL, sparse=T))
m2007_data=graph.data.frame(m2007, directed=F)
m2007_adj=as.matrix(as_adjacency_matrix(m2007_data,attr=NULL, sparse=T))
m2008_data=graph.data.frame(m2008, directed=F)
m2008_adj=as.matrix(as_adjacency_matrix(m2008_data,attr=NULL, sparse=T))
m2009_data=graph.data.frame(m2009, directed=F)
m2009_adj=as.matrix(as_adjacency_matrix(m2009_data,attr=NULL, sparse=T))
m2010_data=graph.data.frame(m2010, directed=F)
m2010_adj=as.matrix(as_adjacency_matrix(m2010_data,attr=NULL, sparse=T))

p2006_data=graph.data.frame(p2006, directed=F)
p2006_adj=as.matrix(as_adjacency_matrix(p2006_data,attr=NULL, sparse=T))
p2007_data=graph.data.frame(p2007, directed=F)
p2007_adj=as.matrix(as_adjacency_matrix(p2007_data,attr=NULL, sparse=T))
p2008_data=graph.data.frame(p2008, directed=F)
p2008_adj=as.matrix(as_adjacency_matrix(p2008_data,attr=NULL, sparse=T))
p2009_data=graph.data.frame(p2009, directed=F)
p2009_adj=as.matrix(as_adjacency_matrix(p2009_data,attr=NULL, sparse=T))
p2010_data=graph.data.frame(p2010, directed=F)
p2010_adj=as.matrix(as_adjacency_matrix(p2010_data,attr=NULL, sparse=T))


# Put network data in an array with dimensions N x N x T where:
# N is the number of nodes in the network (here, # of artists 1000) 
# and T is the number of time points/kind of network (here 2)
marketnet1 <-array(c(m2006_adj, m2007_adj, m2008_adj, m2009_adj, m2010_adj), dim=c(568,568,5))
pronet1 <-array(c(p2006_adj, p2007_adj, p2008_adj, p2009_adj, p2010_adj), dim=c(568,568,5))

# Prepare covariates data for RSiena
library(RSiena)
age2.vc <- varCovar(age2)
ranking2.vc <- varCovar(ranking2)
soloshow2.vc <- varCovar(soloshow2)
award2.vc <- varCovar(award2)
meanprice2.vc <- varCovar(meanprice2)
festbiennal2.vc <- varCovar(festbiennal2)
publicinst2.vc <- varCovar(publicinst2)
countyear2.vc <- varCovar(countyear2)
groupshow2.vc <- varCovar(groupshow2)

# Create a Siena network object with sienaNet()
marketnetwork2 <- sienaDependent(marketnet2)
pronetwork2 <- sienaDependent(pronet2)

#Data Create
multidata_trial2 <- sienaDataCreate(marketnetwork2, pronetwork2, age2.vc, award2.vc, meanprice2.vc,ranking2.vc, soloshow2.vc, festbiennal2.vc, publicinst2.vc, countyear2.vc, groupshow2.vc)

#get an outline of the data set with some basic descriptives from
print01Report(multidata_trial2)

####   Running the model and looking at results                           ####
####                                                                      ####

# For the model specification we need to create the effects object
multieff2 <- getEffects(multidata_trial2)

#adding between-networks(multiple networks) effects
multieff2 <- includeEffects(multieff2, crprod, name="marketnetwork2",interaction1="pronetwork2")
multieff2 <- includeEffects(multieff2, crprod, name="pronetwork2",interaction1="marketnetwork2")

#adding covariates
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="age2.vc", name="marketnetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="countyear2.vc", name="marketnetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="ranking2.vc", name="marketnetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="meanprice2.vc", name="marketnetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="award2.vc", name="marketnetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="soloshow2.vc", name="marketnetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="festbiennal2.vc", name="marketnetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="publicinst2.vc", name="marketnetwork2")

multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="age2.vc", name="pronetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="groupshow2.vc", name="pronetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="ranking2.vc", name="pronetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="meanprice2.vc", name="pronetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="award2.vc", name="pronetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="soloshow2.vc", name="pronetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="festbiennal2.vc", name="pronetwork2")
multieff2 <- includeEffects(multieff2, egoPlusAltX, simX, interaction1="publicinst2.vc", name="pronetwork2")

#model running

multiAlg2 <- sienaAlgorithmCreate(projname = 'two_art_networks2', n3=500, nsub=3)
result_period2full <- siena07(multiAlg2, data = multidata_trial2, nbrNodes=20, effects = multieff2)
xtable(result_period2full, file="result_period2full.htm", type="html")


