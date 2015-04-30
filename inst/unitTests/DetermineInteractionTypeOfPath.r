# Copyright: Copyright 2012 GSK. All rights reserved
#
# *** SVN ***
# LastChanged: 	$Date: 2013-01-02 11:21:09 +0000 (Wed, 02 Jan 2013) $
# Changed By:  	$Author: MISC $
# Version:		$Revision: 351 $
# Source:		$HeadURL: file:///L:/7000-7499/7412/SVNrepository/trunk/NetworkPredictions/DetermineInteractionTypeOfPath.r $
# 
# *** Summary ***
# Description: Given a path and through the network, this function will determine if the path results in activation or inhibition.
# Activation is indicated by 1, inhibition by -1 

DetermineInteractionTypeOfPath <- function(network, nodesInPath){
# network must be an igraph
# nodesInPath is an ordered list of the nodes visited on the path - note that these contain numbers which use R's internal reference to the edges

numEdges <- length(nodesInPath[[1]])-1
# Declare a variable that will store the numerical references for the edges in a path
nodeEdges <- matrix(0,1,numEdges)

for (i in 1:numEdges){
	# Get the R's edge number for two successive nodes in nodesInPath
	nodeEdges[i] <- E(network)[nodesInPath[[1]][i]%->%nodesInPath[[1]][i+1]][[1]]
}
# Take the product of the weights for each edge in the path to get the overall interaction
netPathInteraction = prod(get.edge.attribute(network, "Weight", nodeEdges))
	
return(netPathInteraction)
}


