# Copyright: Copyright 2012 GSK. All rights reserved
#
# *** SVN ***
# LastChanged: 	$Date: 2013-01-03 13:22:53 +0000 (Thu, 03 Jan 2013) $
# Changed By:  	$Author: MISC $
# Version:		$Revision: 357 $
# Source:		$HeadURL: file:///L:/7000-7499/7412/SVNrepository/trunk/NetworkPredictions/CreateSimpleNetworkForTest_ComputationalCausalGraphCreator.r $
# 
# *** Summary ***
# Description: A helper function for Test_ComputationalCausalGraphCreator. It gets a simple network for the test

CreateSimpleNetworkForTest_ComputationalCausalGraphCreator <- function(){
network <- graph.formula( "node0" -+ "node1", "node0" -+ "node2")
network <- set.edge.attribute(network,"Weight", 1, 1)
network <- set.edge.attribute(network,"Weight", 2, -1)
network <- set.vertex.attribute(network,"ID", 1:3, 1:3)

return(network)
}