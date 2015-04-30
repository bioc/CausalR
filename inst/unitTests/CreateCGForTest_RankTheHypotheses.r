CreateCGForTest_RankTheHypotheses <- function(){

# Copyright: Copyright 2012 GSK. All rights reserved
#
# *** SVN ***
# LastChanged: 	$Date: 2013-01-16 10:51:07 +0000 (Wed, 16 Jan 2013) $
# Changed By:  	$Author: MISC $
# Version:		$Revision: 429 $
# Source:		$HeadURL: file:///L:/7000-7499/7412/SVNrepository/trunk/RankingHypotheses/CreateCGForTest_RankTheHypotheses.r $
# 
# *** Summary ***
# Description: A helper function that will create the CG for the purpose of testing in RankTheHypotheses

  # Create test_network
  test_network <- graph.formula( "node0" -+ "node1", "node0" -+ "node2")
  test_network <- set.edge.attribute(test_network,"Interaction", 1, "Activates")
  test_network <- set.edge.attribute(test_network,"Interaction", 2, "Inhibits")
  test_network <- set.edge.attribute(test_network,"Weight", 1, 1)
  test_network <- set.edge.attribute(test_network,"Weight", 2, -1)
  test_network <- set.vertex.attribute(test_network,"ID", 1:3, 1:3)
  test_network$isCCG <- FALSE
  
  return(test_network)
}