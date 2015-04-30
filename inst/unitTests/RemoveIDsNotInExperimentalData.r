RemoveIDsNotInExperimentalData <- function(connectedNodes, nodesInExperimentalData){

# Copyright: Copyright 2012 GSK. All rights reserved
#
# *** SVN ***
# LastChanged: 	$Date: 2013-01-03 14:30:21 +0000 (Thu, 03 Jan 2013) $
# Changed By:  	$Author: MISC $
# Version:		$Revision: 361 $
# Source:		$HeadURL: file:///L:/7000-7499/7412/SVNrepository/trunk/NetworkPredictions/RemoveIDsNotInExperimentalData.r $
# 
# *** Summary ***
# Description: Takes in a list of connected nodes and removes
# those not in the experimental data.

connectedNodesInExperimentalData <- intersect(connectedNodes, nodesInExperimentalData)
return(connectedNodesInExperimentalData)
}