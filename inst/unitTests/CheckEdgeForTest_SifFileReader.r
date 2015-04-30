# Copyright: Copyright 2012 GSK. All rights reserved
#
# *** SVN ***
# LastChanged: 	$Date: 2012-11-12 14:12:28 +0000 (Mon, 12 Nov 2012) $
# Changed By:  	$Author: rild $
# Version:		$Revision: 105 $
# Source:		$HeadURL: file:///L:/7000-7499/7412/SVNrepository/trunk/NetworkFileReader/CheckEdgeForTest_SifFileReader.r $
# 
# *** Summary ***
# Description:  A helper file for the function Test_SifFileReader

CheckEdgeForTest_SifFileReader <- function(edge, vertex1, vertex2){
checkEquals(edge[1],vertex1)&&checkEquals(edge[2], vertex2)
}