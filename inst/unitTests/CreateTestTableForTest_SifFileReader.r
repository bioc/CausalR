# Copyright: Copyright 2012 GSK. All rights reserved
#
# *** SVN ***
# LastChanged: 	$Date: 2013-01-06 21:19:06 +0000 (Sun, 06 Jan 2013) $
# Changed By:  	$Author: rild $
# Version:		$Revision: 389 $
# Source:		$HeadURL: file:///L:/7000-7499/7412/SVNrepository/trunk/NetworkFileReader/CreateTestTableForTest_SifFileReader.r $
# 
# *** Summary ***
# Description: Create a table to be used in the testsuite for CreateCG

CreateTestTableForTest_SifFileReader <- function(){
  matrix1 <- matrix(c("node0","node0","Activates","Inhibits","node1","node2"), nrow =2)
  tab <- as.table(matrix1)
  
  return(tab)
}