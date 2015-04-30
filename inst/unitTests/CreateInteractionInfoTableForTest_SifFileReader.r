# Copyright: Copyright 2012 GSK. All rights reserved
#
# *** SVN ***
# LastChanged: 	$Date: 2013-01-06 21:19:06 +0000 (Sun, 06 Jan 2013) $
# Changed By:  	$Author: rild $
# Version:		$Revision: 389 $
# Source:		$HeadURL: file:///L:/7000-7499/7412/SVNrepository/trunk/NetworkFileReader/CreateInteractionInfoTableForTest_SifFileReader.r $
# 
# *** Summary ***
# Description: Create interactionInfo table for use in the testsuite for CreateCG

CreateInteractionInfoTableForTest_SifFileReader <- function(){
  matrix1 <- matrix(c("Activates", "Inhibits"))
  tab <- as.table(matrix1)
  
  return(tab)
}