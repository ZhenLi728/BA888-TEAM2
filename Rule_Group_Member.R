#library package
library(tidyverse)
library(arules)
library(arulesViz)
library(ggplot2)

######################### for group table
#choose useful columns
group_01 =group_c1 %>%
  select(group_name,group_id,category.name,category_id,state,members)

#separate by different states(NY,IL,CA)
groupNY = group_01 %>%   
  filter(state=='NY')

groupIL = group_01 %>%   
  filter(state=='IL')

groupCA = group_01%>%
  filter(state=='CA')

######################## for members table
#choose useful columns
member_01 = member_c1%>%
  select(member_id,member_name,state,group_id)

#spreate by different states(NY,IL,CA)
memberNY = member_01%>%
  filter(state == 'NY')

memberIL = member_01%>%
  filter(state == 'IL')

memberCA = member_01%>%
  filter(state == 'CA')

##########################  Inner join member and group
mmgrNY = inner_join(groupNY,memberNY,by='group_id')
mmgrNY$state.x <- NULL

mmgrNY%>%
  count(group_name,sort = TRUE)%>%
  top_n(15)  # business,tech

mmgrIL = inner_join(groupIL,memberIL,by='group_id')
mmgrIL$state.x <- NULL

mmgrIL%>%
  count(group_name,sort = TRUE)%>%
  top_n(15) # network, social events

mmgrCA = inner_join(groupCA,memberCA,by='group_id')
mmgrCA$state.x <- NULL

mmgrCA%>%
  count(group_name,sort = TRUE)%>%
  top_n(15)  # tech, art

########################## Creating association rule
#### NY
mmgrNY = as.data.frame(mmgrNY)
list_mmgrNY = split(mmgrNY[,"group_name"], mmgrNY[,"member_name"])
#how many transcations?
mmgrNY_tr = as(list_mmgrNY, "transactions")
#set rules
rules_mgNY <- apriori(mmgrNY_tr, 
                  parameter = list(supp = 0.003, 
                                   conf = 0.2, 
                                   minlen = 2,
                                   target = "rules"))
summary(rules_mgNY)

ruleExplorer(rules_mgNY)

#### IL
mmgrIL = as.data.frame(mmgrIL)
list_mmgrIL = split(mmgrIL[,"group_name"], mmgrIL[,"member_name"])
#how many transcations?
mmgrIL_tr = as(list_mmgrIL, "transactions")
#set rules
rules_mgIL <- apriori(mmgrIL_tr, 
                      parameter = list(supp = 0.003, 
                                       conf = 0.2, 
                                       minlen = 2,
                                       target = "rules"))
summary(rules_mgIL)

ruleExplorer(rules_mgIL)

#### CA
mmgrCA = as.data.frame(mmgrCA)
list_mmgrCA = split(mmgrCA[,"group_name"], mmgrCA[,"member_name"])
#how many transcations?
mmgrCA_tr = as(list_mmgrCA, "transactions")
#set rules
rules_mgCA <- apriori(mmgrCA_tr, 
                      parameter = list(supp = 0.003, 
                                       conf = 0.2, 
                                       minlen = 2,
                                       target = "rules"))
summary(rules_mgCA)

ruleExplorer(rules_mgCA)


############# Pruning Redundnt Rules
rules_mgNY.sorted = sort(rules_mgNY, by="lift")
inspect(rules_mgNY.sorted)

subset_mgNY.matrix <- is.subset(rules_mgNY.sorted,rules_mgNY.sorted)
subset_mgNY.matrix

subset_mgNY.matrix[lower.tri(subset_mgNY.matrix, diag=T)] <- F
subset_mgNY.matrix

redundant_mgNY <- apply(subset_mgNY.matrix, 2, any)
redundant_mgNY

rules_mgNY.pruned <- rules_mgNY.sorted[!redundant_mgNY]
inspect(rules_mgNY.pruned)

ruleExplorer(rules_mgNY.pruned)






