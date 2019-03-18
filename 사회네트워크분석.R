library(sna)
library(dplyr)
library(ggplot2)
#library(igraph)
#detach("package:igraph", unload=TRUE)

###################################
### 커뮤니티 만들기 
###################################

## 평일 출근
nonred_chulgeun_mat <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/matrix/vol2_nonred_chulgeun_mat_1482x1482.csv'
                                , header = FALSE)
nonred_chulgeun_mat[is.na(nonred_chulgeun_mat)] <- 0

ID <- as.integer(as.character(nonred_chulgeun_mat$V1[-1]))

temp_nonred_chulgeun <- nonred_chulgeun_mat[-1,]
temp_nonred_chulgeun <- temp_nonred_chulgeun[,-1]
colnames(temp_nonred_chulgeun) <- ID
rownames(temp_nonred_chulgeun) <- ID

graph_nonred_chulgeun <- graph_from_adjacency_matrix(as.matrix(temp_nonred_chulgeun), mode = "directed",
                                                     weighted = TRUE , diag = TRUE)
#graph_nonred_chulgeun
walktrap_nonred_chulgeun <- walktrap.community(graph_nonred_chulgeun,
                                               weight = E(graph_nonred_chulgeun)$weight,
                                               merges = T, modularity = T)

nonred_chulgeun_group <- cbind(ID,walktrap_nonred_chulgeun$membership)
colnames(nonred_chulgeun_group) <- c("ID", "Membership")

# write.table(cbind(ID,walktrap_nonred_chulgeun$membership), "C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/sibal.csv", 
#            sep = ",", 
#            row.names = FALSE,
#            quote = FALSE,
#            append = FALSE,
#            na = 'NA')



#test <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/test.csv'
#         , header = TRUE)

# suffle <- cbind(seq(1:22),sample(1:22, 22, replace=F))
# colnames(suffle) <- c("membership", "NC")

# drr <- merge(test, suffle, by= 'membership')
# drr$NC <- as.factor(drr$NC)
# g <- ggplot(data = drr, aes(x=longitude, y=latitude))
# g + geom_point(aes(color = membership))
  
## 평일 낮 
nonred_nat_mat <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/matrix/vol2_nonred_nat_mat_1487x1487.csv'
                           , header = FALSE)
nonred_nat_mat[is.na(nonred_nat_mat)] <- 0

ID <- as.integer(as.character(nonred_nat_mat$V1[-1]))

temp_nonred_nat <- nonred_nat_mat[-1,]
temp_nonred_nat <- temp_nonred_nat[,-1]
ID <- as.integer(as.character(nonred_nat_mat$V1[-1]))

temp_nonred_nat <- nonred_nat_mat[-1,]
temp_nonred_nat <- temp_nonred_nat[,-1]

colnames(temp_nonred_nat) <- ID
rownames(temp_nonred_nat) <- ID

graph_nonred_nat <- graph_from_adjacency_matrix(as.matrix(temp_nonred_nat), mode = "directed",
                                                     weighted = TRUE , diag = TRUE)

walktrap_nonred_nat <- walktrap.community(graph_nonred_nat,
                                               weight = E(graph_nonred_nat)$weight,
                                               merges = T, modularity = T)

nonred_nat_group <- cbind(ID,walktrap_nonred_nat$membership)
colnames(nonred_nat_group) <- c("ID", "Membership")

## 평일 퇴근 

nonred_toegeun_mat <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/matrix/vol2_nonred_toegeun_mat_1487x1487.csv'
                               , header = FALSE)
nonred_toegeun_mat[is.na(nonred_toegeun_mat)] <- 0

ID <- as.integer(as.character(nonred_toegeun_mat$V1[-1]))

temp_nonred_toegeun <- nonred_toegeun_mat[-1,]
temp_nonred_toegeun <- temp_nonred_toegeun[,-1]
ID <- as.integer(as.character(nonred_toegeun_mat$V1[-1]))

temp_nonred_toegeun <- nonred_toegeun_mat[-1,]
temp_nonred_toegeun <- temp_nonred_toegeun[,-1]

colnames(temp_nonred_toegeun) <- ID
rownames(temp_nonred_toegeun) <- ID

graph_nonred_toegeun <- graph_from_adjacency_matrix(as.matrix(temp_nonred_toegeun), mode = "directed",
                                                     weighted = TRUE , diag = TRUE)

walktrap_nonred_toegeun <- walktrap.community(graph_nonred_toegeun,
                                               weight = E(graph_nonred_toegeun)$weight,
                                               merges = T, modularity = T)

nonred_toegeun_group <- cbind(ID,walktrap_nonred_toegeun$membership)
colnames(nonred_toegeun_group) <- c("ID", "Membership")

## 평일 밤 
nonred_bam_mat <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/matrix/vol2_nonred_bam_mat_1484x1484.csv'
                           , header = FALSE)
nonred_bam_mat[is.na(nonred_bam_mat)] <- 0

ID <- as.integer(as.character(nonred_bam_mat$V1[-1]))

temp_nonred_bam <- nonred_bam_mat[-1,]
temp_nonred_bam <- temp_nonred_bam[,-1]
ID <- as.integer(as.character(nonred_bam_mat$V1[-1]))

temp_nonred_bam <- nonred_bam_mat[-1,]
temp_nonred_bam <- temp_nonred_bam[,-1]

colnames(temp_nonred_bam) <- ID
rownames(temp_nonred_bam) <- ID

graph_nonred_bam <- graph_from_adjacency_matrix(as.matrix(temp_nonred_bam), mode = "directed",
                                                     weighted = TRUE , diag = TRUE)

walktrap_nonred_bam <- walktrap.community(graph_nonred_bam,
                                               weight = E(graph_nonred_bam)$weight,
                                               merges = T, modularity = T)

nonred_bam_group <- cbind(ID,walktrap_nonred_bam$membership)
colnames(nonred_bam_group) <- c("ID", "Membership")

## 평일 심야 
nonred_simya_mat <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/matrix/vol2_nonred_simya_mat_1481x1481.csv'
                             , header = FALSE)
nonred_simya_mat[is.na(nonred_simya_mat)] <- 0

ID <- as.integer(as.character(nonred_simya_mat$V1[-1]))

temp_nonred_simya <- nonred_simya_mat[-1,]
temp_nonred_simya <- temp_nonred_simya[,-1]
ID <- as.integer(as.character(nonred_simya_mat$V1[-1]))

temp_nonred_simya <- nonred_simya_mat[-1,]
temp_nonred_simya <- temp_nonred_simya[,-1]

colnames(temp_nonred_simya) <- ID
rownames(temp_nonred_simya) <- ID

graph_nonred_simya <- graph_from_adjacency_matrix(as.matrix(temp_nonred_simya), mode = "directed",
                                                     weighted = TRUE , diag = TRUE)

walktrap_nonred_simya <- walktrap.community(graph_nonred_simya,
                                               weight = E(graph_nonred_simya)$weight,
                                               merges = T, modularity = T)

nonred_simya_group <- cbind(ID,walktrap_nonred_simya$membership)
colnames(nonred_simya_group) <- c("ID", "Membership")

###########################

## 빨간날 출근
red_chulgeun_mat <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/matrix/vol2_red_chulgeun_mat_1463x1463.csv'
                             , header = FALSE)
red_chulgeun_mat[is.na(red_chulgeun_mat)] <- 0

ID <- as.integer(as.character(red_chulgeun_mat$V1[-1]))


ID <- as.integer(as.character(red_chulgeun_mat$V1[-1]))

temp_red_chulgeun <- red_chulgeun_mat[-1,]
temp_red_chulgeun <- temp_red_chulgeun[,-1]

colnames(temp_red_chulgeun) <- ID
rownames(temp_red_chulgeun) <- ID

graph_red_chulgeun <- graph_from_adjacency_matrix(as.matrix(temp_red_chulgeun), mode = "directed",
                                                     weighted = TRUE , diag = TRUE)

walktrap_red_chulgeun <- walktrap.community(graph_red_chulgeun,
                                               weight = E(graph_red_chulgeun)$weight,
                                               merges = T, modularity = T)

red_chulgeun_group <- cbind(ID,walktrap_red_chulgeun$membership)
colnames(red_chulgeun_group) <- c("ID", "Membership")



## 빨간날 낮 
red_nat_mat <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/matrix/vol2_red_nat_mat_1487x1487.csv'
                        , header = FALSE)
red_nat_mat[is.na(red_nat_mat)] <- 0

ID <- as.integer(as.character(red_nat_mat$V1[-1]))

temp_red_nat <- red_nat_mat[-1,]
temp_red_nat <- temp_red_nat[,-1]
ID <- as.integer(as.character(red_nat_mat$V1[-1]))

temp_red_nat <- red_nat_mat[-1,]
temp_red_nat <- temp_red_nat[,-1]

colnames(temp_red_nat) <- ID
rownames(temp_red_nat) <- ID

graph_red_nat <- graph_from_adjacency_matrix(as.matrix(temp_red_nat), mode = "directed",
                                                weighted = TRUE , diag = TRUE)

walktrap_red_nat <- walktrap.community(graph_red_nat,
                                          weight = E(graph_red_nat)$weight,
                                          merges = T, modularity = T)

red_nat_group <- cbind(ID,walktrap_red_nat$membership)
colnames(red_nat_group) <- c("ID", "Membership")

## 빨간날 퇴근 
red_toegeun_mat <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/matrix/vol2_red_toegeun_mat_1476x1476.csv'
                            , header = FALSE)
red_toegeun_mat[is.na(red_toegeun_mat)] <- 0
ID <- as.integer(as.character(red_toegeun_mat$V1[-1]))

temp_red_toegeun <- red_toegeun_mat[-1,]
temp_red_toegeun <- temp_red_toegeun[,-1]
ID <- as.integer(as.character(red_toegeun_mat$V1[-1]))

temp_red_toegeun <- red_toegeun_mat[-1,]
temp_red_toegeun <- temp_red_toegeun[,-1]

colnames(temp_red_toegeun) <- ID
rownames(temp_red_toegeun) <- ID

graph_red_toegeun <- graph_from_adjacency_matrix(as.matrix(temp_red_toegeun), mode = "directed",
                                                    weighted = TRUE , diag = TRUE)

walktrap_red_toegeun <- walktrap.community(graph_red_toegeun,
                                              weight = E(graph_red_toegeun)$weight,
                                              merges = T, modularity = T)

red_toegeun_group <- cbind(ID,walktrap_red_toegeun$membership)
colnames(red_toegeun_group) <- c("ID", "Membership")

## 빨간날 밤 
red_bam_mat <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/matrix/vol2_red_bam_mat_1474x1474.csv'
                        , header = FALSE)
red_bam_mat[is.na(red_bam_mat)] <- 0


ID <- as.integer(as.character(red_bam_mat$V1[-1]))

temp_red_bam <- red_bam_mat[-1,]
temp_red_bam <- temp_red_bam[,-1]
ID <- as.integer(as.character(red_bam_mat$V1[-1]))

temp_red_bam <- red_bam_mat[-1,]
temp_red_bam <- temp_red_bam[,-1]

colnames(temp_red_bam) <- ID
rownames(temp_red_bam) <- ID

graph_red_bam <- graph_from_adjacency_matrix(as.matrix(temp_red_bam), mode = "directed",
                                                weighted = TRUE , diag = TRUE)

walktrap_red_bam <- walktrap.community(graph_red_bam,
                                          weight = E(graph_red_bam)$weight,
                                          merges = T, modularity = T)

red_bam_group <- cbind(ID,walktrap_red_bam$membership)
colnames(red_bam_group) <- c("ID", "Membership")

## 빨간날 심야 
red_simya_mat <- read.csv('C:/Users/student/OneDrive/dr/dh_remove_centers/drr_with_timeAndRed_category/matrix/vol2_red_simya_mat_1475x1475.csv'
                          , header = FALSE)
red_simya_mat[is.na(red_simya_mat)] <- 0

ID <- as.integer(as.character(red_simya_mat$V1[-1]))

temp_red_simya <- red_simya_mat[-1,]
temp_red_simya <- temp_red_simya[,-1]
ID <- as.integer(as.character(red_simya_mat$V1[-1]))

temp_red_simya <- red_simya_mat[-1,]
temp_red_simya <- temp_red_simya[,-1]

colnames(temp_red_simya) <- ID
rownames(temp_red_simya) <- ID

graph_red_simya <- graph_from_adjacency_matrix(as.matrix(temp_red_simya), mode = "directed",
                                                  weighted = TRUE , diag = TRUE)

walktrap_red_simya <- walktrap.community(graph_red_simya,
                                            weight = E(graph_red_simya)$weight,
                                            merges = T, modularity = T)

red_simya_group <- cbind(ID,walktrap_red_simya$membership)
colnames(red_simya_group) <- c("ID", "Membership")

