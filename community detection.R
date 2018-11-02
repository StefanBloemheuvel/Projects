install.packages('igraph')
install.packages('dplyr')

library(igraph)
library(dplyr)
set.seed(123)
el = read.csv("/Users/stefan/Google Drive/publicatie/FinalDataversion1.csv")
max(el$Total_time)
el1 = select(el,tag_me,tag_them)
g = graph_from_data_frame(el1, directed = FALSE)
g
#plot(g)
V(g) #25
E(g) #606
mean(el$Total_time) #309.6023
max(el$Total_time) #5807
diameter(g) #5
edge_density(g, loops=FALSE) # 2.02
mean(degree(g)) # 48.48
transitivity(g, type=c('undirected'))
average.path.length(g) # 2

simulations = 20
averagecommunities = c()
averagemod = c()
ptm <- proc.time()
for(i in 1:simulations){
  info = cluster_leading_eigen(g2)
  averagecommunities = c(averagecommunities,max(info$membership))
  averagemod = c(averagemod,info$modularity)
}
averagecommunities
proc.time() - ptm
mean(averagecommunities);mean(averagemod)
sd(averagecommunities);sd(averagemod)

# number of communities rounded up, average modularity
#edge betweenness = 6, 0.24
#infomap = 10, 0.059
#label propagation = 4, 0.46
#leading eigenvector = 5, 0.39
#louvain = 4, 0.47
#spinglass = 4, 0.072
#walktrap = 5, 0.15   --------

#centrality measures
mean(edge.betweenness(g)) #0.99
mean(degree(g)) #48.48
mean(closeness(g)) #0.021
mean(betweenness(g)) #12

########################################## NMI ##############################

eb = cluster_edge_betweenness(g2)
im = cluster_infomap(g2)
lp = cluster_label_prop(g2)
le = cluster_leading_eigen(g2)
lv = cluster_louvain(g2)
sg = cluster_spinglass(g2)
#wt = cluster_walktrap(g)

compare(le,lp, method=c('nmi'))



#### graph evolution ####
slice1 <- read.csv("/Users/stefan/Google Drive/publicatie/experiment1slices/slice3.csv")

slice = select(slice1,Source,Target)
g3 = graph_from_data_frame(slice, directed = FALSE)

plot(g3)
V(g3) #25
E(g3) #606
diameter(g3) #5
average.path.length(g3) # 2
max(slice1$Total_time) #5807
mean(slice1$Total_time) #309.6023
edge_density(g3, loops=FALSE) # 2.02
mean(degree(g3)) # 48.48
mean(closeness(g3)) #0.021
mean(betweenness(g3)) #12

############################################ PART 2 ##################


set.seed(123)
el2 = read.csv("/Users/stefan/Google Drive/publicatie/FinalDataversion2.csv")
el3 = select(el2,tag_me,tag_them)
g2 = graph_from_data_frame(el3, directed = FALSE)
g2
#plot(g)
V(g2) #19
E(g2) #1239
mean(el2$Total_time) #535.70
max(el2$Total_time) #5126
diameter(g2) #2
edge_density(g2, loops=FALSE) # 7.25
mean(degree(g2)) # 130.42
transitivity(g2, type=c('undirected')) #0.60
average.path.length(g2) # 1.51

#communities
simulations = 20
averagecommunities = c()
averagemod = c()
for(i in 1:simulations){
  info = cluster_edge_betweenness(g2)
  averagecommunities = c(averagecommunities,max(info$membership))
  averagemod = c(averagemod,info$modularity)
}
mean(averagecommunities);mean(averagemod)

# number of communities rounded up, average modularity
#edge betweenness = 8, 0.03
#infomap = 8, -0.074
#label propagation = 1, 0.00   -----
#leading eigenvector = 3, 0.15
#louvain = 3, 0.16
#spinglass = 3, 0.029
#walktrap = 19, -0.109

#centrality measures
mean(edge.betweenness(g2)) #0.20
mean(degree(g2)) #130.42
mean(closeness(g2)) #0.037
mean(betweenness(g2)) #4.58
testje2 = evcent(g2)
mean(testje2$vector)

################# graph evolution #################
slice1 <- read.csv("/Users/stefan/Google Drive/publicatie/experiment1slices/epxeriment2slices/slice3.csv")

slice = select(slice1,Source,Target)
g3 = graph_from_data_frame(slice, directed = FALSE)
g3
plot(g3)
V(g3) #25
E(g3) #606
diameter(g3) #5
average.path.length(g3) # 2
max(slice1$Total_time) #5807
mean(slice1$Total_time) #309.6023
edge_density(g3, loops=FALSE) # 2.02
mean(degree(g3)) # 48.48
mean(closeness(g3)) #0.021
mean(betweenness(g3)) #12

#####################################
############### PLOTS ###############
#####################################

#agregated contacts plot

data = el
max(data$Total_time)
datatotal1 = select(data,Total_time)
datatotal1 = sort(datatotal1$Total_time, decreasing = TRUE)

data2 = el2
max(data2$Total_time)
datatotal2 = select(data2,Total_time)
datatotal2 = sort(datatotal2$Total_time, decreasing = TRUE)

par(pty='s')
plot(datatotal2,type='l',main='Aggregated Contacts', lty=1
     , col='red', ylim = c(20,10000), xlab = 'Number of interactions',
     ylab='Total time of contact', log='xy')
lines(datatotal1, type = 'l', col='blue')
legend('bottomleft', inset=.05,cex=0.75,title='Legend', c('Event 2','Event 1'),lty=c(1,1),lwd=c(1,1),col=c('red','blue'))

