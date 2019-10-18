# install.packages('RDStreeboot')


# example from manual -----------------------------------------------------


require(RDStreeboot)

data(faux.network)

## draw RDS from network
set.seed(123);samp <- sample.RDS(faux.network$traits, faux.network$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)

str(samp,1)
## estimate 80% and 95% confidence intervals
samp=samp;B=3
test = treeboot.RDS(samp, c(0.025, 0.10, 0.90, 0.975), 2000)
str(test)
rm(test)

# main sampling function is .TBS()
?treeboot.RDS
RDStreeboot:::.TBS
View(RDStreeboot:::.TBS)


# # level 1 bootstrap -------------------------------------------------------
# B1=3
# id_boot_0 = RDStreeboot:::.TBS(samp, B=B1)
# 
# str(id_boot_0,1)
# 
# id_boot_0[[1]]
# id_boot_0[[3]]
# 
# 
# # level 2 bootstrap -------------------------------------------------------
# 
# # ez idea, for 2nd level bootstrap do classic bootstrap
# # classic := sample ids with replacement, ignore tree structure
# 
# b=1
# 
# B2 = 2
# resamp_2 = vector(mode='list',length=B2)
# for(bb in 1:B2){
#   # id_boot_1=samp_tb[[b]] is the b-th set of ids from the 1st level bootstrap
#   id_boot_1 = id_boot_0[[b]]
#   id_boot_2 = sample(id_boot_1,
#                      size=length(id_boot_1),
#                      replace=TRUE)  
#   resamp_2[[bb]] <- id_boot_2
# }
# 
# str(resamp_2,1)
# 

# better idea, for 2nd level bootstrap
# do another RDStreeboot:::.TBS()
# need to puzzle/figure out how to recreate tree / adjacency
# for each b, id_boot_1_tree = remap_tree(id_boot_0[[b]],...)
# key step is to match descendent ids to ancestor ids

# remap_tree = function(id_boot_desc,table_edges_ances){}

# id_boot_1_tree = remap_tree(id_boot_0[[b]],...)
# id_boot_2_tree = RDStreeboot:::.TBS(id_boot_1_tree, B=B2)


#  ------------------------------------------------------------------------
# above, is general for loop sketch
# below, work out details of one simple b=1 and B=1


source('scripts/remap_tree.R')
# View(remap_tree)

# faux network ex ---------------------------------------------------------

# library(RDStreeboot)
# data(faux.network)
# set.seed(123);samp <- sample.RDS(faux.network$traits, faux.network$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)
# result of sample.RDS(replace=TRUE) allows for non unique nodes

str(samp,1)
## estimate 80% and 95% confidence intervals
samp=samp;B1=5

# confirm edgelist ignores islands
# confirm seed edge do not need NA parents
# confirm nodes makes up for islands
str(samp$edges,1)
str(samp$nodes,1)

set.seed(4321);id_boot_1 = RDStreeboot:::.TBS(samp, B=B1)

str(id_boot_1,1)

tree_info_b1 = remap_tree(id_boot_desc_b=id_boot_1[[1]],
													df_edges_ances_elig = samp$edges)

tree_info_b2 = remap_tree(id_boot_desc_b=id_boot_1[[5]],
													df_edges_ances_elig = samp$edges)

str(tree_info_b1,1)
str(tree_info_b2,1)

# level 2 treebootstrap
B2=5
id_dubboot_from_b1 = RDStreeboot:::.TBS(tree_info_b1, B=B2)
id_dubboot_from_b2 = RDStreeboot:::.TBS(tree_info_b2, B=B2)

str(id_dubboot_from_b1)  # B2=5 re samples from b=1
str(id_dubboot_from_b2)  # B2=5 re samples from b=2

lapply(id_dubboot_from_b1,sort)
lapply(id_dubboot_from_b2,sort)


# what do they look like plot wise, need to know edge list again

tree_info_b1_bb1 = remap_tree(id_boot_desc_b=id_dubboot_from_b1[[1]],
															df_edges_ances_elig = samp$edges)

tree_info_b2_bb1 = remap_tree(id_boot_desc_b=id_dubboot_from_b2[[1]],
															df_edges_ances_elig = samp$edges)


library(igraph)
par(mfrow=c(2,3))

# original data
plot(graph_from_data_frame(d=samp$edges, 
													 directed=TRUE, 
													 vertices=unique(c(samp$edges[,1],
													 									samp$edges[,2],
													 									samp$nodes))),
		 layout=layout_as_tree,
		 vertex.size = 10,
		 edge.arrow.mode=0,
		 edge.arrow.size = 0.01,
		 edge.arrow.width = 0.01)

# level 1 ids resampled from .TBS(ids_original)
plot(graph_from_data_frame(d=tree_info_b1$edges, 
													 directed=TRUE, 
													 vertices=unique(c(tree_info_b1$edges[,1],
													 									tree_info_b1$edges[,2],
													 									tree_info_b1$nodes))),
		 layout=layout_as_tree,
		 vertex.size = 10,
		 edge.arrow.mode=0,
		 edge.arrow.size = 0.01,
		 edge.arrow.width = 0.01)

plot(graph_from_data_frame(d=tree_info_b2$edges, 
													 directed=TRUE, 
													 vertices=unique(c(tree_info_b2$edges[,1],
													 									tree_info_b2$edges[,2],
													 									tree_info_b2$nodes))),
		 layout=layout_as_tree,
		 vertex.size = 10,
		 edge.arrow.mode=0,
		 edge.arrow.size = 0.01,
		 edge.arrow.width = 0.01)

# level 2 ids resampled from .TBS(ids_lvl_1)
plot(graph_from_data_frame(d=tree_info_b1_bb1$edges, 
													 directed=TRUE, 
													 vertices=unique(c(tree_info_b1_bb1$edges[,1],
													 									tree_info_b1_bb1$edges[,2],
													 									tree_info_b1_bb1$nodes))),
		 layout=layout_as_tree,
		 vertex.size = 10,
		 edge.arrow.mode=0,
		 edge.arrow.size = 0.01,
		 edge.arrow.width = 0.01)

plot(graph_from_data_frame(d=tree_info_b2_bb1$edges, 
													 directed=TRUE, 
													 vertices=unique(c(tree_info_b2_bb1$edges[,1],
													 									tree_info_b2_bb1$edges[,2],
													 									tree_info_b2_bb1$nodes))),
		 layout=layout_as_tree,
		 vertex.size = 10,
		 edge.arrow.mode=0,
		 edge.arrow.size = 0.01,
		 edge.arrow.width = 0.01)


# think of data storage structure for later model fitting  -------------

# maybe nested tibble best way to save 2 level bootstrap
?tibble()
# ids_b1,list(ids_b1_bb_1,ids_b1_bb_2,...,ids_b1_bb_B2)
# ids_b2,list(ids_b2_bb_1,ids_b2_bb_2,...,ids_b2_bb_B2)
# ...
# ids_B1,list(ids_bB1_bb_1,ids_bB1_bb_2,...,ids_bB1_bb_B2)

# nested tibbles
b=1

id_boot_1[[1]]
id_dubboot_from_b1[[1]]

size1=tibble(ids_tbs1=list(id_boot_1[[b]]),ids_tbs2=id_dubboot_from_b1)
size2=tibble(ids_tbs1=list(id_boot_1[[b]]),ids_tbs2=list(id_dubboot_from_b1))
object.size(size1)
object.size(size2)

str(size1,1)
str(size2,1)

# can then row wise purr:::map or for loop



# deprec ------------------------------------------------------------------


# library(igraph)
# ?graph.data.frame
# graph_from_data_frame
# plot(graph.data.frame(tree_info_b1$edges,
# 											vertices = tree_info_b1$nodes,
# 											directed = T))

# plot(graph_from_edgelist(as.matrix(
# 	remap_tree(id_boot_desc_b=id_dubboot_from_b1[[1]],
# 						 df_edges_ances_elig = samp$edges)$edges),
# 	directed = TRUE),
# 	layout=layout_as_tree,
# 	vertex.size = 10,
# 	edge.arrow.mode=0,
# 	edge.arrow.size = 0.01,
# 	edge.arrow.width = 0.01)

