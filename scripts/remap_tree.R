# to do a 2nd level treebootstrap with RDStreeboot:::.TBS()
# need to puzzle/figure out how to recreate tree / adjacency
# for each b, id_boot_1_tree = create_tree(id_boot_0[[b]])

# key step is to match descendent ids to ancestor ids

id_boot_desc_b  # ids from 1 single bootstrap from level 1
df_edges_ances_elig  # df of edge list of original data as eligible reference

remap_tree = function(id_boot_desc_b,df_edges_ances_elig){
	require(dplyr)
	# df_edges_ances_elig = samp$edges
	# id_boot_desc_b = id_boot_1[[b]]
	
	# edges_elig_samp_b_2 = df_edges_ances_elig %>% 
	# 	dplyr::filter((node1 %in% id_boot_desc_b) & (node2 %in% id_boot_desc_b))
	
	# mask out unnecessary? yes,after left_join identical to ignoring mask 
	# edges_elig_samp_b = df_edges_ances_elig
	
	
	# edges_elig_samp_b %>% head()
	# id_boot_1[[b]]
	
	# use fact that
	# node can only have up to 1 parent
	# node can have many child
	
	# for sampled nodes, attach eligible 'original data' edges
	# naming sampled nodes as 'node2' + left join = 'searching up/back tree'
	
	
	# edges_sampled_b_mask = left_join(x=data.frame(node2=as.numeric(id_boot_desc_b)),
	# 														y=edges_elig_samp_b_2)
	
	edges_sampled_b = left_join(x=data.frame(node2=as.numeric(id_boot_desc_b)),
															y=df_edges_ances_elig)
	
	# identical(edges_elig_samp_b_2,edges_elig_samp_b)
	# identical(edges_sampled_b,edges_sampled_b_mask)
	
	# str(edges_sampled_b,1)
	
	# for seed nodes, above attaches NA as parent
	# for island nodes, above attaches NA as edge
	
	# cosmeticaly get rid of parent redundant NA for seed
	# cosmetically get rid of island node with NA edge
	
	# parent and island node info preserved in seperate $node attribute
	# confirm this is behavior of .TBS(samp=list(edges,nodes))
	
	edges_sampled_b = edges_sampled_b %>% 
		select(node1,node2) %>% 
		na.omit() 
	
	# edges_sampled_b %>% head()
	
	# potential island nodes
	# setdiff(as.numeric(id_boot_desc_b),unique(unlist(edges_sampled_b)))
	
	# list with nodes and edges info required by .TBS()
	id_boot_1_treeinfo = list(nodes=as.numeric(id_boot_desc_b),
														edges=edges_sampled_b)
	# str(id_boot_1_treeinfo)
	
	return(id_boot_1_treeinfo)
}

library(RDStreeboot)
data(faux.network)
set.seed(123);samp <- sample.RDS(faux.network$traits, faux.network$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)

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

lapply(id_dubboot_b1,sort)
lapply(id_dubboot_b2,sort)

library(igraph)

par(mfrow=c(2,2))

# original data
plot(graph_from_edgelist(as.matrix(samp$edges),
	directed = TRUE),
	vertex.size = 0.1,
	arrow.mode=0,
	arrow.size = 0.01,
	arrow.width = 0.01)

library(igraph)
# ?graph.data.frame
# graph_from_data_frame
# plot(graph.data.frame(tree_info_b1$edges,
# 											vertices = tree_info_b1$nodes,
# 											directed = T))
# level 1
plot(graph_from_edgelist(as.matrix(
	remap_tree(id_boot_desc_b=id_boot_1[[1]],
						 df_edges_ances_elig = samp$edges)$edges),
	directed = TRUE),
	vertex.size = 0.1,
	arrow.mode=0,
	arrow.size = 0.01,
	arrow.width = 0.01)

# level 2

plot(graph_from_edgelist(as.matrix(
	remap_tree(id_boot_desc_b=id_dubboot_b1[[1]],
						 df_edges_ances_elig = samp$edges)$edges),
	directed = TRUE),
	vertex.size = 0.1,
	arrow.mode=0,
	arrow.size = 0.01,
	arrow.width = 0.01)
