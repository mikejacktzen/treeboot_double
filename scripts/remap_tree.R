# to do a 2nd level treebootstrap with RDStreeboot:::.TBS()
# need to puzzle/figure out how to recreate tree / adjacency
# for each b, id_boot_1_tree = create_tree(id_boot_0[[b]])

# key step is to match descendent ids to ancestor ids

# id_boot_desc_b  # ids from 1 single bootstrap from level 1
# df_edges_ances_elig  # df of edge list of original data as eligible reference


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
	
	# note: node2 is original input
	
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

# library(igraph)
# g0 <- graph_from_literal( 1 -+ 2 -+ 3 -+ 4,
# 													1 -+ 5 -+6,
# 													6-+7,6-+8,6-+9,
# 													7-+10,7-+11,8-+12,
# 													13,14)
# 
# plot(g0,layout=layout_as_tree,edge.arrow.mode=0)
# class(g0)
# 
# # as_edgelist(g0)
# list_v_and_e = igraph::as_data_frame(g0,what='both')
# str(list_v_and_e,2)
# 
# # restructure into format RDStreeboot:::.TBS() needs
# samp0 = list()
# samp0$nodes = as.numeric(unlist(list_v_and_e$vertices,use.names = FALSE))
# samp0$edges = data.frame(node1=as.numeric(list_v_and_e$edges$from),
# 												 node2=as.numeric(list_v_and_e$edges$to))
# 
# ind_lev_1 = RDStreeboot:::.TBS(samp0, B=1)
# tree_info_b1 = remap_tree(id_boot_desc_b=ind_lev_1[[1]],df_edges_ances_elig=samp0$edges)
# 
# # hand selected example
# ind_lev_1_pick = c(1,2,3,4,
# 									 5,6,7,10,11,
# 									 7,10,11,
# 									 7,11,11,
# 									 13,13)
# 
# tree_info_b1_pick = remap_tree(id_boot_desc_b=ind_lev_1_pick,df_edges_ances_elig=samp0$edges)
# tree_info_b1_pick
# 
# plot(graph_from_data_frame(d=tree_info_b1_pick$edges, 
# 													 directed=TRUE, 
# 													 vertices=unique(c(tree_info_b1_pick$edges[,1],
# 													 									tree_info_b1_pick$edges[,2],
# 													 									tree_info_b1_pick$nodes))),
# 		 layout=layout_as_tree,
# 		 vertex.size = 10,
# 		 edge.arrow.mode=0,
# 		 edge.arrow.size = 0.01,
# 		 edge.arrow.width = 0.01)
# 
# 
# id_dubboot_from_b1 = RDStreeboot:::.TBS(tree_info_b1_pick, B=5)

