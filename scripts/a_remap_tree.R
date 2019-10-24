# to do a 2nd level treebootstrap with RDStreeboot:::.TBS()
# need to puzzle/figure out how to recreate tree / adjacency
# for each b, id_boot_1_tree = remap_tree(id_boot_0[[b]],df_edges_ances_elig)
# 
# key step is to match descendent ids to ancestor ids
# then simple left_join / lookup from original dataset edges (acting as eligible edges)
# 
# id_boot_desc_b  # ids from 1 single bootstrap from level 1
# df_edges_ances_elig  # df of edge list of original data as eligible reference

# side notes
# mask out unnecessary? yes,after left_join identical to ignoring mask 
# identical(edges_elig_samp_b_2,edges_elig_samp_b)
# identical(edges_sampled_b,edges_sampled_b_mask)

remap_tree = function(id_boot_desc_b,df_edges_ances_elig){
	
	## only throw this warning if working with original id
	## but original id method is more book keeping
	# 
	# if(identical(sort(id_boot_desc_b),seq_along(id_boot_desc_b))==TRUE){
	# 	warning(cat('make sure the "id_boot_desc_b" argument is a vector of "original" ids,
	# 					since .TBS() outputs ordered ids (it internally requires ordered ids).
	# 					Goal is to lookup / attach original edges of "df_edges_ances_elig",
	# 					which requires "id_boot_desc_b" to be original id labels'))
	# }


	
	require(dplyr)
	# df_edges_ances_elig = samp$edges
	# id_boot_desc_b = id_boot_1[[b]]
	
	# use fact that
	# node can only have up to 1 parent
	# node can have many child
	
	# for sampled nodes, attach eligible 'original data' edges
	# naming sampled nodes as 'node2' + left join = 'searching up/back tree'
	
	edges_sampled_b = suppressMessages(left_join(x=data.frame(node2=as.numeric(id_boot_desc_b)),
														 y=df_edges_ances_elig))
	
	# edges_sampled_b = left_join(x=data.frame(node2=as.numeric(id_boot_desc_b)),
	# 														y=df_edges_ances_elig)
	# 
	# note: node2 is original input
	
	# str(edges_sampled_b,1)
	
	# for seed nodes, above attaches NA as parent
	# for island nodes, above attaches NA as edge
	
	# cosmeticaly get rid of parent redundant NA for seed
	# cosmetically get rid of island node with NA edge
	
	# parent and island node info preserved in seperate foo$node attribute
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

