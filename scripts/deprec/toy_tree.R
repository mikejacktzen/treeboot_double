# install.packages('igraph')
library(igraph)
# ?graph_from_literal

# specify easy toy example
# A directed graph

# RDS graph of 'original data' (fake) -----------------------------------

g0 <- graph_from_literal( 1 -+ 2 -+ 3 -+ 4,
													1 -+ 5 -+6,
													6-+7,6-+8,6-+9,
													7-+10,7-+11,8-+12,
													13,14
)
plot(g0,layout=layout_as_tree)

class(g0)

as_edgelist(g0)
list_v_and_e = igraph::as_data_frame(g0,what='both')
str(list_v_and_e,2)

# restructure into format RDStreeboot:::.TBS() needs
samp0 = list()
samp0$nodes = as.numeric(unlist(list_v_and_e$vertices,use.names = FALSE))
samp0$edges = data.frame(node1=as.numeric(list_v_and_e$edges$from),
												 node2=as.numeric(list_v_and_e$edges$to))


str(samp0)


# level 1 tbs -------------------------------------------------------------

set.seed(1234); nodes_l1=RDStreeboot:::.TBS(samp=samp0,B=2)
b=1
nodes_l1[[b]]
str(nodes_l1,1)


# pretend 'ind_lev_1_pick' is one bootstrap from level 1 tbs
# ind_lev_1_pick = nodes_l1[[1000]]

ind_lev_1_pick = c(1,2,3,4,
									5,6,7,10,11,
									7,10,11,
									7,11,11,
									13,13)

ind_lev_1_pick
unique(ind_lev_1_pick)

# graph_from_literal() needs unique IDS too

g1 <- graph_from_literal( 1 -+ 2 -+ 3 -+ 4,
													1 -+ 5 -+6,
													6-+7,#6-+8,6-+9,
													7-+10,7-+11,
													6-+'7b',
													'7b'-+'10b','7b'-+'11b',
													6-+'7c',
													'7c'-+'11c','7c'-+'11d',
													13,'13b')
plot(g1,layout=layout_as_tree)

# RDStreeboot:::.TBS() needs to relabel unique IDS

# this will not work
# RDStreeboot:::.TBS(remap_tree(ind_lev_1_pick,...))



# TODO FILL IN ------------------------------------------------------------
# # might need to do something like
# # ind_lev_1_pick_unique = relabel_unique(ind_lev_1_pick)
# # ...
# # remap_tree(ind_lev_1_pick_unique)
# # ...
# # 
# # RDStreeboot:::.TBS(remap_tree(ind_lev_1_pick_unique))
# 
# 
# par(mfrow=c(1,2))
# plot(g0,layout=layout_as_tree,edge.arrow.mode=0)
# plot(g1,layout=layout_as_tree,edge.arrow.mode=0)
# 
# 
# # # 3 use three objects
# # ind_lev_1_pick
# # unique(ind_lev_1_pick)
# # samp0
# # # to create
# # as_edgelist(g1)
# 
# 
# test=data.frame(ind_lev_1_uniq=seq_along(ind_lev_1_pick),ind_lev_1_pick=sort(ind_lev_1_pick))
# 
# test2 = test %>% 
# 	group_by(ind_lev_1_pick) %>% 
# 	mutate(appear = row_number()) %>% 
# 	ungroup %>% 
# 	mutate(ind_lev_1_relab = ifelse(appear>1,
# 																	paste0(ind_lev_1_pick,letters[appear]),
# 																	ind_lev_1_pick))
# 	
# 
# # lapply(test2$appear,FUN=function(xx)paste0(rep(" ",times=xx),collapse=""))
# # lapply(test2$appear,FUN=function(xx)letters[xx])
# 
# test3 = test2 %>% 
# 	# select(ind_lev_1_pick) %>% 
# 	select(ind_lev_1_pick) %>% 
# 	unlist %>% 
# 	remap_tree(id_boot_desc_b=.,
# 						 df_edges_ances_elig = samp0$edges)
# 
# # note: node2 is original input
# 
# test3$edges %>% 
# 	arrange(node1,node2) %>% 
# 	mutate(node12=paste0(node1,"_",node2)) %>% 
# 	# group_by(node2) %>% 
# 	group_by(node12) %>% 
# 	
# 	mutate(appear = row_number()) %>% 
# 	mutate(ind_lev_1_relab = ifelse(appear>1,
# 																paste0(ind_lev_1_pick,letters[appear]),
# 																ind_lev_1_pick))
# as_edgelist(g1)
# 
# ids_nonedges = test3$nodes[which(!(test3$nodes %in% c(test3$edges[,1],test3$edges[,2])))]
# ids_nonedges
# 
# # data.frame(ids_nonedges=ids_nonedges) %>% 
# # 	group_by(ids_nonedges) %>% 
# # 	mutate(appear = row_number()) %>% 
# # 	ungroup %>% 
# # 	mutate(ids_nonedges_relab = ifelse(appear>1,
# # 																	paste0(ids_nonedges,letters[appear]),
# # 																	ids_nonedges))

