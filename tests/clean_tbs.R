source("~/treeboot_double/tests/tbs_3_good.R")

library(igraph)
library(plyr)
library(dplyr)
g0 <- graph_from_literal(4-+12,
												 4-+13-+15,
												 13-+20,
												 4-+10,4-+10,
												 3-+6,
												 1,
												 simplify = F)
plot(g0,layout=layout_as_tree)

list_v_and_e = igraph::as_data_frame(g0,what='both')
samp0 = list()
samp0$nodes = as.numeric(unlist(list_v_and_e$vertices,use.names = FALSE))
samp0$edges = data.frame(node1=as.numeric(list_v_and_e$edges$from),
												 node2=as.numeric(list_v_and_e$edges$to))
samp0$nodes = sort(samp0$nodes)
tree_info_samp_orig = samp0; rm(samp0)


nodes_orig_samp = c(1,3,6,4,13,15,12,10,20,10)
# 4 has 4 kids
t0 = rank_ids_treeinfo(tree_info_samp_orig,nodes_orig_samp)


# idsb1 = tbs_2(t0,id_node_samp = t0$nodes_obs_rank,B=10)
idsb1 = tbs_3(t0,id_node_samp = t0$nodes_obs_rank,B=10)

lapply(idsb1,
			 function(xx){
			 	left_join(x=data.frame(id_rank=xx),
			 						t0$df_key_id_in2rank,by='id_rank')
			 }
)

idb1 = idsb1[[9]]


t1 = rank_ids_treeinfo(t0,idb1)


# idsb2 = tbs_2(t1,id_node_samp = t1$nodes_obs_rank,B=10)

idsb2 = tbs_3(t1,id_node_samp = t1$nodes_obs_rank,B=10)


lapply(idsb2,
			 function(xx){
			 	left_join(x=data.frame(id_rank=xx),
			 						t1$df_key_id_in2rank,by='id_rank') %>% 
			 		left_join(x=.,y=t0$df_key_id_in2rank,
			 							by=c('id_in'='id_rank'))
			 }
)


