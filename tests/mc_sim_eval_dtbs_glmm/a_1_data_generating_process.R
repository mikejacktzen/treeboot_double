# sample RDS faux network -------------------------------------------------




# source('~/temp_work_ts2/treeboot_double/scripts/a_rank_ids_tree_info.R')

# n=100, 
# num.seeds=4, 
# num.samp=3,
# num.prob=c(0,1/3,1/3,1/3),

gen_data_rds = function(beta,
												n_use,
												num.seeds_use,
												num.samp_use=4,
												num.prob_use = c(0,c(1/4,1/4,1/4,1/4))
												){
	
	require(plyr)
	require(dplyr)
	require(RDStreeboot)
	
	data("faux.network")
	# edge list to igraph
	# str(faux.network)
	
	dat_pop_x = faux.network$traits[,1:2]
	
	# use population traits to create pop level outcome X~Y GLM no RE
	
	
	gen_pop_yx = function(beta_yonx,dat_pop){
		# simple glm
		eta_glm <- as.vector(dat_pop$X * beta_yonx)
		dat_pop$Y <- rbinom(nrow(dat_pop), 1, plogis(eta_glm))
		return(dat_pop)
	}
	
	dat_pop=gen_pop_yx(beta_yonx=beta,dat_pop_x)
	
	# hist(dat_pop$Y)
	# hist(dat_pop$X)
	
	
	# fm_glm <- glm(X ~ -1 + Z, data = dat_pop,
	# 							family = binomial())
	# summary(fm_glm)
	
	# dim(dat_pop)
	
	

	
	# n_use=100,
	# num.seeds_use=4,
	# num.samp_use=3,
	# num.prob_use=c(0,1/3,1/3,1/3),
	
	# set.seed(123);
	samp0 <- sample.RDS(traits=dat_pop,
																		adj.mat=faux.network$adj.mat,
																		n=n_use,
																		num.seeds=num.seeds_use, 
																		num.samp=num.samp_use,
																		num.prob=num.prob_use,
																		replace=FALSE)
	
	
	df_orig = samp0$traits
	
	# # pre sort $nodes attribute
	samp0$nodes = sort(samp0$nodes)
	tree_info_samp_orig = samp0; rm(samp0)
	# str(tree_info_samp_orig)
	
	df_orig$id_rank = rank(as.numeric(rownames(df_orig)))
	
	# diagnose the bahviour should be something like
	# particular to sample.RDS output
	# rank(tree_info_samp_orig$nodes)
	# unlist(tree_info_samp_orig$edges)
	# setdiff(unlist(tree_info_samp_orig$edges),rank(tree_info_samp_orig$nodes))
	
	all_nodes = unique(union(rank(tree_info_samp_orig$nodes),unlist(tree_info_samp_orig$edges)))
	
	# length(all_nodes)
	# dim(df_orig)
	# setdiff(df_orig$id_rank,all_nodes)
	
	df_key_id_orig_w_rank = data.frame(id_orig=sort(all_nodes),
																		 id_rank=seq_along(all_nodes))
	
	# length(all_nodes)
	# dim(df_orig)
	
	df_use = df_orig
	
	# str(tree_info_samp_orig)
	tree_info_samp_orig$nodes = rank(tree_info_samp_orig$nodes)
	tree_info_samp_ranked = rank_ids_tree_info(tree_info_samp_orig,df_key_id_orig_w_rank)
	
	require(igraph)
	g1 = igraph:::graph.data.frame(d=tree_info_samp_ranked$edges, 
																 vertices=unique(c(tree_info_samp_ranked$nodes,
																 									unlist(tree_info_samp_ranked$edges))),
																 directed = TRUE)
	
	# plot(g1,layout=layout_as_tree)
	
	
	form_cluster_id = function(igraph_to_cluster){
		require(igraph)
		require(dplyr)
		
		# treat each tree as a cluster
		graph_test_simplify = simplify(igraph_to_cluster)
		nodes_sort = V(graph_test_simplify) %>% as.integer(.)
		test_edgelist = data.frame(as_edgelist(graph_test_simplify),
															 stringsAsFactors = FALSE) %>% 
			mutate_all(.,.funs=as.integer)
		names(test_edgelist) = c('node1','node2')
		
		edgelist_sort = dplyr::arrange(test_edgelist,
																	 node1,node2)
		
		########################################
		# use graph_test_simplify
		########################################
		# every vertex involved in an edge?
		# expect 216 singleton vertex
		sum(!(nodes_sort %in% c(edgelist_sort[,1],edgelist_sort[,2])))
		
		# for cluster assignment, eg id of random effect
		c1 <- igraph::clusters(graph_test_simplify,mode='weak')
		
		# c1$membership
		df_cluster = data.frame(id_cluster=seq_along(c1$csize),size_cluster=c1$csize)
		
		# c1$membership
		
		id_cluster_singleton = df_cluster %>% 
			filter(size_cluster==1) %>% select(id_cluster)
		
		df_membership_map = data.frame(id_node=as.integer(names(c1$membership)),
																	 #tab_hash_pid_int,
																	 id_cluster=as.integer(c1$membership)) %>% 
			mutate(id_cluster_grpsing =
						 	ifelse(id_cluster %in% unlist(id_cluster_singleton),
						 				 9999,id_cluster))
		# c1$membership
		# df_cluster_info = list(df_membership_map = data.frame(id_node=as.numeric(names(c1$membership)),
		# 																											#tab_hash_pid_int,
		# 																											id_cluster=as.integer(c1$membership)),
		# 											 id_cluster_singleton=id_cluster_singleton)
		
		return(df_membership_map)
	}
	
	
	info_cluster = form_cluster_id(g1)
	
	# View(info_cluster)
	# table(info_cluster$id_cluster_grpsing)
	
	# df_use_preattach = df_use
	
	# dim(df_use_preattach)
	# sum(table(info_cluster$id_cluster_grpsing))
	# tree info has 193 nodes but df traits only has 100 rows
	
	df_4_estimate = left_join(x=df_use,
														y=select(info_cluster,-id_cluster),
														by=c('id_rank'='id_node'))

	out_info = list(df_4_estimate=df_4_estimate,
									tree_info_samp_ranked=tree_info_samp_ranked)
	
	# out_info = list(df_4_estimate=df_4_estimate)
	return(out_info)
}

# test = gen_data_rds(num.seeds_use = 3)
# test$id_cluster_grpsing %>% unique()
# 
# test = gen_data_rds(num.seeds_use = 10)
# test$id_cluster_grpsing %>% unique()
