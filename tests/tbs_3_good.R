# best version works

tbs_3 = function (samp, B,id_node_samp) {
	# id_node_samp need to be all obs nodes (possibly repeated)
	# the ids need to match those used by samp
	# eg 'ranked' 
	
	# nodes_orig_samp = c(1,3,6,4,13,15,12,10,20)  	# 4 has 4 kids
	# t0 = rank_ids_treeinfo(tree_info_samp_orig,nodes_orig_samp)
	# idsb1 = tbs_3(t0,id_node_samp_rank = t0$nodes_obs_rank,B=10)
	
	n <- length(samp$nodes)
	samp.adj.mat <- matrix(F, n, n)
	samp.adj.mat[cbind(samp$edges$node1, samp$edges$node2)] <- T
	samp.adj.list <- list()
	for (i in 1:n) samp.adj.list[[i]] <- which(samp.adj.mat[i,])
	
	# original
	# seeds <- which(apply(samp.adj.mat, 2, sum) == 0)
	
	# small tweak to allow repeated seeds (helpful for b1 -> b2)
	seeds_nomult <- which(apply(samp.adj.mat, 2, sum) == 0)
	# with mult
	seeds = id_node_samp[id_node_samp %in% sort(unique(id_node_samp))[seeds_nomult]]
	
	num.seeds <- length(seeds)
	leaves <- apply(samp.adj.mat, 1, sum) == 0
	resamp <- list()
	for (b in 1:B) {
		nodes <- rep(NA, 2 * n)
		nodes[1:num.seeds] <- seeds[sample.int(num.seeds, replace = T)]
		curr <- 1
		end <- length(seeds) + 1
		while (curr < end) {
			# curr=1
			adj <- samp.adj.list[[nodes[curr]]]  # unique adjacent child node values
			# num.adj <- length(adj)  # this needs to be # of non unique adjacent nodes
			
			# num.adj needs to be adapted justs like seeds and seeds_nomult was
			
			# num times current parent node appears in obs samp (if par repeated)
			num_par_curr = sum(id_node_samp %in% nodes[curr])
			# num times adj nodes of current parent node appears in obs samp
			adj_of_par_curr = id_node_samp[id_node_samp %in% adj]
			num_adj_of_par_curr = length(adj_of_par_curr)
			num.adj = num_adj_of_par_curr/num_par_curr  # reduces to current parent only (if parent was repeated)
			
			if (end + num.adj - 1 > length(nodes)) 
				nodes <- c(nodes, rep(NA, n))
			if (num.adj > 0) {
				
			nodes[end:(end + num.adj - 1)] <- adj_of_par_curr[sample.int(n=num_adj_of_par_curr,
																																	 size=num.adj,replace = T)]
			}
			
			
			curr <- curr + 1
			end <- end + num.adj
		}
		resamp[[b]] <- nodes[1:(end - 1)]
	}
	return(resamp)
}


rank_ids_treeinfo = function(tree_info,nodes_obs_in){
	
	require(plyr)
	require(dplyr)
	
	nodes_obs_uniq = unique(nodes_obs_in)
	nodes_uniq_use = sort(tree_info$nodes[(tree_info$nodes %in% nodes_obs_uniq)])
	
	edges_use = tree_info$edges %>% 
		dplyr::filter((node1 %in% nodes_obs_in)&(node2 %in% nodes_obs_in))
	
	
	df_key_id_in2rank = data.frame(id_in=nodes_uniq_use,
																 id_rank=seq_along(nodes_uniq_use),
																 stringsAsFactors = FALSE)
	
	from_pick=df_key_id_in2rank$id_in  # original
	to_pick=df_key_id_in2rank$id_rank  # sorted/ordered
	
	tree_info_id_rank = list()
	
	tree_info_id_rank$nodes = as.integer(plyr::mapvalues(nodes_uniq_use,
																											 from=from_pick,
																											 to=to_pick,
																											 warn_missing=FALSE))
	edges_temp = data.frame(sapply(edges_use,
																 FUN=plyr::mapvalues,
																 from=from_pick,
																 to=to_pick,
																 warn_missing=FALSE),
													stringsAsFactors = F)
	
	
	tree_info_id_rank$edges = as.data.frame(sapply(edges_temp,as.integer))
	
	tree_info_id_rank$df_key_id_in2rank = df_key_id_in2rank
	
	tree_info_id_rank$nodes_obs_in = nodes_obs_in
	
	tree_info_id_rank$nodes_obs_rank = left_join(data.frame(id_in=nodes_obs_in),
																							 y=df_key_id_in2rank) %>% 
		select(id_rank) %>% unlist(use.names = F)
	
	return(tree_info_id_rank)
}


