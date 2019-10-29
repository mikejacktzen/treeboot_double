
# ?remap_tree
# ?sort_ids_tree_info

resample_id_tbs_dub = function(id_boot_1_ranked,B2,tree_info_samp_ranked){
	require(RDStreeboot)
	
	# needs access to 
	# remap_tree()
	# debug extra integers
	# suspect 2nd level still needs to re rank even if using 1st level rank
	# id_boot_1 = id_boot_1_ranked[[6]]
	
	# id_boot_1 = id_boot_1_orig
	id_boot_1 = id_boot_1_ranked
	
	# for(b in seq_along(id_boot_1)){
		# b=1
		# id_boot_1=samp_tb[[b]]  #is the b-th set of ids from the 1st level bootstrap
	
	
		# require original ids to lookup original edges
		tree_info_b1 = remap_tree(id_boot_desc_b=sort(id_boot_1), # pre sort important
															# id_boot_desc_b=id_boot_1[[b]],
															## if using original ids
															# df_edges_ances_elig = tree_info_samp_orig$edges  
															## if using ranked ids (less book keeping)
															
															df_edges_ances_elig = tree_info_samp_ranked$edges)
		
		# str(tree_info_b1,1)
		
		# Error in samp.adj.mat[cbind(samp$edges$node1, samp$edges$node2)] <- T : 
		# 	subscript out of bounds
		# id_dubboot_from_b1 = RDStreeboot:::.TBS(tree_info_b1, B=B2)
		
		## IF 'tree_info_b1' is using original ids
		## df_edges_ances_elig = tree_info_samp_orig$edges
		## sort again before using 2nd level .TBS
		
		# re rank 2nd time and need 2nd level df key
		# creating this key needs population of all nodes uniqued
		# unlike samp to b1, b1 to b2 can allow with replacement trees
		# need to uniqify and book keep new ids

		# based on n <- length(samp$nodes)		
		# ez lazy assumption that
		# output of tbs matches seq_along($nodes)


		# all_nodes_b1 = unique(union(tree_info_b1$nodes,unlist(tree_info_b1$edges)))
		
		# df_key_id_orig_w_rank_b1 = data.frame(id_orig=sort(all_nodes_b1),
		# 																			id_rank=seq_along(all_nodes_b1))
		# 
		# df_key_id_orig_w_rank_b
		# tree_info_b1_ranked = rank_ids_tree_info(tree_info_b1,df_key_id_orig_w_rank_b1)
		# 
		# tree_info_b1_ranked2 = tree_info_b1_ranked
		# tree_info_b1_ranked2$nodes = unique(tree_info_b1_ranked$nodes)
		
		TBS_possibly = purrr::possibly(RDStreeboot:::.TBS,NULL)
		id_dubboot_from_b1_ranked = TBS_possibly(tree_info_b1,
																						 # tree_info_b1_ranked2,
																						 B=B2)
		
		# output has ids that are outside of tree_info_b1$nodes
		# this is because output of tbs does not book keep original ids
		# it again, relies on nodes argument to be pre sorted
		# based on n <- length(samp$nodes) in first line of .TBS()
		# ez lazy assumption that
		# output of tbs matches seq_along(tree_info_b1$nodes)
		
		lapply(id_dubboot_from_b1_ranked,sort)
		
		df_key_id_orig_w_rank_b1 = data.frame(id_rank=seq_along(tree_info_b1$nodes),
																					id_orig=tree_info_b1$nodes)
		
		
		id_dubboot_from_b1_allow_rep = lapply(id_dubboot_from_b1_ranked,
																					FUN=plyr::mapvalues,
																					from=df_key_id_orig_w_rank_b1$id_rank,
																					to=df_key_id_orig_w_rank_b1$id_orig,
																					warn_missing=FALSE)
		
		
		tab_b_bb = tibble::tibble(ids_b=list(id_boot_1),
															# ids_b=list(id_boot_1[[b]]),
															# ids_bb=list(id_dubboot_from_b1)
															ids_bb=list(id_dubboot_from_b1_allow_rep))
		
	# 	list_tib_b_bb[[b]] = tab_b_bb
	# }
	# str(list_tib_b_bb,3)
		
	return(tab_b_bb)
}

# resample_id_tbs_dub(id_boot_1_ranked[[1]],B2=5)
# 
# ids_resamp_b_bb = id_boot_1_ranked %>% 
# 	purrr::map(.f=resample_id_tbs_dub,B2=10) %>% 
# 	do.call(rbind,.)
