
# ?remap_tree
# ?sort_ids_tree_info

resample_id_tbs_dub = function(id_boot_1_ranked,B2){
	require(RDStreeboot)
	
	# needs access to 
	# remap_tree()
	
	# id_boot_1 = id_boot_1_orig
	id_boot_1 = id_boot_1_ranked
	
	# for(b in seq_along(id_boot_1)){
		# b=1
		# id_boot_1=samp_tb[[b]]  #is the b-th set of ids from the 1st level bootstrap
		
		# require original ids to lookup original edges
		tree_info_b1 = remap_tree(id_boot_desc_b=id_boot_1,
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
		
		# tree_info_b1_ranked = sort_ids_tree_info(tree_info_b1,df_key_id_orig_w_sort)
		
		
		TBS_possibly = purrr::possibly(RDStreeboot:::.TBS,NULL)
		id_dubboot_from_b1_ranked = TBS_possibly(tree_info_b1,
																						 # tree_info_b1_ranked,
																						 B=B2)
		
		
		tab_b_bb = tibble::tibble(ids_b=list(id_boot_1),
															# ids_b=list(id_boot_1[[b]]),
															# ids_bb=list(id_dubboot_from_b1)
															ids_bb=list(id_dubboot_from_b1_ranked))
		
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
