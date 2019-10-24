rank_ids_tree_info = function(tree_info,
															df_key_id_orig_w_rank){
	
	require(plyr)
	# tree_info = samp
	# takeaway: need to pre sort nodes before using .TBS()
	# df_key_id_orig_w_rank = data.frame(id_orig=sort(tree_info$nodes),
	# 																	 id_rank=seq_along(tree_info$nodes))
	# if(orig_2_rank=TRUE){
	from_pick=df_key_id_orig_w_rank$id_orig  # original
	to_pick=df_key_id_orig_w_rank$id_rank  # sorted/ordered
	# }
	
	tree_info_id_rank = list()
	tree_info_id_rank$nodes = plyr::mapvalues(tree_info$nodes,
																						from=from_pick,
																						to=to_pick,
																						warn_missing=FALSE)
	
	# tree_info = tree_info_samp_orig$edges[1:10,]
	tree_info_id_rank$edges = data.frame(sapply(tree_info$edges,
																							FUN=plyr::mapvalues,
																							from=from_pick,
																							to=to_pick,
																							warn_missing=FALSE))
	return(tree_info_id_rank)
}

