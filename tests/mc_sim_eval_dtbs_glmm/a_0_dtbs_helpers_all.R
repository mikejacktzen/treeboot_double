rank_ids_tree_info = function(tree_info,
															df_key_id_orig_w_rank){
	
	require(plyr)
	
	from_pick=df_key_id_orig_w_rank$id_orig  # original
	to_pick=df_key_id_orig_w_rank$id_rank  # sorted/ordered
	
	tree_info_id_rank = list()
	tree_info_id_rank$nodes = plyr::mapvalues(tree_info$nodes,
																						from=from_pick,
																						to=to_pick,
																						warn_missing=FALSE)
	
	tree_info_id_rank$edges = data.frame(sapply(tree_info$edges,
																							FUN=plyr::mapvalues,
																							from=from_pick,
																							to=to_pick,
																							warn_missing=FALSE))
	return(tree_info_id_rank)
}



remap_tree = function(id_boot_desc_b,df_edges_ances_elig){
	
	
	require(dplyr)
	
	
	edges_sampled_b = suppressMessages(left_join(x=data.frame(node2=as.numeric(id_boot_desc_b)),
																							 y=df_edges_ances_elig))
	
	edges_sampled_b = edges_sampled_b %>% 
		select(node1,node2) %>% 
		na.omit() 
	
	
	# list with nodes and edges info required by .TBS()
	id_boot_1_treeinfo = list(nodes=as.numeric(id_boot_desc_b),
														edges=edges_sampled_b)
	
	# str(id_boot_1_treeinfo)
	
	return(id_boot_1_treeinfo)
}


resample_id_tbs_dub = function(id_boot_1_ranked,B2,tree_info_samp_ranked){
	require(RDStreeboot)
	
	# id_boot_1 = id_boot_1_orig
	id_boot_1 = id_boot_1_ranked
	
	# require original ids to lookup original edges
	tree_info_b1 = remap_tree(id_boot_desc_b=sort(id_boot_1), # pre sort important
														df_edges_ances_elig = tree_info_samp_ranked$edges)
	
	# str(tree_info_b1,1)
	
	
	# IMPORTANT: RE RANK IDS b4 2nd TBS
	
	all_nodes_b1 = unique(union(tree_info_b1$nodes,unlist(tree_info_b1$edges)))
	df_key_id_orig_w_rank_b1 = data.frame(id_orig=sort(all_nodes_b1),
																				id_rank=seq_along(all_nodes_b1))
	
	
	tree_info_b1_ranked = rank_ids_tree_info(tree_info_b1,df_key_id_orig_w_rank_b1)
	
	tree_info_b1_ranked2 = tree_info_b1_ranked
	tree_info_b1_ranked2$nodes = unique(tree_info_b1_ranked$nodes)
	
	TBS_possibly = purrr::possibly(RDStreeboot:::.TBS,NULL)
	id_dubboot_from_b1_ranked = TBS_possibly(# tree_info_b1,
		tree_info_b1_ranked2,
		B=B2)
	
	
	df_key_id_orig_w_rank_b1 = data.frame(id_rank=seq_along(tree_info_b1$nodes),
																				id_orig=tree_info_b1$nodes)
	
	
	id_dubboot_from_b1_allow_rep = lapply(id_dubboot_from_b1_ranked,
																				FUN=plyr::mapvalues,
																				from=df_key_id_orig_w_rank_b1$id_rank,
																				to=df_key_id_orig_w_rank_b1$id_orig,
																				warn_missing=FALSE)
	
	# lapply(id_dubboot_from_b1_allow_rep,function(xx){
	# 	xx %in% id_boot_1
	# })
	
	tab_b_bb = tibble::tibble(ids_b=list(id_boot_1),
														# ids_b=list(id_boot_1[[b]]),
														# ids_bb=list(id_dubboot_from_b1)
														ids_bb=list(id_dubboot_from_b1_allow_rep))
	
	
	return(tab_b_bb)
}


form_int_dubboot = function(df_est_samp_orig,
														df_ref_quants,alpha=0.05){
	
	B1_na = nrow(df_ref_quants)
	
	root_pivot_0_b_sorted = sort(df_ref_quants$root_pivot_0_b)
	
	# using above limits to lookup ordered roots of first level bootstrap estimates
	
	# l_ref_root = root_pivot_0_b_sorted[floor(l_ref)]
	# u_ref_root = root_pivot_0_b_sorted[ifelse(floor(u_ref)==0,1,floor(u_ref))]
	
	# above is manual
	# same as using quantile()
	l_ref_root = quantile(root_pivot_0_b_sorted,(1-alpha/2))
	u_ref_root = quantile(root_pivot_0_b_sorted,(alpha/2))
	
	# df_est_samp_orig = compute_quants_possibly(df_use)
	
	# ll_dtbs_pivot = df_est_samp_orig$est_samp - l_ref_root*df_est_samp_orig$se_samp
	# ul_dtbs_pivot = df_est_samp_orig$est_samp - u_ref_root*df_est_samp_orig$se_samp
	
	ll_dtbs_pivot = as.numeric(df_est_samp_orig[[1]] - l_ref_root*df_est_samp_orig[[2]])
	ul_dtbs_pivot = as.numeric(df_est_samp_orig[[1]] - u_ref_root*df_est_samp_orig[[2]])
	
	if(ul_dtbs_pivot >= ll_dtbs_pivot){
		ul=ul_dtbs_pivot
		ll=ll_dtbs_pivot
	}else{
		ul=ll_dtbs_pivot
		ll=ul_dtbs_pivot
	}
	
	est_dubboot_out = data.frame(# est_samp=df_est_samp_orig$est_samp,
		# mean_est_b = mean(df_ref_quants$est_b),
		length_int = abs(ul_dtbs_pivot-ll_dtbs_pivot),
		# ll_dtbs_pivot,ul_dtbs_pivot,
		ll=ll,ul=ul
	)
	
	return(est_dubboot_out)
}



get_ids_dtbs = function(B1_pick,B2_pick,df_sim_one){
	
	# source("~/temp_work_ts2/treeboot_double/scripts/zz_example_clean.R")
	# B1_pick=B
	# set.seed(4321);
	
	tree_info_samp_ranked = df_sim_one$tree_info_samp_ranked
	
	id_boot_1_ranked = RDStreeboot:::.TBS(tree_info_samp_ranked, B=B1_pick)
	
	
	# level 2
	# 'a_resample_id_tbs_dub.R'
	
	# B2_pick = C
	ids_resamp_b_bb = id_boot_1_ranked %>% 
		purrr::map(.f=resample_id_tbs_dub,B2=B2_pick,tree_info_samp_ranked) %>% 
		do.call(rbind,.)
	
	# head(ids_resamp_b_bb)
	
	# all(T==names(table(unlist(ids_resamp_b_bb[[2]]))) %in% names(table(unlist(ids_resamp_b_bb[[1]]))))
	
	# dim(ids_resamp_b_bb)
	return(ids_resamp_b_bb)
}


# this _serial() version below useful only if embeded in outer mc sim
# the package version should be parallelized at the first level b

comp_est_over_ids_b_bb_onerow_serial = function(ids_b_bb_one,
																								# ids_b,ids_bb,
																								df_use,  # data frame of original data
																								df_est_samp_orig,
																								compute_quants,  # user def function
																								# iter_pick,
																								...){
	
	require(tibble)
	# require(parallel)
	
	# ids_b_bb_one = sstest[1,]
	
	# ids_b_bb_one = sstest[1,]
	# using numeric indexing prevented some errors about $ index
	ids_b = unlist(ids_b_bb_one[[1]],recursive=FALSE)
	# ids_bb=ids_b_bb_one[[2]]
	
	ids_bb = unlist(ids_b_bb_one[[2]],recursive=FALSE)
	
	
	stopifnot("id_rank" %in% names(df_use))
	# pre sort by 'id_rank'
	# since our goal of using id_rank returned from bootstrapping
	# is to easily use it as row ids to subset df
	
	df_use = arrange(df_use,id_rank)
	
	# b=1
	# df_resamp = df_use[test$ids_b[[1]],]
	
	# if not ordered, need left join
	# df_resamp = left_join(by='pid',
	# 											x=data.frame(pid=ids_b),
	# 											y=df_use)
	
	# ids_b = ids_resamp_b_bb[1,][['ids_b']] %>% unlist
	
	df_resamp = df_use[ids_b,]
	
	# quant_boot_lvl_1 = compute_quants(df_4_estimate = df_resamp)
	
	# quant_boot_lvl_1 = compute_quants(df_4_estimate = df_resamp,iter_pick)
	quant_boot_lvl_1 = compute_quants(df_4_estimate = df_resamp)
	
	# quant_boot_lvl_1 = compute_quants(df_4_estimate = df_use[ids_b,])
	
	# est_b = quant_boot_lvl_1$est_samp
	# se_b = quant_boot_lvl_1$se_samp
	# 
	
	# if 'quant_boot_lvl_1' 3 types of estimates, then lapply over
	# lapply(quant_boot_lvl_1)
	
	est_b = quant_boot_lvl_1[[1]]
	se_b = quant_boot_lvl_1[[2]]
	
	# inefficient to repeatedly compute this
	# better to compute once outside and have user pass in as argument
	# df_est_samp_orig = compute_quants(df_4_estimate = df_use)
	
	# root_pivot_0_b = (est_b - df_est_samp_orig$est_samp)/se_b
	# root_pivot_0_b = (est_b - df_est_samp_orig['est_samp'])/se_b
	root_pivot_0_b = (est_b - df_est_samp_orig[[1]])/se_b
	
	
	# equation before (5)
	# list_ind_bb_from_b = ids_bb
	
	# do not need to unroll the outter list (for purr map tibble )
	# ids_bb = unlist(ids_bb,recursive = FALSE)
	# ids_bb should already be a list(vec1,vec2,vec3)
	
	# ids_bb = ids_resamp_b_bb[1,2] %>% unlist(.,FALSE) %>% unlist(.,FALSE)
	
	if(any(is.null(ids_bb))==TRUE){
		u_b=NA  # null 2nd level bootstrap from problematic 1st level bootstrap
	}else{
		# parLapply()
		# serial
		
		quant_boot_lvl_2 = lapply(X=ids_bb,
															df_use=df_use,compute_quants=compute_quants,
															FUN=function(xx,df_use,compute_quants){
																
																# df_resamp = left_join(by='pid',
																# 											x=data.frame(pid=unlist(xx)),
																# 											y=df_use)
																
																df_resamp = df_use[unlist(xx),]
																
																# compute_quants(df_4_estimate = df_resamp)
																# compute_quants(df_4_estimate = df_resamp,iter_pick)
																compute_quants(df_4_estimate = df_resamp)
																
																# compute_quants(df_4_estimate = df_use[unlist(xx),])
															}
		)
		
		# the lower level bootstrap comp is the time consuming part
		# candidate to do parallel
		
		# quant_boot_lvl_2 = lapply(ids_bb,
		# 													FUN=function(xx){
		# 														
		# 														# df_resamp = left_join(by='pid',
		# 														# 											x=data.frame(pid=unlist(xx)),
		# 														# 											y=df_use)
		# 														
		# 														df_resamp = df_use[unlist(xx),]
		# 														
		# 														# compute_quants(df_4_estimate = df_resamp)
		# 														# compute_quants(df_4_estimate = df_resamp,iter_pick)
		# 														compute_quants(df_4_estimate = df_resamp)
		# 														
		# 														# compute_quants(df_4_estimate = df_use[unlist(xx),])
		# 													})
		
		est_b_bb = data.frame(do.call(rbind,quant_boot_lvl_2))[,'est_samp']
		se_b_bb = data.frame(do.call(rbind,quant_boot_lvl_2))[,'se_samp']
		
		root_pivot_b_bb = (est_b_bb - est_b)/se_b_bb
		
		# equation (5)
		# u_b = pivot_b  = z_b
		u_b = mean(na.omit(root_pivot_b_bb) <= root_pivot_0_b)
	}
	
	out_tib = tibble::tibble(root_pivot_0_b=root_pivot_0_b,
													 u_b=u_b,
													 est_b=est_b
													 # ids_b=list(ids_b)
	)
	
	return(out_tib)
	# return(cbind(length(ids_b),sapply(ids_bb,length)))
	# return(cbind(length(unlist(ids_b)),length(unlist(ids_bb))))()
}
