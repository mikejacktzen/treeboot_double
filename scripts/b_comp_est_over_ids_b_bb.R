# helper functions after ids have been resampled
# df_est_samp_orig = compute_quants(df_4_estimate = df_use)

comp_est_over_ids_b_bb = function(ids_b,ids_bb,
																	df_use,  # data frame of original data
																	df_est_samp_orig,
																	compute_quants,  # user def function
																	...){
	
	require(tibble)
	
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
	
	df_resamp = df_use[ids_b,]
	
	quant_boot_lvl_1 = compute_quants(df_4_estimate = df_resamp)
	
	# quant_boot_lvl_1 = compute_quants(df_4_estimate = df_use[ids_b,])
	
	est_b = quant_boot_lvl_1$est_samp
	se_b = quant_boot_lvl_1$se_samp
	
	# inefficient to repeatedly compute this
	# better to compute once outside and have user pass in as argument
	# df_est_samp_orig = compute_quants(df_4_estimate = df_use)
	
	root_pivot_0_b = (est_b - df_est_samp_orig$est_samp)/se_b
	
	
	# equation before (5)
	# list_ind_bb_from_b = ids_bb
	
	# do not need to unroll the outter list (for purr map tibble )
	# ids_bb = unlist(ids_bb,recursive = FALSE)
	# ids_bb should already be a list(vec1,vec2,vec3)
	
	if(any(is.null(ids_bb))==TRUE){
		u_b=NA  # null 2nd level bootstrap from problematic 1st level bootstrap
	}else{
		quant_boot_lvl_2 = lapply(ids_bb,
															FUN=function(xx){
																
																# df_resamp = left_join(by='pid',
																# 											x=data.frame(pid=unlist(xx)),
																# 											y=df_use)
																
																df_resamp = df_use[unlist(xx),]
																
																compute_quants(df_4_estimate = df_resamp)
																# compute_quants(df_4_estimate = df_use[unlist(xx),])
															})
		
		est_b_bb = data.frame(do.call(rbind,quant_boot_lvl_2))[,'est_samp']
		se_b_bb = data.frame(do.call(rbind,quant_boot_lvl_2))[,'se_samp']
		
		root_pivot_b_bb = (est_b_bb - est_b)/se_b_bb
		
		# equation (5)
		# u_b = pivot_b  = z_b
		u_b = mean(na.omit(root_pivot_b_bb) <= root_pivot_0_b)
	}
	
	out_tib = tibble::tibble(root_pivot_0_b=root_pivot_0_b,
													 u_b=u_b,
													 est_b=est_b,
													 ids_b=list(ids_b))
	
	return(out_tib)
	# return(cbind(length(ids_b),sapply(ids_bb,length)))
	# return(cbind(length(unlist(ids_b)),length(unlist(ids_bb))))()
}




