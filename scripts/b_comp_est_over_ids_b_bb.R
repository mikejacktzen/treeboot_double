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



form_int_dubboot = function(df_est_samp_orig,
														df_ref_quants,alpha=0.05){
	
	# alpha=0.05
	
	# https://pdfs.semanticscholar.org/46b5/2cb68dc22b1293da76b46b66ae1629ea8a43.pdf
	
	# all the effort is to obtain reference distribution
	# roots 'l_ref' and 'l_ref_root
	
	# limits of pivotal reference distribution
	# l_ref = B1_na*quantile(df_ref_quants$u_b,(1-alpha/2)*B1_na*1e-2)
	# u_ref = B1_na*quantile(df_ref_quants$u_b,(alpha/2)*B1_na*1e-2)
	B1_na = nrow(df_ref_quants)
	
	l_ref = B1_na*quantile(df_ref_quants$u_b,(1-alpha/2))
	u_ref = B1_na*quantile(df_ref_quants$u_b,(alpha/2))
	
	# floor(l_ref)
	# floor(u_ref)
	
	# 1:5
	# (1:5)[5]
	# (1:5)[3]
	# (1:5)[1]
	
	# using above limits to lookup ordered roots of first level bootstrap estimates
	l_ref_root = df_ref_quants$root_pivot_0_b[floor(l_ref)]
	u_ref_root = df_ref_quants$root_pivot_0_b[ifelse(floor(u_ref)==0,1,floor(u_ref))]
	
	
	# df_est_samp_orig = compute_quants(df_use)
	
	ll_dtbs_pivot = df_est_samp_orig$est_samp - l_ref_root*df_est_samp_orig$se_samp
	ul_dtbs_pivot = df_est_samp_orig$est_samp - u_ref_root*df_est_samp_orig$se_samp
	
	est_dubboot_out = data.frame(est_samp_whole=df_est_samp_orig$est_samp,
															 ll_dtbs_pivot,ul_dtbs_pivot)
	
	return(est_dubboot_out)
}
