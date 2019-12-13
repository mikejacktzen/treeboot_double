

form_int_dubboot = function(df_est_samp_orig,
														df_ref_quants,alpha=0.05){
	
	
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