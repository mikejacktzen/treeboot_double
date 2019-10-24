

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
	
	
	# df_est_samp_orig = compute_quants_possibly(df_use)
	
	ll_dtbs_pivot = df_est_samp_orig$est_samp - l_ref_root*df_est_samp_orig$se_samp
	ul_dtbs_pivot = df_est_samp_orig$est_samp - u_ref_root*df_est_samp_orig$se_samp
	est_dubboot_out = data.frame(est_samp=df_est_samp_orig$est_samp,
															 mean_est_b = mean(df_ref_quants$est_b),
															 ll_dtbs_pivot,ul_dtbs_pivot)
	
	return(est_dubboot_out)
}