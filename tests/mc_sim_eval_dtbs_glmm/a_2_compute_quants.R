
compute_quants = function(df_4_estimate){
	require(GLMMadaptive)
	# df_4_estimate = df_use2
	
	fm <- GLMMadaptive::mixed_model(fixed = Y ~ -1 + X, 
																	random = ~ 1 | id_cluster_grpsing,
																	data = df_4_estimate,
																	iter_EM = 10,
																	max_coef_value=10,
																	family = binomial())
	
	coef_marg = GLMMadaptive::marginal_coefs(fm,std_errors=TRUE,cores=1)
	out_coef_marg = coef(coef_marg)[1:2]
	names(out_coef_marg) = c('est_samp','se_samp')
	
	# coef_marg_sand = GLMMadaptive::marginal_coefs(fm,std_errors=TRUE,cores=1,
	# 																							sandwich=TRUE)
	# out_coef_marg_sand = coef(coef_marg_sand)[1:2]
	# names(out_coef_marg_sand) = c('est_samp','se_samp')
	# 
	# 
	# out_coef_fixef = summary(fm)$coef_table[1,1:2]
	# names(out_coef_fixef) = c('est_samp','se_samp')
	# 
	# out_coef_3type = list(out_coef_fixef=out_coef_fixef,
	# 											out_coef_marg=out_coef_marg,
	# 											out_coef_marg_sand=out_coef_marg_sand)
	# return(out_coef_3type)
	return(out_coef_marg)
}

