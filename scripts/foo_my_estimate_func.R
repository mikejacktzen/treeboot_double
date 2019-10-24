# helper functio to compute quants quantities  ----------------------------

# toy ?lm() example

compute_quants = function(df_4_estimate){
	# df_4_estimate = samp$traits
	# df_reg = samp$traits
	
	# note the formula and name of terms are hardcoded
	# in body here (can change later to pass in formula)
	
	lm_samp = lm(data=df_4_estimate,X~Y+Z)
	
	ind_estimand = 2
	est_samp = coef(lm_samp)[ind_estimand]
	se_samp = sqrt(diag(vcov(lm_samp))[ind_estimand])
	test_stat_samp = est_samp/se_samp
	# return(data.frame(est_samp,se_samp,test_stat_samp,t(confint(lm_samp)[ind_estimand,])))
	return(data.frame(est_samp,se_samp))
}

# # quantities from original sample dataset (level 0) -----------------------------------------
# 
# require(RDStreeboot)
# data(faux.network)
# ## draw RDS from network
# set.seed(123);samp <- sample.RDS(faux.network$traits, faux.network$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)
# 
# df_orig = samp$traits
# quant_samp_orig = compute_quants(df_4_estimate = df_orig)
# quant_samp_orig
# est_0 = quant_samp_orig$est_samp

