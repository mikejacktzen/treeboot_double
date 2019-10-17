

# quantities from original sample -----------------------------------------

# df_reg = samp$traits
# head(df_reg)
# lm_samp_orig = lm(data=df_reg,X~Y+Z)
# 
# ind_estimand = 2
# est_samp_orig = coef(lm_samp_orig)[ind_estimand]
# se_samp_orig = sqrt(diag(vcov(lm_samp_orig))[ind_estimand])
# test_stat_samp_orig = est_samp_orig/se_samp_orig

# toy ?lm() example

compute_quants = function(df_4_estimate){
	# df_4_estimate = samp$traits
	# df_reg = samp$traits
	lm_samp = lm(data=df_4_estimate,X~Y+Z)
	
	ind_estimand = 2
	est_samp = coef(lm_samp)[ind_estimand]
	se_samp = sqrt(diag(vcov(lm_samp))[ind_estimand])
	test_stat_samp = est_samp/se_samp
	# return(data.frame(est_samp,se_samp,test_stat_samp,t(confint(lm_samp)[ind_estimand,])))
	return(data.frame(est_samp,se_samp,test_stat_samp))
}

# # quantities from original sample dataset (level 0) -----------------------------------------
# 
# df_orig = samp$traits
# quant_samp_orig = compute_quants(df_4_estimate = df_orig)
# est_0 = quant_samp_orig$est_samp

# 
# 
# # resample ids for level 1 and level 2 (pre-process step) -----------------
# 
# 
# # quantities from 1st level bootstrap -------------------------------------
# 
# # simple classic bootstrap
# set.seed(1234);ind_test=sample(1:nrow(samp$traits),replace=TRUE)
# compute_quants(df_4_estimate = samp$traits[ind_test,])
# 
# ind_test=sample(1:nrow(samp$traits),replace=TRUE)
# quant_boot_lvl_1 = compute_quants(df_4_estimate = df_orig[ind_test,])
# 
# # tree bootstrap
# 
# # quantities from 2nd level bootstrap -------------------------------------
# 
# str(size2,2)
# df_ids_all = size2
# df_ids_all[1,]
# 
# df_ids_all[1,][['ids_tbs1']]
# ind_temp = unlist(df_ids_all[1,][['ids_tbs1']])
# quant_boot_lvl_1 = compute_quants(df_4_estimate = df_orig[ind_temp,])
# 
# df_ids_all[1,][['ids_tbs2']]
# ind_temp = unlist(df_ids_all[1,][['ids_tbs2']],FALSE)
# quant_boot_lvl_2 = lapply(ind_temp,
# 													FUN=function(xx){
# 														compute_quants(df_4_estimate = df_orig[unlist(xx),])
# 														})
# 
# t_b_bb=data.frame(do.call(rbind,quant_boot_lvl_2))[,'test_stat_samp']
# 
# t_b=quant_boot_lvl_1[,'test_stat_samp']
# 
# u_b = mean(t_b_bb <= t_b)
# 
# return(u_b)
# # u_b_all
# 
# # all the effort is to obtain 'q_hat'
# q_hat = quantile(u_b_all,0.25)
# 
# est_samp_orig - qt(q_hat)*se_samp_orig
# est_samp_orig + qt(q_hat)*se_samp_orig


##### Code Example 5 #####
# See comments at the head of Code Example 1

# Calculate p-values by the "double bootstrap", correcting for the effects of
# parameter estimation

# Inputs: function to calculate a test statistic (test)
# Function to produce a surrogate data set (simulator)
# Number of first-level bootstrap replicates (B1)
# Number of second-level bootstrap replicates (B2)
# Function to estimate parameter from data (estimator)
# Empirical estimate of parameter (thetahat)
# Observed value of test statistic (testhat)
# Calls: rboot, boot.pvalue
# Presumes:  Output of simulator is formatted to be input for test
# test needs no extra arguments
# test returns a value for which the comparison >= is defined
# simulator takes an argument named theta
# The output of estimator has the right format to be the theta argument
# Output: The p-value
# doubleboot.pvalue <- function(test,simulator,B1,B2, estimator, 
# 															thetahat, testhat) {
# 	for (i in 1:B1) {
# 		xboot <- simulator(theta=thetahat)
# 		thetaboot <- estimator(xboot)
# 		testboot[i] <- test(xboot)
# 		pboot[i] <- boot.pvalue(test,simulator,B2,testhat=testboot[i],
# 														theta=thetaboot)
# 	}
# 	p <- (sum(testboot >= testhat)+1)/(B1+1)
# 	p.adj <- (sum(pboot <= p)+1)/(B1+1)
# }
# R exercise for the reader: replace the inner for() loop with something
# more vectorized