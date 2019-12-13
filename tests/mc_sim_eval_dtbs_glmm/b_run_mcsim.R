# Data Generating Processs to evaluate performance

# 1) list_param_rds


# batch submit num_mc_sim=1 iteration
# see how long it takes (hoffman will give you the .log file)
# keep track how many cores you used
# fake example: 8 cores takes 20 minutes for num_mc_sim=1

# Population
require(plyr)
require(dplyr)
require(RDStreeboot)


# generate pop ----------------------------------------------

# below two mimic dataset
# num.samp_use = c(4)  # max recruits
# num.prob_use = c(0,c(1/4,1/4,1/4,1/4))  # recruit probab

# beta <- c(0,1,2) # Y~X fixed effects coefficient scenarios
# n_0_use = c(100,200,300)
# num.seeds_use = c(5,10,15)

beta <- c(1) # Y~X fixed effects coefficient scenarios
n_0_use = c(100)
num.seeds_use = c(5)

list_param_rds = expand.grid(data.frame(n_use=n_0_use,num.seeds_use,beta))
rm(beta,n_0_use,num.seeds_use)

source("~/a_0_dtbs_helpers_all.R")
source("~/a_1_data_generating_process.R")
source("~/a_2_compute_quants.R")
args(gen_data_rds)


library(foreach)
library(doParallel)

# set number of core to pre requested numbre of cores -----------------

num_core=16
cl <- parallel::makeCluster(num_core)
doParallel::registerDoParallel(cl)
# stopCluster(cl)

B1s_pick = c(10)  # B1=B2
num_mc_sim_pick=16

# purrr::pmap(list_param_rds,.f=one_popsim_eval_dtbs_b1b2)

one_popsim_eval_dtbs_b1b2 = function(n_use,num.seeds_use,
																		 beta,
																		 B1_pick,B2_pick,
																		 num_mc_sim){
	# n_use=list_param_rds[1,'n_use']
	# num.seeds_use = list_param_rds[1,'num.seeds_use']
	# beta=list_param_rds[1,'beta']

	# one_int_sim = vector(mode='list',length = num_mc_sim)
	# for(s in 1:num_mc_sim){
	require('dplyr')
	
	one_int_sim <- foreach(s = 1:num_mc_sim,
												 .packages=c('plyr','dplyr','tibble',
												 						'GLMMadaptive'
												 ),
												 .export=c(
												 	'gen_data_rds',
												 	'rank_ids_tree_info',
												 	'get_ids_dtbs',
												 	'resample_id_tbs_dub',
												 	'remap_tree',
												 	'comp_est_over_ids_b_bb_onerow_serial',
												 	'form_int_dubboot',
												 	'compute_quants'
											)) %dopar% {
												
		# s=1
		# message(paste('B1',B1_pick,'iter_sim',s,'of',num_mc_sim,'n_use',n_use,'num.seeds',num.seeds_use,'beta',beta))
		
		# df_sim_one = purrr::pmap(.f=gen_data_rds,list_param_rds[1,]) %>% unlist(FALSE)
		
		df_sim_one = gen_data_rds(n_use=n_use,num.seeds_use=num.seeds_use,beta=beta)
		df_4_estimate_sim = df_sim_one$df_4_estimate
		
		df_est_samp_orig = compute_quants(df_4_estimate = df_4_estimate_sim)
		
		
		# ids_resamp_b_bb = get_ids_dtbs(10,10,df_sim_one)
		# B1_pick <- B2_pick <- 10
		ids_resamp_b_bb = get_ids_dtbs(B1_pick,B2_pick,df_sim_one)
		
		ind_skip = unlist(purrr::map(unlist(ids_resamp_b_bb[,2],FALSE),
																 .f=function(xx){is.null(unlist(xx))}))
		
		# any(ind_skip)
		sstest = ids_resamp_b_bb[!ind_skip,]
		
		
		# old serial for level 1
		# ests_dtbs = vector(mode='list',length = nrow(sstest))
		# for(ii in 1:nrow(sstest)){
		
# serial for lvl 1 and lvl 2 -------------------------------------------
		
		# par for level 1
		# require(foreach)
		#' ests_dtbs <- foreach(ii = 1:nrow(sstest),
		#' 										 .packages=c('plyr','dplyr','tibble',
		#' 										 						'GLMMadaptive'
		#' 										 ),
		#' 										 .export=c(#'brm_comp',
		#' 										 	#'df_est_samp_orig',
		#' 										 	#'comp_est_over_ids_b_bb_onerow',
		#' 										 	'comp_est_over_ids_b_bb_onerow_serial',
		#' 										 	'compute_quants')) %dopar% 
		
		
		ests_dtbs = vector(mode='list',length = nrow(sstest))
		
		for(ii in 1:nrow(sstest)){
											
			# message('level b resample iter ',ii)
			# args(comp_est_over_ids_b_bb)
			# need to define possibly here,
		
			compute_quants_possibly=purrr::possibly(compute_quants,NA)
			
			# comp_est_over_ids_b_bb_onerow() internally uses parLapply()
			# during 2nd level estimates
			
			# source('/a_5_comp_est_over_ids_b_bb_onerow_serial.R')
			comp_est_over_ids_b_bb_onerow_possibly = purrr::possibly(comp_est_over_ids_b_bb_onerow_serial,
																															 otherwise = NA)
			
			# comp_est_over_ids_b_bb_onerow_possibly = purrr::possibly(comp_est_over_ids_b_bb_onerow_3type,otherwise = NA)
			
			# ii=1
			out_b_bb = comp_est_over_ids_b_bb_onerow_possibly(ids_b_bb_one=(sstest[ii,]),  # index rows of tibble seems to work
																							 # ids_b_bb_one=unlist(sstest[ii,]),
																							 df_use=df_4_estimate_sim,
																							 df_est_samp_orig=df_est_samp_orig,
																							 # cl=cl,
																							 # iter_pick=10,
																							 # iter_pick=5000,
																							 # compute_quants=compute_quants
																							 compute_quants=compute_quants_possibly
																							 )
			# via serial for loop pre allocated storage
			ests_dtbs[[ii]] = out_b_bb
			
			# via foreach()
			# return(out_b_bb)  
			}
		
		# str(ests_dtbs,1)
		df_ref_quants_sim_one = do.call(rbind,ests_dtbs)
		
		df_ref_quants_sim_one_non_na = na.omit(df_ref_quants_sim_one)
		
		# dim(df_ref_quants_sim_one)
		
		int_dtbs = form_int_dubboot(df_est_samp_orig=df_est_samp_orig,
																alpha=0.05,
																df_ref_quants=df_ref_quants_sim_one_non_na)

		# one_int_sim[[s]] = int_dtbs
		
		# one option, externally write.csv(int_dtbs_s.csv)
		
		return(int_dtbs)
	}
	
	# one_int_sim[[s]] = int_dtbs
	
	
	df_int_all = do.call(rbind,one_int_sim) %>% 
		mutate(beta_inside=ifelse((ll <= beta)&&(ul >= beta),1,0))
	
	# mean(one_int_sim$length)
	# mean(one_int_sim$beta_inside)
	
	mean_int_length = mean(df_int_all$length_int)
	prop_cover = mean(df_int_all$beta_inside)
	
	out_res = data.frame(n_use,num.seeds_use,
						 beta,
						 B1_pick,B2_pick,
						 num_mc_sim,
						 mean_int_length,prop_cover,
						 n_sim_non_na=nrow(na.omit(df_int_all)))
	
	return(out_res)
	
}





# gen dtbs context --------------------------------------------------------
# 
# don't care when B2 < B1, so repeat B2=B1
# B1s = c(100,200,300)
# B1s = c(20,30,40)
# B1s_pick = c(10)  # B1=B2
# num_mc_sim_pick=16
	
list_param_dtbs = data.frame(B1_pick=B1s_pick,B2_pick=B1s_pick)
# list_param_dtbs[1,]

results_all = vector(mode='list',length = nrow(list_param_dtbs))

system.time(out_timing <- for(j in 1:nrow(list_param_dtbs)){
# list_param_dtbs[b,]

	one_popsim_eval_dtbs_b1b2_fix = function(b1,b2){
		eval_int_sim_one_b = purrr::pmap(list_param_rds,
																		 # list_param_rds[c(1,10),],
																		 .f=one_popsim_eval_dtbs_b1b2,
																		 num_mc_sim=num_mc_sim_pick,
																		 # num_mc_sim=8,
																		 B1_pick=b1,B2_pick=b2)
		return(eval_int_sim_one_b)
	}

	# j=2
	results_all_oneb = one_popsim_eval_dtbs_b1b2_fix(b1=list_param_dtbs[j,1],
																									 b2=list_param_dtbs[j,2])
	results_all[[j]] = results_all_oneb
})

results_all

# > results_all
# [[1]]
# [[1]][[1]]
# n_use num.seeds_use beta B1_pick B2_pick num_mc_sim mean_int_length prop_cover n_sim_non_na
# 1   100             5    1      50      50          8       0.7480822          0            8

# 8 cores, 8 sims , serial B1=50, serial B2=50

# user  system elapsed 
# 0.57    0.64 4858.78

# > 4858/60/60
# [1] 1.349444
# eg 1.3 hours for 1 sim of (2500) B1*B2 iters
# eg ~1.3 hours for 8 parallel sim of (2500) B1*B2 iters

# probably 5 hours if B1=B2=100 for 1 sim

# ~16 cores give you 64 parallel sims in 24 hours

# user  system elapsed 
# 4.09    0.28 1985.89 

# yes perallel at top level and serial lower level faster because no wait time for group completion

# in that case, fastest approach would be to parallel at the mc sim level and serial lvl1 lvl2

# for package, par at lvl 1, since end users will not typicaly embed use in outter mc simulation

# eval summary --------------------------------------------------------------------

# 
# results_all[[1]]
# > results_all
# [[1]]
# [[1]][[1]]
# n_use num.seeds_use beta B1_pick B2_pick num_mc_sim mean_int_length prop_cover n_sim_non_na
# 1   100             5    1     100     100          2       0.7474501          1            2


