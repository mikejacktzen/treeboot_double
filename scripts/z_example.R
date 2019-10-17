# https://pdfs.semanticscholar.org/46b5/2cb68dc22b1293da76b46b66ae1629ea8a43.pdf


require(RDStreeboot)
data(faux.network)
## draw RDS from network
set.seed(123);samp <- sample.RDS(faux.network$traits, faux.network$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)

source('scripts/remap_tree.R')


# select id bootstraps ----------------------------------------------------

# level 1

B1=200
set.seed(4321);id_boot_1 = RDStreeboot:::.TBS(samp, B=B1)
str(id_boot_1,1)

# level 2
list_tib = vector(mode='list',length=B1)
B2=10

for(b in seq_along(id_boot_1)){
	# b=10
  # id_boot_1=samp_tb[[b]]  #is the b-th set of ids from the 1st level bootstrap
	tree_info_b1 = remap_tree(id_boot_desc_b=id_boot_1[[b]],
														df_edges_ances_elig = samp$edges)
	# str(tree_info_b1,1)
	
	# Error in samp.adj.mat[cbind(samp$edges$node1, samp$edges$node2)] <- T : 
	# 	subscript out of bounds
	# id_dubboot_from_b1 = RDStreeboot:::.TBS(tree_info_b1, B=B2)
	
	TBS_possibly = purrr::possibly(RDStreeboot:::.TBS,NULL)
	id_dubboot_from_b1 = TBS_possibly(tree_info_b1, B=B2)
	
	tab_b_bb = tibble::tibble(ids_b=list(id_boot_1[[b]]),
														ids_bb=list(id_dubboot_from_b1))
	list_tib[[b]] = tab_b_bb
}

str(list_tib,2)
# length(list_tib)

source('scripts/c_estimates_after_dubboot.R')
# View(compute_quants)
compute_quants_possibly = purrr::possibly(compute_quants,NA)

df_orig = samp$traits
est_samp_orig = compute_quants(df_orig)

list_tib_b_bb = list_tib

str(list_tib_b_bb,2)
View(list_tib_b_bb[[1]])
View(list_tib_b_bb[[1]][1])
View(list_tib_b_bb[[1]][[2]])

# list_tib_b_bb[[1]][,'ids_b']
# list_tib_b_bb[[1]][,'ids_bb']
# list_tib_b_bb[[2]]
# str(list_tib_b_bb,2)

# compute estimates -------------------------------------------------------
# using pre-sampled ids

list_u_all = vector(mode='list',length=B1)

for(b in seq_along(id_boot_1)){
	cat(paste('iter b:',b,' '))
	
	# b=1
	# b=4
	# quantities from 1st level bootstrap -------------------------------------
	
	ind_b = unlist(list_tib_b_bb[[b]][,'ids_b'])
	# quant_boot_lvl_1 = compute_quants(df_4_estimate = df_orig[ind_b,])
	quant_boot_lvl_1 = compute_quants_possibly(df_4_estimate = df_orig[ind_b,])
	

	# equation 3 paper
	est_0 = est_samp_orig$est_samp
	root_pivot_0_b = (quant_boot_lvl_1$est_samp-est_0)/quant_boot_lvl_1$se_samp
	
	# quantities from 2nd level bootstrap -------------------------------------
	# list_ind_bb_from_b = unlist(unlist(list_tib_b_bb[[b]][,'ids_bb'],recursive = FALSE),FALSE)
	
	list_ind_bb_from_b = unlist(list_tib_b_bb[[b]][['ids_bb']],recursive = FALSE)
	
	
	# equation before (5)
	if(any(is.null(list_ind_bb_from_b))==TRUE){
		u_b=NA
	}else{
	quant_boot_lvl_2 = lapply(list_ind_bb_from_b,
														#list_ind_bb_from_b[[ind_good]],
														FUN=function(xx){
															# compute_quants(df_4_estimate = df_orig[unlist(xx),])
															compute_quants_possibly(df_4_estimate = df_orig[unlist(xx),])
														})
		
		# t_b_bb = data.frame(do.call(rbind,quant_boot_lvl_2))
		# t_b = quant_boot_lvl_1[,'test_stat_samp']
		# u_b = mean(t_b_bb <= t_b,na.rm = TRUE) 
		
		est_b_bb = data.frame(do.call(rbind,quant_boot_lvl_2))[,'est_samp']
		se_b_bb = data.frame(do.call(rbind,quant_boot_lvl_2))[,'se_samp']
		
		root_pivot_b_bb = (est_b_bb - quant_boot_lvl_1$est_samp)/se_b_bb
			
		# equation (5)
		# u_b = pivot_b  = z_b
		u_b = mean(na.omit(root_pivot_b_bb) <= root_pivot_0_b)
		list_u_all[[b]] = data.frame(u_b=u_b,root_pivot_0_b=root_pivot_0_b)
	}
}

ref_quants = do.call(rbind,list_u_all)
str(list_u_all,1)
mean(unlist(lapply(list_u_all,is.null)))
# B1_na = sum(unlist(lapply(list_u_all,is.null)))

# arrange() ;+ ordered / sort 
ref_quants = ref_quants %>% arrange(u_b) %>% na.omit()
B1_na = nrow(ref_quants)
B1_na

# reference_pivot_sort = sort(unlist(list_u_all))
alpha=0.05

# https://pdfs.semanticscholar.org/46b5/2cb68dc22b1293da76b46b66ae1629ea8a43.pdf

# all the effort is to obtain reference distribution
# roots 'l_ref' and 'l_ref_root

# limits of pivotal reference distribution
# l_ref = B1_na*quantile(ref_quants$u_b,(1-alpha/2)*B1_na*1e-2)
# u_ref = B1_na*quantile(ref_quants$u_b,(alpha/2)*B1_na*1e-2)

l_ref = B1_na*quantile(ref_quants$u_b,(1-alpha/2))
u_ref = B1_na*quantile(ref_quants$u_b,(alpha/2))

# floor(l_ref)

# floor(u_ref)

# 1:5
# (1:5)[5]
# (1:5)[3]
# (1:5)[1]

# using above limits to lookup ordered roots of first level bootstrap estimates
l_ref_root = ref_quants$root_pivot_0_b[floor(l_ref)]
u_ref_root = ref_quants$root_pivot_0_b[ifelse(floor(u_ref)==0,1,floor(u_ref))]


# est_samp_orig = compute_quants_possibly(df_orig)

ll_dtbs_pivot = est_samp_orig$est_samp - l_ref_root*est_samp_orig$se_samp
ul_dtbs_pivot = est_samp_orig$est_samp - u_ref_root*est_samp_orig$se_samp
data.frame(est_samp=est_samp_orig$est_samp,ll_dtbs_pivot,ul_dtbs_pivot)
B1_na
ref_quants$root_pivot_0_b %>% hist()
ref_quants$u_b %>% hist()


# deprecated


# all the effort is to obtain 'q_hat'
# q_hat_025 = quantile(unlist(list_u_all),0.025,na.rm = TRUE)
# q_hat_975 = quantile(unlist(list_u_all),0.975,na.rm = TRUE)
# 
# 
# ll = est_samp_orig$est_samp - q_hat_975*est_samp_orig$se_samp
# ul = est_samp_orig$est_samp + q_hat_025*est_samp_orig$se_samp
# data.frame(est_samp=est_samp_orig$est_samp,ll,ul)
# est_samp_orig



# comp_on_ss = function(xx){
# 	if(is.null(xx)==TRUE){
# 		return(data.frame(est_samp=NA,
# 							 se_samp=NA,
# 							 test_stat_samp=NA))
# 	}else{
# 	# compute_quants(df_4_estimate = df_orig[unlist(xx),])
# 	return(compute_quants(df_4_estimate = df_orig[unlist(xx),]))
# 	}
# }

# quant_boot_lvl_2 = list_ind_bb_from_b %>% purrr::map(comp_on_ss)
