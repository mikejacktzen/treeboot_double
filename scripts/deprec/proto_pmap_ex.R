# list_tib from 'z_example.R'
# list_tib_b_bb = list_tib
head(list_tib_b_bb)

test = rbind(list_tib_b_bb[[1]],list_tib_b_bb[[2]])
test = do.call(rbind,list_tib_b_bb)


str(test,2)
dim(test)

str(test$ids_b)
str(test$ids_bb)

est_samp_orig = compute_quants(df_use)


# helper function sub4loop ------------------------------------------------


# assume 'est_samp_orig' is in environment
str(est_samp_orig)
head(df_use)

compute_est_tib = function(ids_b,ids_bb,...){
	# compute_est_tib = function(ids_b,ids_bb){
	
	# b=1
	
	# pre sort by id_ord
	# since our goal of using id_ord returned from bootstrapping
	# is to easily use it as row ids to subset df
	
	df_use = arrange(df_use,id_ord)
	
	## for loop notation
	# df_resamp = df_use[test$ids_b[[1]],]
	
	# if not ordered, need left join
	# df_resamp = left_join(by='pid',
	# 											x=data.frame(pid=ids_b),
	# 											y=df_use)
	
	df_resamp = df_use[ids_b,]
	
	quant_boot_lvl_1 = compute_quants_possibly(df_4_estimate = df_resamp)
	
	# quant_boot_lvl_1 = compute_quants_possibly(df_4_estimate = df_use[ids_b,])
	
	est_b = quant_boot_lvl_1$est_samp
	se_b = quant_boot_lvl_1$se_samp
	
	root_pivot_0_b = (est_b - est_samp_orig$est_samp)/se_b
	
	
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
																
																compute_quants_possibly(df_4_estimate = df_resamp)
																# compute_quants_possibly(df_4_estimate = df_use[unlist(xx),])
															})
		
		est_b_bb = data.frame(do.call(rbind,quant_boot_lvl_2))[,'est_samp']
		se_b_bb = data.frame(do.call(rbind,quant_boot_lvl_2))[,'se_samp']
		
		root_pivot_b_bb = (est_b_bb - est_b)/se_b_bb
		
		# equation (5)
		# u_b = pivot_b  = z_b
		u_b = mean(na.omit(root_pivot_b_bb) <= root_pivot_0_b)
	}
	
	out_tib = tibble(root_pivot_0_b=root_pivot_0_b,
									 u_b=u_b,
									 ids_b=list(ids_b))
	
	return(out_tib)
	
	# return(cbind(length(ids_b),sapply(ids_bb,length)))
	
	# return(cbind(length(unlist(ids_b)),length(unlist(ids_bb))))()
	
}


library(purrr)
dim(test)

test2 = test[1:10,] %>% pmap(compute_est_tib) %>% do.call(rbind,.)

compute_est_tib_possibly = possibly(compute_est_tib,NA)

test2 = test %>% pmap(compute_est_tib_possibly) %>% do.call(rbind,.)

df_out_est = test2

# test2 = test %>% pmap_dfr(compute_est_tib) %>% do.call(rbind,.)

# install.packages('furrr')
library(furrr)
# ?multisession
plan(multisession, workers = 4)

df_out_est = test %>% future_pmap(compute_est_tib,.progress = TRUE) %>% do.call(rbind,.)

df_out_est = test %>% future_pmap(compute_est_tib_possibly,.progress = TRUE) %>% do.call(rbind,.)

plan(sequential)  # close background workers

df_out_est$root_pivot_0_b %>% hist()
df_out_est$u_b %>% hist()


ref_quants = df_out_est %>% arrange(u_b) %>% na.omit()

ref_quants %>% filter(u_b > 0.01) %>% filter(u_b < 0.99)

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


# est_samp_orig = compute_quants_possibly(df_use)

ll_dtbs_pivot = est_samp_orig$est_samp - l_ref_root*est_samp_orig$se_samp
ul_dtbs_pivot = est_samp_orig$est_samp - u_ref_root*est_samp_orig$se_samp
data.frame(est_samp=est_samp_orig$est_samp,ll_dtbs_pivot,ul_dtbs_pivot)

# B1_na
# ref_quants$root_pivot_0_b %>% hist()
# ref_quants$u_b %>% hist()

# df_out_est %>% filter(is.na(u_b))
# ids_b_null = df_out_est %>% filter(is.na(u_b)) %>% select(ids_b)
# ids_b_null[1,] %>% unlist()
# ids_b_null[2,] %>% unlist()

