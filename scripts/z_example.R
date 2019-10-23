# https://pdfs.semanticscholar.org/46b5/2cb68dc22b1293da76b46b66ae1629ea8a43.pdf


require(RDStreeboot)

# data(faux.network)
# ## draw RDS from network
# set.seed(123);samp <- sample.RDS(faux.network$traits, 
# 																 faux.network$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)

library(igraph)
g0 <- graph_from_literal(20,
												 2 -+ 3 -+ 4 -+ 6,
												 4 -+ 7,
												 5 -+ 11)
pid = c(20,2,3,4,6,7,5,11)

eps = rnorm(0,n=length(pid),sd=10)
Z = rnorm(0,n=length(pid))
Y = rnorm(0,n=length(pid))
X = -2*Y + 3*Z + eps

df_orig = data.frame(pid,Y,X,Z)

plot(g0,layout=layout_as_tree)
list_v_and_e = igraph::as_data_frame(g0,what='both')
samp0 = list()
samp0$nodes = as.numeric(unlist(list_v_and_e$vertices,use.names = FALSE))
samp0$edges = data.frame(node1=as.numeric(list_v_and_e$edges$from),
												 node2=as.numeric(list_v_and_e$edges$to))

tree_info_samp_orig = samp0; rm(samp0)
str(tree_info_samp_orig)
# note that samp$nodes has unique(union(nodes,unlist(edges)))

# source('scripts/remap_tree.R')
source('scripts/a_remap_tree_func.R')


# select id bootstraps ----------------------------------------------------
# samp$edges
# setdiff(unlist(samp$edges),samp$nodes)
# maybe don't need nodes to include edges
# suspect yes nodes needs to be excluding edges


# step 0, have user pre map ordered ids
# two arguments user needs at step 0
# tree_info_samp_orig
# df_orig

tree_info_samp_orig
df_orig



# creating this key needs population of all nodes uniqued
df_key_id_orig_w_sort = data.frame(id_orig=sort(tree_info_samp_orig$nodes),
																	 id_ord=seq_along(tree_info_samp_orig$nodes))

# user needs to specify how person ids map to node ids
library(dplyr)
df_use = left_join(df_orig,df_key_id_orig_w_sort,
									 c('pid'='id_orig'))



# level 1

# pre sort to sequential ids
# helper function if user needs to convert tree info with sorted id values
# can skip if user already in sorted id format
sort_ids_tree_info = function(tree_info,df_key_id_orig_w_sort){
	# tree_info = samp
	# takeaway: need to pre sort nodes before using .TBS()
	# df_key_id_orig_w_sort = data.frame(id_orig=sort(tree_info$nodes),
	# 																	 id_ord=seq_along(tree_info$nodes))
	# if(orig_2_sort=TRUE){
		from_pick=df_key_id_orig_w_sort$id_orig  # original
		to_pick=df_key_id_orig_w_sort$id_ord  # sorted/ordered
	# }
	
	tree_info_id_sort = list()
	tree_info_id_sort$nodes = plyr::mapvalues(tree_info$nodes,
																						from=from_pick,
																						to=to_pick,
																						warn_missing=FALSE)
	
	tree_info_id_sort$edges = data.frame(sapply(tree_info$edges,
																							FUN=plyr::mapvalues,
																							from=from_pick,
																							to=to_pick,
																							warn_missing=FALSE))
	return(tree_info_id_sort)
}

tree_info_samp_sorted = sort_ids_tree_info(tree_info_samp_orig,df_key_id_orig_w_sort)

# nodes_sorted_nonedge = setdiff(samp_sorted$nodes,unlist(samp_sorted$edges))
# samp_sorted$nodes = unlist(nodes_sorted_nonedge)
# samp_sorted

# tests suggesting samp$nodes should be all unique(nodes,unlist(edges))
B1=50

set.seed(4321);id_boot_1_sorted = RDStreeboot:::.TBS(tree_info_samp_sorted, B=B1)
# set.seed(4321);id_boot_1 = RDStreeboot:::.TBS(samp, B=B1)

# do not need to map back if 
# map sorted its back to original ids
# can remain using sorted ids IF the original edge list to lookup is
# also using sorted id system
# remap_tree(id_boot_desc_b=id_boot_sorted,
# 					 df_edges_ances_elig = tree_info_samp_orig_sorted$edges)

# Optional work with original ids
id_boot_1_orig = lapply(id_boot_1_sorted,
												FUN=plyr::mapvalues,
												to=df_key_id_orig_w_sort$id_orig,  # reverse to/from
												from=df_key_id_orig_w_sort$id_ord,
												warn_missing=FALSE)

str(id_boot_1_sorted,1)
str(id_boot_1_orig,1)

id_boot_1_sorted[[35]]
id_boot_1_orig[[35]]

# level 2
list_tib = vector(mode='list',length=B1)
B2=10


# id_boot_1 = id_boot_1_orig
id_boot_1 = id_boot_1_sorted

for(b in seq_along(id_boot_1)){
	# b=1
  # id_boot_1=samp_tb[[b]]  #is the b-th set of ids from the 1st level bootstrap
	
	# require original ids to lookup original edges
	tree_info_b1 = remap_tree(id_boot_desc_b=id_boot_1[[b]],
														## if using original ids
														# df_edges_ances_elig = tree_info_samp_orig$edges  
														## if using sorted ids (less book keeping)
														df_edges_ances_elig = tree_info_samp_sorted$edges)
	
	# str(tree_info_b1,1)
	
	# Error in samp.adj.mat[cbind(samp$edges$node1, samp$edges$node2)] <- T : 
	# 	subscript out of bounds
	# id_dubboot_from_b1 = RDStreeboot:::.TBS(tree_info_b1, B=B2)
	
	## IF 'tree_info_b1' is using original ids
	## df_edges_ances_elig = tree_info_samp_orig$edges
	## sort again before using 2nd level .TBS
	
	# tree_info_b1_sorted = sort_ids_tree_info(tree_info_b1,df_key_id_orig_w_sort)
	
	
	TBS_possibly = purrr::possibly(RDStreeboot:::.TBS,NULL)
	id_dubboot_from_b1_sorted = TBS_possibly(tree_info_b1,
																					 # tree_info_b1_sorted,
																					 B=B2)
	
	
	tab_b_bb = tibble::tibble(ids_b=list(id_boot_1[[b]]),
														# ids_bb=list(id_dubboot_from_b1)
														ids_bb=list(id_dubboot_from_b1_sorted)
														)
	
	list_tib[[b]] = tab_b_bb
}

str(list_tib,3)
list_tib_b_bb = list_tib; rm(list_tib)
str(list_tib_b_bb,3)

## OPTIONAL: output map back to original ids
## can do this later outside id resampling
# id_dubboot_from_b1 = lapply(id_dubboot_from_b1_sorted,
# 														FUN=plyr::mapvalues,
# 														to=df_key_id_orig_w_sort$id_orig,  # reverse to/from
# 														from=df_key_id_orig_w_sort$id_ord,
# 														warn_missing=FALSE)


# length(list_tib)

# source('scripts/c_estimates_after_dubboot.R')
source('scripts/b_estimate_func.R')

# View(compute_quants)
compute_quants_possibly = purrr::possibly(compute_quants,NA)



# alternative 'proto_pmap_ex.R' -------------------------------------------


# for loop for estimates --------------------------------------------------


# df_orig = samp$traits
est_samp_orig = compute_quants(df_use)


str(list_tib_b_bb,2)
# View(list_tib_b_bb[[1]])
# View(list_tib_b_bb[[1]][1])
# View(list_tib_b_bb[[1]][[2]])

# table structure is tibble
# row 1 column 1 is a list of 1 vector
# row 1 column 2 is a list of B2-many vectors

list_tib_b_bb[[1]][,'ids_b']
list_tib_b_bb[[1]][,'ids_bb']

length((list_tib_b_bb[[1]][,'ids_b']))
length(unlist(list_tib_b_bb[[1]][,'ids_b']))

length((list_tib_b_bb[[1]][,'ids_bb']))
length(unlist(list_tib_b_bb[[1]][['ids_bb']],recursive = F))

# compute estimates -------------------------------------------------------
# using pre-sampled level-1 ids, 
# for loop
# run tbs to get level-2 ids 
# then compute required quantities of level 0, level 1, and level 2
# end loop

list_u_all = vector(mode='list',length=B1)

for(b in seq_along(id_boot_1)){
	cat(paste('iter b:',b,' '))
	
	# b=1
	# b=4
	# quantities from 1st level bootstrap -------------------------------------
	
	# consider converting node ids to row ids
	# then subset rows

	ind_b = unlist(list_tib_b_bb[[b]][,'ids_b'])
	# quant_boot_lvl_1 = compute_quants(df_4_estimate = df_use[ind_b,])
	df_resamp = left_join(by='pid',
												x=data.frame(pid=ind_b),
												y=df_use)
	# ind_b
	quant_boot_lvl_1 = compute_quants_possibly(df_4_estimate = df_resamp)
	
	# df_subset = dplyr::filter(df_use,nodes %in% ind_b)
	# quant_boot_lvl_1 = compute_quants_possibly(df_4_estimate = df_subset)
	
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
															FUN=function(xx){
																df_resamp = left_join(by='pid',
																					x=data.frame(pid=unlist(xx)),
																					y=df_use)
																compute_quants_possibly(df_4_estimate = df_resamp)
															})
		
		
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


# est_samp_orig = compute_quants_possibly(df_use)

ll_dtbs_pivot = est_samp_orig$est_samp - l_ref_root*est_samp_orig$se_samp
ul_dtbs_pivot = est_samp_orig$est_samp - u_ref_root*est_samp_orig$se_samp
data.frame(est_samp=est_samp_orig$est_samp,ll_dtbs_pivot,ul_dtbs_pivot)
B1_na
ref_quants$root_pivot_0_b %>% hist()
ref_quants$u_b %>% hist()


