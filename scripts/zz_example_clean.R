
# level 0 whole dataset ---------------------------------------------------
library(dplyr)
require(RDStreeboot)


# simple tree to easily see results
library(igraph)
g0 <- graph_from_literal(20,
												 2 -+ 3 -+ 4 -+ 6,
												 4 -+ 7,
												 5 -+ 11)
pid = c(20,2,3,4,6,7,5,11)

# variables
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

# note that samp$nodes has unique(union(nodes,unlist(edges)))

tree_info_samp_orig = samp0; rm(samp0)
str(tree_info_samp_orig)

# step 0, 
# have user pre map ordered ids
# two arguments user needs at step 0
# tree_info_samp_orig
# df_orig

str(tree_info_samp_orig)
str(df_orig)


# creating this key needs population of all nodes uniqued
all_nodes = unique(union(tree_info_samp_orig$nodes,unlist(tree_info_samp_orig$edges)))

df_key_id_orig_w_rank = data.frame(id_orig=sort(all_nodes),
																			 id_rank=seq_along(all_nodes))

head(df_key_id_orig_w_rank)
tail(df_key_id_orig_w_rank)

# user needs to specify how person ids map to node ids
# usually the same, person id := node id

library(dplyr)
df_use = left_join(df_orig,df_key_id_orig_w_rank,
									 c('pid'='id_orig'))

head(df_use)


source('scripts/a_rank_ids_tree_info.R')

tree_info_samp_ranked = rank_ids_tree_info(tree_info_samp_orig,df_key_id_orig_w_rank)
str(tree_info_samp_orig)
str(tree_info_samp_ranked)

# tree_info_samp_ranked$edges %>% View()

# resample ids ------------------------------------------------------------

# level 1
B1=50
set.seed(4321);id_boot_1_ranked = RDStreeboot:::.TBS(tree_info_samp_ranked, B=B1)
str(id_boot_1_ranked)

# set.seed(4321);id_boot_1_orig = RDStreeboot:::.TBS(tree_info_samp_orig, B=B1)

# level 2

source('scripts/a_remap_tree.R')
source("scripts/a_resample_id_tbs_dub.R")

# ?resample_id_tbs_dub

# resample_id_tbs_dub(id_boot_1_ranked[[1]],B2=10)
# resample_id_tbs_dub(id_boot_1_ranked[[1]],B2=5)
# resample_id_tbs_dub(id_boot_1_ranked[[2]],B2=5)


B2_pick = 50
ids_resamp_b_bb = id_boot_1_ranked %>% 
	purrr::map(.f=resample_id_tbs_dub,B2=B2_pick) %>% 
	do.call(rbind,.)

ids_resamp_b_bb[1,][[1]]
ids_resamp_b_bb[1,][[2]]

str(ids_resamp_b_bb,2)

# compute quantities ------------------------------------------------------

# user needs to code up a function that computes their desired estimate 

# compute_quants = function(){
# 	cat('i want this estimate and its standard error)
#		# ...
# 	return(data.frame(est_samp,se_samp,test_stat_samp))
# }

# example defined in 
source('scripts/foo_my_estimate_func.R')
# View(compute_quants)
library(purrr)
compute_quants_possibly = purrr::possibly(compute_quants,otherwise=NA)

# runs compute_quants_possibly over ids level 1 b and level 2 bb
source('scripts/b_comp_est_over_ids_b_bb.R')

args(comp_est_over_ids_b_bb)
# note, needs user def function 'compute_quants' as argument
# comp_est_over_ids_b_bb(compute_quants=compute_quants, ...)

comp_est_over_ids_b_bb_possibly = possibly(comp_est_over_ids_b_bb,
																		otherwise=NA)

head(df_use)


# precompute estimate of whole dataset
df_est_samp_orig = compute_quants_possibly(df_4_estimate = df_use)


# compute estimates over each resample id_b_bb
est_b_bb_test = ids_resamp_b_bb[1:5,] %>% 
	pmap(.f=comp_est_over_ids_b_bb_possibly,  
			 df_use=df_use,
			 df_est_samp_orig=df_est_samp_orig,
			 compute_quants=compute_quants_possibly) %>%
	do.call(rbind,.)

source("scripts/b_form_int_dubboot.R")
# mean(est_b_bb_test$est_b)

form_int_dubboot(df_est_samp_orig=df_est_samp_orig,
								 df_ref_quants=na.omit(est_b_bb_test))



# example 2 ---------------------------------------------------------------
require(RDStreeboot)

# draw RDS from network
data(faux.network)
set.seed(123);samp0 <- sample.RDS(faux.network$traits,
																	faux.network$adj.mat,
																	100, 2, 3, c(0,1/3,1/3,1/3),
																	TRUE)
df_orig = samp0$traits

df_orig = tibble::rownames_to_column(df_orig) %>% 
	mutate(rowname=as.numeric(gsub(rowname,
																 pattern="\\..*",
																 replacement=""))) %>% 
	arrange(rowname)

tree_info_samp_orig = samp0; rm(samp0)
str(tree_info_samp_orig)

all_nodes = unique(union(tree_info_samp_orig$nodes,unlist(tree_info_samp_orig$edges)))
df_key_id_orig_w_rank = data.frame(id_orig=sort(all_nodes),
																	 id_rank=seq_along(all_nodes))

df_use = left_join(df_orig,df_key_id_orig_w_rank,
									 c('rowname'='id_orig'))

tree_info_samp_ranked = rank_ids_tree_info(tree_info_samp_orig,df_key_id_orig_w_rank)

# level 1
B1=500
set.seed(4321);id_boot_1_ranked = RDStreeboot:::.TBS(tree_info_samp_ranked, B=B1)
str(id_boot_1_ranked)

# level 2
B2_pick = 500
ids_resamp_b_bb = id_boot_1_ranked %>% 
	purrr::map(.f=resample_id_tbs_dub,B2=B2_pick) %>% 
	do.call(rbind,.)
head(ids_resamp_b_bb)

# .TBS() sometimes unable to 2nd level resample 
# based on provided 1st level resamples
sum(unlist(lapply(ids_resamp_b_bb$ids_bb,is.null)))

# compute estimates in parallel using furrr
# install.packages('furrr')
library(furrr)
# ?multisession
plan(multisession, workers = 8)

df_est_samp_orig = compute_quants_possibly(df_4_estimate = df_use)

est_b_bb = ids_resamp_b_bb %>% 
	future_pmap(.f=comp_est_over_ids_b_bb_possibly,  
							.progress = TRUE,
							df_use=df_use,
							df_est_samp_orig=df_est_samp_orig,
							compute_quants=compute_quants_possibly) %>%
	do.call(rbind,.)

# close background workers
plan(sequential) 

est_b_bb$root_pivot_0_b %>% hist()
est_b_bb$u_b %>% hist()

form_int_dubboot(alpha=0.05,
								 # alpha=0.5,
								 df_est_samp_orig=df_est_samp_orig,
								 df_ref_quants=na.omit(est_b_bb))

