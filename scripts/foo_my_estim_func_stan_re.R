
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

# creating this key needs population of all nodes uniqued
all_nodes = unique(union(tree_info_samp_orig$nodes,unlist(tree_info_samp_orig$edges)))

df_key_id_orig_w_rank = data.frame(id_orig=sort(all_nodes),
																	 id_rank=seq_along(all_nodes))
library(dplyr)
df_use = left_join(df_orig,df_key_id_orig_w_rank,
									 c('pid'='id_orig'))



form_cluster_id = function(igraph_to_cluster){
	require(igraph)
	require(dplyr)
	
	# treat each tree as a cluster
	graph_test_simplify = simplify(igraph_to_cluster)
	nodes_sort = V(graph_test_simplify) %>% as.integer(.)
	test_edgelist = data.frame(as_edgelist(graph_test_simplify),
														 stringsAsFactors = FALSE) %>% 
		mutate_all(.,.funs=as.integer)
	names(test_edgelist) = c('node1','node2')
	
	edgelist_sort = dplyr::arrange(test_edgelist,
																 node1,node2)
	
	########################################
	# use graph_test_simplify
	########################################
	# every vertex involved in an edge?
	# expect 216 singleton vertex
	sum(!(nodes_sort %in% c(edgelist_sort[,1],edgelist_sort[,2])))
	
	# for cluster assignment, eg id of random effect
	c1 <- igraph::clusters(graph_test_simplify,mode='weak')
	
	# c1$membership
	df_cluster = data.frame(id_cluster=seq_along(c1$csize),size_cluster=c1$csize)
	
	# c1$membership
	
	id_cluster_singleton = df_cluster %>% 
		filter(size_cluster==1) %>% select(id_cluster)
	
	df_membership_map = data.frame(id_node=as.numeric(names(c1$membership)),
																 #tab_hash_pid_int,
																 id_cluster=as.integer(c1$membership)) %>% 
		mutate(id_cluster_grpsing =
					 	ifelse(id_cluster %in% unlist(id_cluster_singleton),
					 				 9999,id_cluster))
	# c1$membership
	# df_cluster_info = list(df_membership_map = data.frame(id_node=as.numeric(names(c1$membership)),
	# 																											#tab_hash_pid_int,
	# 																											id_cluster=as.integer(c1$membership)),
	# 											 id_cluster_singleton=id_cluster_singleton)
	
	return(df_membership_map)
}

info_cluster = form_cluster_id(g0)
# group all singletons into 1 cluster

# attach cluster id to dataset to use as random effect
df_use = left_join(df_use,y=select(info_cluster,-id_cluster),
									 by=c('pid'='id_node'))


# Convert brms calls to Stan code #
## install.packages('brms')
library(brms)
library(rstan)

# formula_stan = X ~ Y + Z - id_cluster + (1|id_cluster)
formula_stan = X ~ Y + Z - id_cluster_grpsing + (1|id_cluster_grpsing)

prior_stan = c(set_prior("cauchy(0, 10)", class = "Intercept"),
							 set_prior("cauchy(0, 2.5)", class = "b"),
							 set_prior("student_t(3, 0, 10)", class = "sd"),
							 set_prior("normal(0, 10)", class = "sd",
							 					coef = "Intercept", group = "id_cluster_grpsing"),
							 set_prior("student_t(3, 0, 10)", class = "sd",
							 					group = "id_cluster_grpsing"))



head(df_use)
df_4_estimate = df_use
dat_stan = make_standata(formula=formula_stan,
												 data = df_4_estimate, 
												 # family = cumulative("logit"),
												 family = "normal",
												 prior = prior_stan)

code_stan <- make_stancode(formula = formula_stan, 
													 data = df_4_estimate, 
													 # family = cumulative("logit"),
													 family = "normal",
													 prior = prior_stan)
# do once
m <- stan_model(model_code = code_stan)

compute_quants = function(df_4_estimate){
	
	dat_stan = make_standata(formula=formula_stan,
													 data = df_4_estimate, 
													 # family = cumulative("logit"),
													 family = "normal",
													 prior = prior_stan)
	
	fit <- rstan::optimizing(m,data = dat_stan,hessian=TRUE,
													 draws = 10000) 
	
	stopifnot(fit$return_code==0)
	
	# out_se = sqrt(diag(solve(-(fit$hessian))))[names_fixef_hess]
	# names_fixef_par = grep(names(fit$par),pattern='b\\[')
	
	# https://discourse.mc-stan.org/t/optimizing-function-incorrect-estimation-of-the-standard-deviation-of-mle/2417
	hess2 = cov(fit$theta_tilde)
	
	ind_pick = 2
	
	est_samp = (t(fit$par))[[ind_pick]]
	se_samp = (sqrt(1/diag((hess2))))[[ind_pick]]
	
	# all params, so just postprocess after
	# out_fit = tibble::tibble(par=list(t(fit$par)),
	# 												 # hess=list(fit$hessian)
	# 												 hess=list(hess2)
	# 												 )
	
	out_fit = data.frame(est_samp,se_samp)
	
	return(out_fit)
	
}

test_fit = compute_quants(df_4_estimate)


