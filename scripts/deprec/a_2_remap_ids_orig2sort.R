# need to verify
# use node ids to filter data and not just as row
# node and edge info arguments into .tbs
# should node be unique nodes? exhaustive? 

id_boot_desc_b = c(13,13,1,2,3,4)
edges_sampled_b = data.frame(node1=c(1,2,3),
														 node2=c(2,3,4))
test = list(nodes=as.numeric(id_boot_desc_b),
						edges=edges_sampled_b)


library(RDStreeboot)
RDStreeboot:::.TBS(test,1)

# .TBS internally orderd node ids, output is also ordered

# error
id_boot_desc_b = c(13,13)
test = list(nodes=as.numeric(id_boot_desc_b),
						edges=edges_sampled_b)
RDStreeboot:::.TBS(test,1)

id_boot_desc_b = c(13,14)
test = list(nodes=as.numeric(id_boot_desc_b),
						edges=edges_sampled_b)
RDStreeboot:::.TBS(test,1)

# takeaway: $nodes need to be unique and include nodes found in $edges 


# output 6 but not present input
id_boot_desc_b = c(5,5,1,2,3,4)
test = list(nodes=as.numeric(id_boot_desc_b),
						edges=edges_sampled_b)
RDStreeboot:::.TBS(test,1)

# output 5 but not present input
id_boot_desc_b = c(6,1,2,3,4)
test = list(nodes=as.numeric(id_boot_desc_b),
						edges=edges_sampled_b)
RDStreeboot:::.TBS(test,10)

g0 <- graph_from_literal( 1 -+ 2 -+ 3 -+ 4,
													20)

# error
g0 <- graph_from_literal(20,
												 2 -+ 3 -+ 4 -+ 6,
												 5 -+ 11)

# # first 3 lines of .TBS code assumes a nxn matrix 1:n=length(samp$nodes)
# n <- length(samp$nodes)
# samp.adj.mat <- matrix(F, n, n)
# samp.adj.mat[cbind(samp$edges$node1, samp$edges$node2)] <- T

# takeaway: need to pre sort nodes before using .TBS()
df_key_id_orig_w_sort = data.frame(id_orig=sort(samp0$nodes),
																	 id_ord=seq_along(samp0$nodes))


tree_info_id_sort = list()
# supply samp

tree_info_id_sort$nodes = plyr::mapvalues(samp$nodes,
																								df_key_id_orig_w_sort$id_orig,
																								df_key_id_orig_w_sort$id_ord)

# tree_info_id_sort$edges = data.frame(node1=plyr::mapvalues(samp$edges$node1, 
# 																													 df_key_id_orig_w_sort$id_orig,
# 																													 df_key_id_orig_w_sort$id_ord),
# 																		 node2=plyr::mapvalues(samp$edges$node2, 
# 																		 											df_key_id_orig_w_sort$id_orig,
# 																		 											df_key_id_orig_w_sort$id_ord))

tree_info_id_sort$edges = data.frame(sapply(samp$edges,FUN=plyr::mapvalues,
																						from=df_key_id_orig_w_sort$id_orig,
																						to=df_key_id_orig_w_sort$id_ord))

tree_info_id_sort
set.seed(1234); out_tbs = RDStreeboot:::.TBS(tree_info_sort_id,10)

# id_resamp_sort = out_tbs[[3]]
id_resamp_sort = out_tbs

id_resamp_orig = lapply(id_resamp_sort,FUN=plyr::mapvalues,
												to=df_key_id_orig_w_sort$id_orig,  # reverse to/from
												from=df_key_id_orig_w_sort$id_ord)

plot(g0,layout=layout_as_tree)
list_v_and_e = igraph::as_data_frame(g0,what='both')
samp0 = list()
samp0$nodes = as.numeric(unlist(list_v_and_e$vertices,use.names = FALSE))
samp0$edges = data.frame(node1=as.numeric(list_v_and_e$edges$from),
												 node2=as.numeric(list_v_and_e$edges$to))
str(samp0)
set.seed(1234); out_tbs = RDStreeboot:::.TBS(samp0,10)
# takeaway: ids supplied to .TBS NEED to be gapless ordered integers 
# since .TBS internally will relabel to gapless ordered ids
# so need to match output of .tbs back to original id labeles

# need to sort / unsort after each time .tbs used

# output of tbs are ordered ids
# out_tbs[[3]]

# samp0 is the tree info fed into .tbs

# # create orig id / ordered id  key table
# 
# df_key_id_orig_w_sort = data.frame(id_orig=sort(samp0$nodes),
# 																	id_ord=seq_along(samp0$nodes))


# use the tbs output ordered ids as row id  of key table, return orig id

# id_orig_fit_for_use = df_key_id_orig_w_sort[out_tbs[[3]],'id_orig']
