# install.packages('igraph')
library(igraph)
# ?graph_from_literal

# specify easy toy example
# A directed graph

# RDS graph of 'original data' (fake) -----------------------------------

g0 <- graph_from_literal( 1 -+ 2 -+ 3 -+ 4,
													1 -+ 5 -+6,
													6-+7,6-+8,6-+9,
													7-+10,7-+11,8-+12,
													13,14
)
plot(g0,layout=layout_as_tree)

class(g0)

as_edgelist(g0)
list_v_and_e = igraph::as_data_frame(g0,what='both')
str(list_v_and_e,2)

# restructure into format RDStreeboot:::.TBS() needs
samp0 = list()
samp0$nodes = as.numeric(unlist(list_v_and_e$vertices,use.names = FALSE))
samp0$edges = data.frame(node1=as.numeric(list_v_and_e$edges$from),
												 node2=as.numeric(list_v_and_e$edges$to))


str(samp0)


# level 1 tbs -------------------------------------------------------------

set.seed(1234); nodes_l1=RDStreeboot:::.TBS(samp=samp0,B=2)
b=1
nodes_l1[[b]]
str(nodes_l1,1)


# pretend 'ind_lev_1_fake' is one bootstrap from level 1 tbs
# ind_lev_1_fake = nodes_l1[[1000]]

ind_lev_1_fake =c(1,2,3,4,
									5,6,7,10,11,
									7,10,11,
									1,11,11,
									13,13)

ind_lev_1_fake
unique(ind_lev_1_fake)

# graph_from_literal() needs unique IDS too

g1 <- graph_from_literal( 1 -+ 2 -+ 3 -+ 4,
													1 -+ 5 -+6,
													6-+7,#6-+8,6-+9,
													7-+10,7-+11,
													6-+'7a',
													'7a'-+'10a','7a'-+'11a',
													6-+'7b',
													'7b'-+'11b','7b'-+'11c',
													13,'13a')
plot(g1,layout=layout_as_tree)

# RDStreeboot:::.TBS() needs to relabel unique IDS

# this will not work
RDStreeboot:::.TBS(remap_tree(ind_lev_1_fake))
RDStreeboot:::.TBS(remap_tree(ind_lev_1_fake))


# TODO FILL IN ------------------------------------------------------------

# need to do something like
ind_lev_1_fake_unique = relabel_unique(ind_lev_1_fake)
# ...
remap_tree(ind_lev_1_fake_unique)
# ...

RDStreeboot:::.TBS(remap_tree(ind_lev_1_fake_unique))


par(mfrow=c(1,2))
plot(g0,layout=layout_as_tree)
plot(g1,layout=layout_as_tree)



