library(RDStreeboot)
data(faux.network)
set.seed(123);samp <- sample.RDS(faux.network$traits, faux.network$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)

str(samp,1)
## estimate 80% and 95% confidence intervals
samp=samp;B1=5

# confirm edgelist ignores islands
# confirm seed edge do not need NA parents
# confirm nodes makes up for islands
str(samp$edges,1)  
str(samp$nodes,1)  

set.seed(4321);id_boot_1 = RDStreeboot:::.TBS(samp, B=B1)


library(dplyr)
b=1
id_boot_1[[b]]
id_boot_1[[2]]

# mask out original data edges not present in first level boot
# nodes_sampled, look up direct parent from previous generation

edges_elig_samp_b = samp$edges %>% 
	dplyr::filter((node1 %in% id_boot_1[[b]]) & (node2 %in% id_boot_1[[b]]))

# mask out unnecessary?
edges_elig_samp_b = samp$edges


edges_elig_samp_b %>% head()

id_boot_1[[b]]

# use fact that
# node can only have up to 1 parent
# node can have many child

# for sampled nodes, attach eligible 'original data' edges
edges_sampled_b = left_join(x=data.frame(node2=as.numeric(id_boot_1[[b]])),
													y=edges_elig_samp_b)

head(edges_sampled_b)
# for seed nodes, above attaches NA as parent
# for island nodes, above attaches NA as edge

# cosmeticaly get rid of parent redundant na for seed
# cosmetically get rid of island node with NA edge

# parent and island node info preserved in seperate $node attribute
# confirm this is behavior of .TBS(samp=list(edges,nodes))

edges_sampled_b = select(edges_sampled_b,node1,node2) %>% 
	na.omit() 

edges_sampled_b %>% head()

# potential island nodes
setdiff(as.numeric(id_boot_1[[b]]),unique(unlist(edges_sampled_b)))

# list with nodes and edges info required by .TBS()
id_boot_1_treeinfo = list(nodes=as.numeric(id_boot_1[[b]]),
													edges=edges_sampled_b)
str(id_boot_1_treeinfo)

# identical(id_boot_1_treeinfo,id_boot_2_treeinfo)

# pre filtering using intersection same thing

# 2nd level tree bootstrap from elements resulting from 1st level tree bootstrap
B2=5
id_boot_2 = RDStreeboot:::.TBS(id_boot_1_treeinfo, B=B2)
lapply(id_boot_2,sort)

# nodes that are not in eligible edgelist most likely island node

# maybe nested tibble best way to save 2 level bootstrap
?tibble()
# ids_b1,list(ids_b1_bb_1,ids_b1_bb_2,...,ids_b1_bb_B2)
# ids_b2,list(ids_b2_bb_1,ids_b2_bb_2,...,ids_b2_bb_B2)
# ...
# ids_B1,list(ids_bB1_bb_1,ids_bB1_bb_2,...,ids_bB1_bb_B2)
			 
# nested tibbles
size1=tibble(ids_tbs1=list(id_boot_1[[b]]),ids_tbs2=id_boot_2)
size2=tibble(ids_tbs1=list(id_boot_1[[b]]),ids_tbs2=list(id_boot_2))
object.size(size1)
object.size(size2)

# can then row wise purr:::map or for loop

