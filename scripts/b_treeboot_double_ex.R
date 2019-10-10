# install.packages('RDStreeboot')


# example from manual -----------------------------------------------------


require(RDStreeboot)

data(faux.network)

## draw RDS from network
samp <- sample.RDS(faux.network$traits, faux.network$adj.mat, 100, 2, 3, c(0,1/3,1/3,1/3), TRUE)

str(samp,1)
## estimate 80% and 95% confidence intervals
samp=samp;B=3
test = treeboot.RDS(samp, c(0.025, 0.10, 0.90, 0.975), 2000)

# main sampling function is .TBS()
?treeboot.RDS
RDStreeboot:::.TBS
View(RDStreeboot:::.TBS)


# level 1 bootstrap -------------------------------------------------------
B1=3
id_boot_0 = RDStreeboot:::.TBS(samp, B=B1)

str(id_boot_0,1)

id_boot_0[[1]]
id_boot_0[[3]]


# level 2 bootstrap -------------------------------------------------------

# ez idea, for 2nd level bootstrap do classic bootstrap
# classic = sample ids with replacement, ignore tree structure

b=1

B2 = 2
resamp_2 = vector(mode='list',length=B2)
for(bb in 1:B2){
  # id_boot_1=samp_tb[[b]] is the b-th set of ids from the 1st level bootstrap
  id_boot_1 = id_boot_0[[b]]
  id_boot_2 = sample(id_boot_1,
                     size=length(id_boot_1),
                     replace=TRUE)  
  resamp_2[[bb]] <- id_boot_2
}

str(resamp_2,1)


# better idea, for 2nd level bootstrap
# do another RDStreeboot:::.TBS()
# need to puzzle/figure out how to recreate tree / adjacency
# for each b, id_boot_1_tree = remap_tree(id_boot_0[[b]],...)
# key step is to match descendent ids to ancestor ids

# remap_tree = function(id_boot_desc,table_edges_ances){}

# id_boot_1_tree = remap_tree(id_boot_0[[b]],...)
id_boot_2_tree = RDStreeboot:::.TBS(id_boot_1_tree, B=B2)
