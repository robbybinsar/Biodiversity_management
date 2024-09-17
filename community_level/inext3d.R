# https://github.com/KaiHsiangHu/iNEXT.3D

library(iNEXT.3D)

data(Brazil_rainforest_abun_data)
data(Brazil_rainforest_phylo_tree)
data(Brazil_rainforest_distance_matrix)


output_TD_abun <- iNEXT3D(Brazil_rainforest_abun_data, diversity = 'TD', q = c(0,1,2), 
                          datatype = "abundance")
ggiNEXT3D(output_TD_abun, type = 1, facet.var = "Order.q")
