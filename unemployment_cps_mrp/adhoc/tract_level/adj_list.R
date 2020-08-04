require(colorout)
require(sqldf)
require(doMC)
require(foreach)
require(maps)
require(mapproj)
require(maptools)
require(sp)
require(rgdal)
require(rgeos)
require(arm)

remove(list=objects())
options(digits=2, scipen=9, width=110, java.parameters = "-Xrs")
setwd("~/unemployment/unemployment_cps_mrp/adhoc/tract_level/")

################################################################################################################

load("shp.RData")

registerDoMC(30)
print(system.time(raw <- foreach(state = names(shp$tract), .errorhandling="pass") %dopar% {
  message("adjacency lists: ", state)
  shp_tmp <- shp$tract[[state]]
  adj_mat <- gTouches(shp_tmp, byid=TRUE)
  colnames(adj_mat) <- rownames(adj_mat) <- NULL
  adj_list <- sapply(1:nrow(adj_mat), function(i) shp_tmp@data$fips[which(adj_mat[i,])])
  names(adj_list) <- shp_tmp@data$fips
  return(adj_list)
}))

out <- list()
for (i1 in 1:length(raw)) {
  message(i1, " of ", length(raw))
  for (i2 in names(raw[[i1]]))
    out[[i2]] <- raw[[i1]][[i2]]
}

saveRDS(out, file="adj_list.rds")
