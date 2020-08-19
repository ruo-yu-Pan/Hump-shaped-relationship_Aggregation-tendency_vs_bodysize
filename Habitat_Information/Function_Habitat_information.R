
#########################################################################
########   function for habitat map and non-habitat station   ###########
#########################################################################

##
# species  : the northsea data of species 
##

library(sp)
habitat_info_fuc <- function(species=fish_stage){
  
  if(length(which(species$Area==10))>0){
    species <- species[-which(species$Area==10),]
  }
  habitat_pre<-as.data.frame(xtabs(~Subarea,data = species))
  
  # the station has less than 3 data through whole station
  less.3.habitat <-subarea_symb[which(habitat_pre[,2]<=3),]
  
  # the subarea that has at least 3 data through whole station
  habitat.hul <- subarea_symb[-which(habitat_pre[,2]<=3),]
  
  # calculation for polygon
  hull <- chull(as.matrix(habitat.hul[,-1]))
  hull <- c(hull, hull[1])
  
  # pick out the station which is inside the polygon from less.3.habitat
  n.habitat <- less.3.habitat[-which(
    point.in.polygon(less.3.habitat[,2],less.3.habitat[,3],
                     habitat.hul[hull,2],habitat.hul[hull,3])!=0),]
  
  output <- list(habitat.hul,hull,n.habitat)
  return(output)
}
