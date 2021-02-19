windowsFonts(newrom = "Times New Roman")

wd = "D:/Ruo_data/2019_Paper_Publish/Publish"
setwd(wd)

northsea <- read.csv("./SizeAggregTend_data/compiled/CPUE_per_length_per_subarea_clean.csv", header = T, sep = ",")
tot_sta <-as.data.frame(xtabs(~Subarea,data = northsea))

sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")

sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice","Saithe",
                    "Mackerel","Sprat","Norwaypout")


# make the subarea symbol become true latitude and longtitude
lat <-seq(49.75,61.75,0.5)
long <- seq(-3.5,12.5,1)

subarea_symb <- data.frame(subarea=tot_sta[,1],
                           lat = lat[as.integer(factor(substr(tot_sta[,1],1,2)))],
                           long = long[as.integer(factor(substr(tot_sta[,1],3,4)))])


#########################################################################
# plot only presence and absence

source("./SizeAggregTend_code/Habitat_Information/Function_Habitat_plot.R")

# for each sp 

jpeg("./SizeAggregTend_data/output/fig/Appendix/Fig_sp_Habitat.jpeg", width=1800, height=1800, units = "px",res=300)
par(mfrow=c(3,3),mar=c(0,0,0,0),oma=c(4,4,1,1))
for (i in 1:6){
  if(i%%3==1){
    habitat_plot_func(sp_name[i],sp_common_name[i])
    axis(side = 2)
    box(lwd=1)
  }
  if(i%%3!=1){
    habitat_plot_func(sp_name[i],sp_common_name[i])
    box(lwd=1)
  }
}
for(i in 7){
  habitat_plot_func(sp_name[i],sp_common_name[i])
  axis(side =1)
  axis(side = 2)
  box(lwd=1)
}
for(i in c(8,9)){
  habitat_plot_func(sp_name[i],sp_common_name[i])
  axis(side =1)
  box(lwd=1)
}
mtext(text="Latitude", side=2, out=T,line=2)
mtext(text="Longtitude", side=1, out=T,line=2)

dev.off()

