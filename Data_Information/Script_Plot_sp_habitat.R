
sp_name <- c("Clupea harengus","Gadus morhua","Melanogrammus aeglefinus",
             "Merlangius merlangus","Pleuronectes platessa", 
             "Pollachius virens","Scomber scombrus",
             "Sprattus sprattus", "Trisopterus esmarkii")

sp_common_name <- c("Herring","Cod","Haddock","Whiting","Plaice","Saithe",
                    "Mackerel","Sprat","Norwaypout")



#########################################################################
# plot only presence and absence

source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Function_Habitat_information_and_plot.R")

# for each sp 

jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\Data_Information\\sp_Habitat.jpeg", width=5, height=5, units = "in",res=300)
par(mfrow=c(3,3),mar=c(0,0,0,0),oma=c(4,4,1,1))
for (i in 1:6){
  if(i%%3==1){
    habitat_plot(sp_name[i],sp_common_name[i])
    axis(side = 2,family=c("newrom"))
    box(lwd=1)
  }
  if(i%%3!=1){
    habitat_plot(sp_name[i],sp_common_name[i])
    box(lwd=1)
  }
}
for(i in 7){
  habitat_plot(sp_name[i],sp_common_name[i])
  axis(side =1,family=c("newrom"))
  axis(side = 2,family=c("newrom"))
  box(lwd=1)
}
for(i in c(8,9)){
  habitat_plot(sp_name[i],sp_common_name[i])
  axis(side =1,family=c("newrom"))
  box(lwd=1)
}
mtext(text="Latitude", side=2, out=T,line=2,family=c("newrom"))
mtext(text="Longtitude", side=1, out=T,line=2,family=c("newrom"))

dev.off()

#########################################################################
# plot with abundance

source("D:\\Ruo_data\\2019_Paper_Publish\\Code\\Function_Habitat_plot_with_abund.R")

# for each sp 
jpeg("D:\\Ruo_data\\2019_Paper_Publish\\Figure\\Data_Information\\sp_Habitat_with_abun.jpeg", width=5, height=5, units = "in",res=300)
par(mfrow=c(3,3),mar=c(0,0,0,0),oma=c(4,4,1,1))

for (i in 1:6){
  if(i%%3==1){
    habitat_plot_with_abun(sp_name[i],sp_common_name[i])
    axis(side = 2,family=c("newrom"))
    box(lwd=1)
  }
  if(i%%3!=0){
    habitat_plot_with_abun(sp_name[i],sp_common_name[i])
    box(lwd=1)
  }
}
for(i in 7){
  habitat_plot_with_abun(sp_name[i],sp_common_name[i])
  axis(side =1,family=c("newrom"))
  axis(side = 2,family=c("newrom"))
  box(lwd=1)
}
for(i in c(8,9)){
  habitat_plot_with_abun(sp_name[i],sp_common_name[i])
  axis(side =1,family=c("newrom"))
  box(lwd=1)
}

mtext(text="Latitude", side=2, out=T,line=2,family=c("newrom"))
mtext(text="Longtitude", side=1, out=T,line=2,family=c("newrom"))

dev.off()
