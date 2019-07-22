#Environmental Data Stats

#Load required packages
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)

#Read in environmental data
data <- read.csv("./soils_data_w2_parse.csv")

#Save the summary data as a csv file in output
sum <- summary(data)
write.csv(sum, "./output/enviro_summary_data.csv")

#Remove all data we dont need (like cluster, name, elements below detection levels)
vars <- c("lon","lat","pH","EC","GWC","per_clay","per_sand","per_silt",
          "Al","Ca","Co","Cr","Cu","Fe","K","Mg","Mn","Na","Ni","P","S",
          "Ti","Zn","per_som","PerN","PerC","CNRatio","Ele")
sd.full <- data[,vars]

#Create a SPDF class object so that the data is clearly distinguished from the coordinates
coordinates(sd.full) <- ~ lon + lat

#Create variogram models for each variable

#                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc", "Ste", "Cir", "Lin", "Bes", "Pen", "Per", "Wav", "Hol", "Log", "Pow", "Spl")))



lzn.vgm.ph <- variogram((pH)~lon+lat, sd.full)
lzn.fit.ph <- fit.variogram(lzn.vgm.ph, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.EC <- variogram((EC)~lon+lat, sd.full)
lzn.fit.EC <- fit.variogram(lzn.vgm.EC, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.GWC <- variogram((GWC)~lon+lat, sd.full)
lzn.fit.GWC <- fit.variogram(lzn.vgm.GWC, model=
                               vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.per_clay <- variogram((per_clay)~lon+lat, sd.full)
lzn.fit.per_clay <- fit.variogram(lzn.vgm.per_clay, model=
                                    vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.per_sand <- variogram((per_sand)~lon+lat, sd.full)
lzn.fit.per_sand <- fit.variogram(lzn.vgm.per_sand, model=
                                    vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.per_silt <- variogram((per_silt)~lon+lat, sd.full)
lzn.fit.per_silt <- fit.variogram(lzn.vgm.per_silt, model=
                                    vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Al <- variogram((Al)~lon+lat, sd.full)
lzn.fit.Al <- fit.variogram(lzn.vgm.Al, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Ca <- variogram((Ca)~lon+lat, sd.full)
lzn.fit.Ca <- fit.variogram(lzn.vgm.Ca, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Co <- variogram((Co)~lon+lat, sd.full)
lzn.fit.Co <- fit.variogram(lzn.vgm.Co, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Cr <- variogram((Cr)~lon+lat, sd.full)
lzn.fit.Cr <- fit.variogram(lzn.vgm.Cr, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Cu <- variogram((Cu)~lon+lat, sd.full)
lzn.fit.Cu <- fit.variogram(lzn.vgm.Cu, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Fe <- variogram((Fe)~lon+lat, sd.full)
lzn.fit.Fe <- fit.variogram(lzn.vgm.Fe, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.K <- variogram((K)~lon+lat, sd.full)
lzn.fit.K <- fit.variogram(lzn.vgm.K, model=
                             vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Mg <- variogram((Mg)~lon+lat, sd.full)
lzn.fit.Mg <- fit.variogram(lzn.vgm.Mg, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Mn <- variogram((Mn)~lon+lat, sd.full)
lzn.fit.Mn <- fit.variogram(lzn.vgm.Mn, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Na <- variogram((Na)~lon+lat, sd.full)
lzn.fit.Na <- fit.variogram(lzn.vgm.Na, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Ni <- variogram((Ni)~lon+lat, sd.full)
lzn.fit.Ni <- fit.variogram(lzn.vgm.Ni, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.P <- variogram((P)~lon+lat, sd.full)
lzn.fit.P <- fit.variogram(lzn.vgm.P, model=
                             vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.S <- variogram((S)~lon+lat, sd.full)
lzn.fit.S <- fit.variogram(lzn.vgm.S, model=
                             vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Ti <- variogram((Ti)~lon+lat, sd.full)
lzn.fit.Ti <- fit.variogram(lzn.vgm.Ti, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Zn <- variogram((Zn)~lon+lat, sd.full)
lzn.fit.Zn <- fit.variogram(lzn.vgm.Zn, model=
                              vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.per_som <- variogram((per_som)~lon+lat, sd.full)
lzn.fit.per_som <- fit.variogram(lzn.vgm.per_som, model=
                                   vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.PerN <- variogram((PerN)~lon+lat, sd.full)
lzn.fit.PerN <- fit.variogram(lzn.vgm.PerN, model=
                                vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.PerC <- variogram((PerC)~lon+lat, sd.full)
lzn.fit.PerC <- fit.variogram(lzn.vgm.PerC, model=
                                vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.CNRatio <- variogram((CNRatio)~lon+lat, sd.full)
lzn.fit.CNRatio <- fit.variogram(lzn.vgm.CNRatio, model=
                                   vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))

lzn.vgm.Ele <- variogram((Ele)~lon+lat, sd.full)
lzn.fit.Ele <- fit.variogram(lzn.vgm.Ele, model=
                               vgm(c("Exp","Sph", "Gau", "Mat", "Nug", "Exc")))


#Plot variograms
plot(main="pH", lzn.vgm.ph, lzn.fit.ph)
plot(main="EC", lzn.vgm.EC, lzn.fit.EC)
plot(main="GWC", lzn.vgm.GWC, lzn.fit.GWC)
plot(main="% Clay", lzn.vgm.per_clay, lzn.fit.per_clay)
plot(main="% Silt", lzn.vgm.per_silt, lzn.fit.per_silt)
plot(main="% Sand", lzn.vgm.per_sand, lzn.fit.per_sand)
plot(main="Al", lzn.vgm.Al, lzn.fit.Al)
plot(main="Ca", lzn.vgm.Ca, lzn.fit.Ca)
plot(main="Co", lzn.vgm.Co, lzn.fit.Co)
plot(main="Cr", lzn.vgm.Cr, lzn.fit.Cr)
plot(main="Cu", lzn.vgm.Cu, lzn.fit.Cu)
plot(main="Fe", lzn.vgm.Fe, lzn.fit.Fe)
plot(main="K", lzn.vgm.K, lzn.fit.K)
plot(main="Mg", lzn.vgm.Mg, lzn.fit.Mg)
plot(main="Mn", lzn.vgm.Mn, lzn.fit.Mn)
plot(main="Na", lzn.vgm.Na, lzn.fit.Na)
plot(main="Ni", lzn.vgm.Ni, lzn.fit.Ni)
plot(main="P", lzn.vgm.P, lzn.fit.P)
plot(main="S", lzn.vgm.S, lzn.fit.S)
plot(main="Ti", lzn.vgm.Ti, lzn.fit.Ti)
plot(main="Zn", lzn.vgm.Zn, lzn.fit.Zn)
plot(main="per_som", lzn.vgm.per_som, lzn.fit.per_som)
plot(main="PerN", lzn.vgm.PerN, lzn.fit.PerN)
plot(main="PerC", lzn.vgm.PerC, lzn.fit.PerC)
plot(main="CNRatio", lzn.vgm.CNRatio, lzn.fit.CNRatio)
plot(main="Ele", lzn.vgm.Ele, lzn.fit.Ele)

#Create grids from the variograms to perfrom ordinary kriging - this will help us map the variables in QGIS
##Determine range of lat and lon
bbox(sd.full)

##Create a grid to esimate values over
x_range <- as.numeric(c(-94.23546, -94.23402))
y_range <- as.numeric(c(36.06563, 36.06665))
# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
sample.grid <- expand.grid(x = seq(from = x_range[1],
                                   to = x_range[2], 
                                   length.out=30),
                           y = seq(from = y_range[1],                                           to = y_range[2], 
                                   length.out=30))  # expand points to grid
class(sample.grid)

sample.grid$number=seq(from=1, to=900, length.out=900)
plot1 <- sd.full %>% as.data.frame %>%
  ggplot(aes(lat, lon)) + geom_point(size=.25) + coord_equal() + 
  ggtitle("Points with measurements")
# this is clearly gridded over the region of interest
plot2 <- sample.grid %>% as.data.frame %>%
  ggplot(aes(y, x)) + geom_point(size=.25) + coord_equal() + 
  ggtitle("Points at which to estimate")
library(gridExtra)
grid.arrange(plot1, plot2, ncol=2)

coordinates(sample.grid) <- ~ x + y

lzn.kriged.ph <- krige(ph~1, sd.full, sample.grid, model=lzn.fit.ph)
