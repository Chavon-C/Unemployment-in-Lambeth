pacman::p_load(sf, tmap, spdep, spgwr, grid, gridExtra)

employment_input <-read.csv("datasets/ts066.csv")
tenure_input<-read.csv("datasets/ts054.csv")

employment <- data.frame(OA=employment_input$OA, Unemployed=(employment_input$ts0660013/employment_input$ts0660001)*100)
social_housing <-data.frame(OA=tenure_input$OA, Social=(tenure_input$ts0540007/tenure_input$ts0540001)*100)

merged_data <- merge(employment, social_housing, by="OA")

write.csv(merged_data, "datasets/census_data.csv", row.names=F)

census_data <- read.csv("datasets/census_data.csv")

Output.Areas <- st_read("OA_2021_EW_BGC_V2.shp")

OA.Lambeth<- Output.Areas[grepl('Lambeth', Output.Areas$LSOA21NM),]

OA.Census<- merge(OA.Lambeth, census_data, by.x="OA21CD", by.y="OA")

#dont need to run
st_write(OA.Census, dsn = "Census_OA_Shapefile.geojson", driver="GeoJSON")

OA.Census<-st_read(dsn = "Census_OA_Shapefile.geojson")

oa_centroid <- st_centroid(OA.Census)

tm_shape(OA.Census) + tm_fill("Unemployed", palette = "BuPu", style = "jenks", title = "% unemployed") + tm_borders(alpha=.4)
tm_shape(OA.Census) + tm_fill("Social", palette = "BuPu", style = "jenks", title = "% social housing") + tm_borders(alpha=.4)

nb <- dnearneigh(st_centroid(OA.Census),0,1400)

listw <- nb2listw(nb, style="W")
listw

Lag <- lag.listw(nb2listw(nb, style = "W"), OA.Census$Unemployed)

Unemployed<- OA.Census$Unemployed

plot(Unemployed, Lag)

abline(h = mean(Unemployed), lty = 2)
abline(v = mean(Lag), lty = 2)

Unemployed.recenter<- Unemployed-mean(Unemployed)
Lag.recenter<- Lag-mean(Lag) 

plot(Unemployed.recenter, Lag.recenter)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

xy.lm <- lm(Lag.recenter ~ Unemployed.recenter)
abline(xy.lm, lty=3)

moran.test(OA.Census$Unemployed, listw)
#moran's i= 0.035

local <- localmoran(x = OA.Census$Unemployed, listw = nb2listw(nb, style = "W"))

moran.map <- OA.Census

moran.map<- cbind(OA.Census, local)

tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic") 

#lisa map
quadrant <- vector(mode="numeric",length=nrow(local))

signif <- 0.1

quadrant[Unemployed.recenter <0 & Lag.recenter<0] <- 1 #LOW-LOW BOTTOM LEFT
quadrant[Unemployed.recenter <0 & Lag.recenter>0] <- 2 #LOW-HIGH TOP LEFT
quadrant[Unemployed.recenter >0 & Lag.recenter<0] <- 3 #HIGH-LOW BOTTOM RIGHT 
quadrant[Unemployed.recenter >0 & Lag.recenter>0] <- 4 #HIGH-HIGH TOP RIGHT

quadrant[local[,5]>signif] <- 0  

brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")

plot(OA.Census[1],border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)], main="Lambeth Spatial Clustering: Unemployment")
legend("bottomleft",legend=c("insignificant","low-low","low-high","high-low","high-high"),fill=colors,bty="n")

#GWR
model <- lm(OA.Census$Unemployed ~ OA.Census$Social)
summary(model)

plot(model, which = 3)

resids<-residuals(model)

map.resids<-OA.Census

map.resids<- cbind(OA.Census, resids) 

# maps the residuals using the quickmap function from tmap
qtm(map.resids, fill = "resids")

OA.Census.SP<- as(OA.Census, "Spatial")

GWRbandwidth <- gwr.sel(OA.Census.SP$Unemployed ~ OA.Census.SP$Social, data=OA.Census.SP,adapt=T)

# run the gwr model
gwr.model = gwr(OA.Census.SP$Unemployed ~ OA.Census.SP$Social, data = OA.Census.SP, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#print the results of the model
gwr.model

results <-as.data.frame(gwr.model$SDF)

names(results)

gwr.map<- OA.Census

gwr.map <- cbind(OA.Census, as.matrix(results))

qtm(gwr.map, fill = "localR2")+ tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)

map1 <- tm_shape(gwr.map) + tm_fill("Social", n = 5, style = "jenks")  + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)
map2 <- tm_shape(gwr.map) + tm_fill("OA.Census.SP.Social", n = 5, style = "jenks", title = "Social Coefficient") + tm_layout(frame = FALSE, legend.text.size = 0.5, legend.title.size = 0.6)

grid.newpage()

pushViewport(viewport(layout=grid.layout(1,2)))

print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))



