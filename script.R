#Load Packages
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(sf)
library(rgdal)
library(geojsonio)
library(plotly)
library(tidymodels)
library(stringr)
library(janitor)
library(spdep)
library(dplyr)

##Load data
Londonborough <- st_read(here::here("data",
                                    "statistical-gis-boundaries-london", 
                                    "ESRI", 
                                    "London_Borough_Excluding_MHW.shp"))%>%
  st_transform(., 27700)


boroughprice <- read_csv(here::here("data",
                                    "data.csv"),
                         locale = locale(encoding = "latin1"),
                         na = "n/a")
affordablehouse <- read_csv(here::here("data",
                                       "affordable-housing-borough.csv"),
                            locale = locale(encoding = "latin1"),
                            na = "n/a")

Datatypelist1 <- boroughprice %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist2 <- affordablehouse %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")


bp_2017 <- boroughprice %>%
  dplyr::filter(str_detect(date, "^2017"))

ah_2017 <- affordablehouse %>%
  dplyr::filter(str_detect(Year, "^2017"))

LondonboroughMerged <- Londonborough %>% 
  left_join(bp_2017, 
            by = c("GSS_CODE" = "code"))

LondonboroughMergedf <- LondonboroughMerged %>%
  mutate(p_density = as.numeric(population_size/HECTARES))%>%
  mutate(crime_r =as.numeric(no_of_crimes/population_size))

# overall trend
price <- read_csv(here::here("data",
                             "housing_in_london.csv"),
                  locale = locale(encoding = "latin1"),
                  na = "n/a")
plot_ly(price,x = ~date, y = ~average_price)%>%
  add_lines(color = ~area, alpha = 0.9)

# spatial autocorrelation
coordsW <- LondonboroughMerged%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW,axes=TRUE)

#create a neighbours list
LWard_nb <- LondonboroughMerged %>%
  poly2nb(., queen=T)
#or nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)
LWard_knn <- knn_wards %>%
  knn2nb()

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
plot(LWard_knn, st_geometry(coordsW), col="blue")
#add a map underneath
plot(LondonboroughMerged$geometry, add=T)

Lward.lw <- LWard_knn %>%
  nb2listw(., style="C")
head(Lward.lw$neighbours)

# house price
I_LWard_Global_price <- LondonboroughMergedf %>%
  pull(average_price) %>%
  as.vector()%>%
  moran.test(., Lward.lw)
I_LWard_Global_price

C_LWard_Global_price <- 
  LondonboroughMergedf %>%
  pull(average_price) %>%
  as.vector()%>%
  geary.test(., Lward.lw)
C_LWard_Global_price

G_LWard_Global_price <- 
  LondonboroughMergedf %>%
  pull(average_price) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)
G_LWard_Global_price

# dwelling density
I_LWard_Global_density <- LondonboroughMergedf %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)
I_LWard_Global_density

C_LWard_Global_density <- 
  LondonboroughMergedf %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)
C_LWard_Global_density

G_LWard_Global_density <- 
  LondonboroughMergedf %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)
G_LWard_Global_density

# pop density
I_LWard_Global_pd <- LondonboroughMergedf %>%
  pull(p_density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)
I_LWard_Global_pd

C_LWard_Global_pd <- 
  LondonboroughMergedf %>%
  pull(p_density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)
C_LWard_Global_pd

G_LWard_Global_pd <- 
  LondonboroughMergedf %>%
  pull(p_density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)
G_LWard_Global_pd

# mean salary
I_LWard_Global_ms <- LondonboroughMergedf %>%
  pull(mean_salary) %>%
  as.vector()%>%
  moran.test(., Lward.lw)
I_LWard_Global_ms

C_LWard_Global_ms <- 
  LondonboroughMergedf %>%
  pull(mean_salary) %>%
  as.vector()%>%
  geary.test(., Lward.lw)
C_LWard_Global_ms

G_LWard_Global_ms <- 
  LondonboroughMergedf %>%
  pull(mean_salary) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)
G_LWard_Global_ms

# crime rate
I_LWard_Global_cr <- LondonboroughMergedf %>%
  pull(crime_r) %>%
  as.vector()%>%
  moran.test(., Lward.lw)
I_LWard_Global_cr

C_LWard_Global_cr <- 
  LondonboroughMergedf %>%
  pull(crime_r) %>%
  as.vector()%>%
  geary.test(., Lward.lw)
C_LWard_Global_cr

G_LWard_Global_cr <- 
  LondonboroughMergedf %>%
  pull(crime_r) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)
G_LWard_Global_cr


#use the localmoran function to generate I for each borough in the city
I_LWard_Local_price <- LondonboroughMergedf %>%
  pull(average_price) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()
Gi_LWard_Local_price <- LondonboroughMergedf %>%
  pull(average_price) %>%
  as.vector()%>%
  localG(., Lward.lw)

I_LWard_Local_dden <- LondonboroughMergedf %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()
Gi_LWard_Local_dden <- LondonboroughMergedf %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

I_LWard_Local_pden <- LondonboroughMergedf %>%
  pull(p_density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()
Gi_LWard_Local_pden <- LondonboroughMergedf %>%
  pull(p_density) %>%
  as.vector()%>%
  localG(., Lward.lw)

I_LWard_Local_ms <- LondonboroughMergedf %>%
  pull(mean_salary) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()
Gi_LWard_Local_ms <- LondonboroughMergedf %>%
  pull(mean_salary) %>%
  as.vector()%>%
  localG(., Lward.lw)

I_LWard_Local_cr <- LondonboroughMergedf %>%
  pull(crime_r) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()
Gi_LWard_Local_cr <- LondonboroughMergedf %>%
  pull(crime_r) %>%
  as.vector()%>%
  localG(., Lward.lw)

LondonboroughMergedf <- LondonboroughMergedf %>%
  mutate(price_Iz =as.numeric(I_LWard_Local_price$Z.Ii))%>%
  mutate(price_G =as.numeric(Gi_LWard_Local_price))%>%
  mutate(dden_Iz =as.numeric(I_LWard_Local_dden$Z.Ii))%>%
  mutate(dden_G =as.numeric(Gi_LWard_Local_dden))%>%
  mutate(pden_Iz =as.numeric(I_LWard_Local_pden$Z.Ii))%>%
  mutate(pden_G =as.numeric(Gi_LWard_Local_pden))%>%
  mutate(ms_Iz =as.numeric(I_LWard_Local_ms$Z.Ii))%>%
  mutate(ms_G =as.numeric(Gi_LWard_Local_ms))%>%
  mutate(cr_Iz =as.numeric(I_LWard_Local_cr$Z.Ii))%>%
  mutate(cr_G =as.numeric(Gi_LWard_Local_cr))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))


tm_shape(LondonboroughMergedf) +
  tm_polygons("price_G",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Gi*, House price in London")

tm_shape(LondonboroughMergedf) +
  tm_polygons("dden_G",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Gi*, Dwelling density in London")

tm_shape(LondonboroughMergedf) +
  tm_polygons("pden_G",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Gi*, Population density in London")

tm_shape(LondonboroughMergedf) +
  tm_polygons("ms_G",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Gi*, Mean salary in London")

tm_shape(LondonboroughMergedf) +
  tm_polygons("cr_G",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Gi*, Crime rate in London")




library(corrplot)

data_cor <- LondonboroughMergedf %>%
  st_drop_geometry()%>%
  dplyr::select(c(average_price,mean_salary,density,p_density,crime_r))

library(caret)
# (1)最大值-最小值规范化
trans = preProcess(data_cor, method = c("range"))
transformed = predict(trans, data_cor)


# Cluster
fit <- transformed %>%
  kmeans(., 4, nstart=50)
centroid <- tidy(fit)%>%
  #print the results of the cluster groupings
  print()%>%
  dplyr::select(average_price, mean_salary, density, p_density)

p <- ggplot(transformed,aes(average_price, mean_salary, density, p_density))+
  geom_point(aes(colour=factor(fit$cluster)))+
  geom_point(data=centroid,aes(average_price, mean_salary, density, p_density), size=7, shape=18)+ theme(legend.position="none")

LondonB <- fit %>% 
  # 
  augment(., LondonboroughMergedf)%>%
  dplyr::select(GSS_CODE, .cluster)%>%
  #make sure the .cluster column is numeric
  mutate(across(.cluster, as.numeric))%>%
  # join the .cluster to our sf layer
  left_join(LondonboroughMergedf, 
            .,
            by = c("GSS_CODE" = "GSS_CODE"))


#now map our geodeomographic classification
map <- ggplot(LondonB) + 
  geom_sf(mapping = aes(fill=.cluster))+
  scale_fill_continuous(breaks=c(1,2,3,4))
map


# affordable housing in london 
ah_2017<-ah_2017%>%
  clean_names()
Londonboroughah <- Londonborough %>% 
  left_join(ah_2017, 
            by = c("GSS_CODE" = "code"))
breaks1<-c(0,100,200,300,400,500,800)
tm_shape(Londonboroughah) +
  tm_polygons("affordable_housing_supply",
              palette="Greens",
              breaks=breaks1,
              midpoint=NA,
              popup.vars=c("area", "affordable_housing_supply"),
              title="Affordable Housing Supply in 2017")

