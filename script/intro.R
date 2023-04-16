library(here)
library(tidyverse)
library(flextable)
library(networkD3)

# First Steps ----------------------------------------------------------
# data=read.csv(here("data/E-Scooter_Trips_-_2020.csv"))
# 
# data |> head()
# 
# glimpse(data)
# 
# summary(data)
# 
# lubridate::as.duration(as.numeric(gsub(",","",data$Trip.Duration)))
# 
# data$Trip.Duration = gsub(",","",data$Trip.Duration) |> as.numeric() |>as.duration()  |> as.numeric()
# 
# data$Trip.Distance = gsub(",","",data$Trip.Distance) |> as.numeric() 

#saveRDS(data,file=here('objects/dataset.R'))
 
# Dataviz --------------------------------------------------------
data = readRDS(here('objects/dataset.R'))

 #plot(data$Trip.Duration, data$Trip.Distance, xlim = c(0,12000)) 
 #lm(Trip.Duration ~Trip.Distance, data=data) |> plot()
 
summary(data)
 table(data$Vendor)

 total_for_vendor= data |>
   group_by(Vendor) |>  
   summarise(total=n()) |> 
   ggplot(aes(x=Vendor)) +
   geom_col(aes(y=total,fill=Vendor))
 
# ggsave(filename = "total_vendor_bar.png", plot=total_for_vendor,path=here('graphics'))
   
 

 
mean_duration= data |>
   ggplot(aes(x=Vendor, y=Trip.Duration, fill=Vendor)) + 
   geom_boxplot(outlier.shape = NA) +
   labs(title = "Average trip duration for scooter each vendor",
        subtitle ="Without Outliers"
         ) +
   coord_cartesian(ylim = quantile(data$Trip.Duration, c(0.1, 0.92)))

mean_duration
 
#ggsave(filename = "mean_duration_for_vendor.png", plot=mean_duration,path=here('graphics'))
 
common_travel_trajectory = data |> na.omit() |> 
   group_by(Start.Community.Area.Name,End.Community.Area.Name) |>  
   summarise(total=n(),meanDuration=mean(Trip.Duration),meanDistance=mean(Trip.Distance)) |>
   arrange(desc(total)) |>
    head(10) |> 
   flextable()

common_travel_trajectory   
 
#save_as_image(commum_travel_trajetory, path=here("graphics/common_travel_trajectory.png"))

# netwoek viz  -----------------------------------


data_trajectory_10 = data |> na.omit() |> 
   group_by(Start.Community.Area.Name,End.Community.Area.Name) |> 
   summarise(total=n(),meanDuration=mean(Trip.Duration),meanDistance=mean(Trip.Distance)) |>
   arrange(desc(total)) |>
   head(10) 

 nodes <- data.frame(
   name=c(as.character(data_trajectory_10$Start.Community.Area.Name), 
          tolower(as.character(data_trajectory_10$End.Community.Area.Name))) |> 
         unique()
)

nodes 

data_trajectory_10$IDsource <- match(data_trajectory_10$Start.Community.Area.Name, nodes$name)-1 
data_trajectory_10$IDtarget <- match(tolower(data_trajectory_10$End.Community.Area.Name), nodes$name) -1 

data_trajectory_10

p <- sankeyNetwork(Links = data_trajectory_10, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "total", NodeID = "name", 
                   sinksRight=T, nodeWidth=10, fontSize=13, nodePadding=20)

p

#savePlot(here("graphics/common_travel_trajectory_sankey_Network.png"))

# INterative map ----------------------------
library(leaflet)
library(leaflet.minicharts)

porcentage = function(a,b){
   return (round((a/b),2))
}

porcentage(1,2)

data |> pivot_longer(cols = 'Vendor')
data_mod= data |> group_by(Start.Community.Area.Name,
                 Start.Centroid.Latitude,
                 Start.Centroid.Longitude,
                 Vendor) |>
      summarise(n=n()) |>
      na.omit() |>
      pivot_wider(names_from='Vendor',values_from=n) |> 
      mutate(total=bird+lime+spin) |> 
      mutate(p.bird=porcentage(bird,total),p.lime=porcentage(lime,total),p.spin=porcentage(spin,total))
   

basemap = leaflet() %>% 
   addTiles() %>% 
   addProviderTiles(providers$OpenStreetMap)
basemap |> addCircleMarkers(lng = data_mod$Start.Centroid.Longitude,lat = data_mod$Start.Centroid.Latitude )
   
   

basemap |> 
addMinicharts(
   lat=data_mod$Start.Centroid.Latitude, lng=data_mod$Start.Centroid.Longitude,
   type = "pie",
   chartdata = data_mod[,c('p.bird','p.lime','p.spin')],
   width =20,
   layerId =data_mod$Start.Community.Area.Name
   
   )

display.brewer.all()
   