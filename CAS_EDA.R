## Define path and set working directory ####

path <- "C:/Users/AKachhara/Desktop/MoT_Assignment/" ## path should be the main folder where Input, Output and Scripts Repository folder exist
setwd(path)

## if libraries don't exist, install and then load the libraries.
source("./ScriptsRepository/loading_libraries.R")

## Projections ####
latlon_CRS <- "+proj=longlat +datum=WGS84"
NZTM_CRS <- "+init=epsg:2193"

## Data Imports####
## WARNING: Reading spatial data can take a few minutes to run. Be patient :)

## Run below to import from a local directory ##
cas.sp <- st_read("./Input/CASData/CASData.shp")

# OR to use GeoJSON key from https://opendata-nzta.opendata.arcgis.com/datasets/NZTA::crash-analysis-system-cas-data-1/about ###
## if using GeoJSON key, coordinate system will need to be changed 
# geojson.key <- ""
# cas.sp <- st_read(geojson.key)


## Other files used: 

## import Territorial Authority Boundary. Data was downloaded from Koordinates.
tla <- st_read("./Input/TLABoundary/nz_TLABoundaries.shp")

## roadlines - from Waka Kotahi NZTA and Koordinates###

sh.centreline <- st_read("./Input/RoadShapefile/NZ-SHCentreline/nz-state-highway-centrelines.shp")
akl.roadline <- st_read("./Input/RoadShapefile/Auckland/roadlines_Auckland.shp")


## analysing only from 2016 onwards ###
cas.sp <- cas.sp %>% filter(crashYear>2015)

## Question 1: Is Crash Severity higher on State Highways than other roads? ####

summary1 <- cas.sp %>% st_drop_geometry() %>%
  group_by(crashSHDes, crashSever) %>%
  summarise(crash.count = n(),
            # fatal.crash.count = sum(crashSever == "Fatal Crash"),
            # serious.crash.count = sum(crashSever == "Serious Crash"),
            # minor.crash.count = sum(crashSever == "Minor Crash"),
            # non.injury = sum(crashSever == "Non-Injury Crash"),
            fatal.person.n = sum(fatalCount, na.rm = T),
            serInj.count.n = sum(seriousInj, na.rm = T),
            minInj.count.n = sum(minorInjur, na.rm = T))


## create levels for easy to follow visualisations
summary1$crashSever <- factor(summary1$crashSever, 
                              levels = c("Non-Injury Crash", "Minor Crash", 
                                         "Serious Crash", "Fatal Crash"))

## plot - 
ggplot(summary1 %>% filter(crashSHDes %in% c("Yes","No") )) +
  geom_bar(aes(crashSever, crash.count, fill = crashSHDes), 
           stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Crash Severity", 
       y = "Crashes Percentage",
       fill = "Crash on State Highway?") +
  scale_fill_manual(values = c("royalblue","goldenrod")) +
  theme_bw(base_size = 18) +
  theme(legend.position = "bottom")


## Question 2: Could weather influence driving conditions and lead to an increased likelihood of a crash? ####
## If weather is NOT "Fine" or "Null", it is assumed to have an influence 

## Weather likely to influence driving conditions - all that is not "Fine"? ###
cas.sp$wet.flag <- ifelse(cas.sp$weatherA %in% c("Fine"), "No", "Yes")
cas.sp$wet.flag <- ifelse(cas.sp$weatherA == "Null","Null", cas.sp$wet.flag)

summary2 <- cas.sp %>% st_drop_geometry() %>%
  group_by(wet.flag,crashSever) %>% 
  summarise(crash.count = n())

ggplot(summary2) +
  geom_bar(aes(crashSever, crash.count, fill = wet.flag), 
           stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Crash Severity", 
       y = "Percentage of crashes under each group",
       fill = "Could weather influence driving conditions?.") +
  scale_fill_manual(values = c("royalblue","grey","goldenrod")) +
  theme_bw(base_size = 18) +
  theme(legend.position = "bottom")


## Question 3: Could weather influence driving conditions, in high speed zones? ####

## speed groups
cas.sp$speedGroup <- cut(cas.sp$speedLimit, c(0,30,60,90,120))

summary3 <- cas.sp %>% st_drop_geometry() %>% 
  group_by(speedGroup, wet.flag) %>%
  summarise(crash.count = n(),
            fatal.crash.count = sum(crashSever == "Fatal Crash"),
            serious.crash.count = sum(crashSever == "Serious Crash"),
            minor.crash.count = sum(crashSever == "Minor Crash"),
            non.injury = sum(crashSever == "Non-Injury Crash"),
            fatal.person.n = sum(fatalCount, na.rm = T),
            serInj.count.n = sum(seriousInj, na.rm = T),
            minInj.count.n = sum(minorInjur, na.rm = T))

## death or serious injury count #
summary3$dsi.count <- summary3$fatal.crash.count + summary3$serious.crash.count

# plot - all crashes
ggplot(summary3 %>% filter(!is.na(speedGroup))) +
  geom_bar(aes(speedGroup, crash.count, fill = wet.flag), 
           stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Speed Limit", 
       y = "Percentage of crashes under each speed group",
       fill = "Could weather influence driving conditions?") +
  scale_fill_manual(values = c("royalblue","grey","goldenrod")) +
  theme_bw(base_size = 18) +
  theme(legend.position = "bottom")

# plot - all DSI crashes
ggplot(summary3 %>% filter(!is.na(speedGroup))) +
  geom_bar(aes(speedGroup, dsi.count, fill = wet.flag), 
           stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Speed Limit", 
       y = "Fatal or Serious Crashes Percentage under each group",
       fill = "Could weather influence driving conditions?") +
  scale_fill_manual(values = c("royalblue","grey","goldenrod")) +
  theme_bw(base_size = 18) +
  theme(legend.position = "bottom")


## Question 4: How many crashes where a bicycle is involved? ####


### summary by year ####
bicycle.summary <- cas.sp  %>%  filter(bicycle>=1) 

# st_write(bicycle.summary, "./Output/Bike-CAS_points.shp")
bicylce.summary <- bicycle.summary %>% st_drop_geometry()

bicycle.summary.year <- bicycle.summary %>% group_by(crashYear) %>%
  summarise(crash.count = n(),
            car = (sum(carStation>0) +sum(suv>0) + sum(taxi>0)),
            bus = sum(bus>0) + sum(schoolBus>0),
            truck = sum(truck>0),
            motorcycle = sum(motorcycle>0),
            moped = sum(moped>0),
            vanOrUtili = sum(vanOrUtili>0),
            otherVehic = sum(otherVehic>0),
            train = sum(train>0))

## plot - 
ggplot(bicycle.summary.year) +
  geom_line(aes(crashYear, car/crash.count), color = "royalblue", size = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0.5,1), name = "% of all bicycle crashes",
                     breaks = seq(0,1,0.05)) +
  scale_x_continuous(breaks = seq(2016,2021,1), name = "Year") +
  labs(color = "", title = "All bicycle crashes, where car involved") +
  theme_light(base_size = 19)

### summary by TLA ####
bicycle.summary.tla <- cas.sp %>% st_drop_geometry() %>% 
  filter(bicycle>=1) %>% group_by(tlaName) %>%
  summarise(crash.count = n(),
            car = (sum(carStation>0) +sum(suv>0) + sum(taxi>0)),
            bus = sum(bus>0) + sum(schoolBus>0),
            truck = sum(truck>0),
            motorcycle = sum(motorcycle>0),
            moped = sum(moped>0),
            vanOrUtili = sum(vanOrUtili>0),
            otherVehic = sum(otherVehic>0),
            train = sum(train>0))

bicycle.summary.tla <- bicycle.summary.tla %>% ungroup()

colnames(bicycle.summary.tla)[1] <- "NAME"
bicycle.summary.tla <- merge(tla,bicycle.summary.tla, all = T)

## plot - 
ggplot(bicycle.summary.tla) +
  geom_sf(aes(fill = car)) +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Number of crashes where car is involved (2016-2021)", fill = "Total crashes") 

## write TLA Shapefile - Use in QGIS for pretty plotting ##
# st_write(bicycle.summary.tla, "./Output/BicycleCrashes.shp")



## Find Car to Bike crash hotspots in Auckland ##
cas.sp.akl <- cas.sp %>% filter(tlaName == "Auckland") %>%  filter(bicycle>0 & 
                                                                     (carStation> 0 |suv>0 | taxi>0))

## all crashes within 50m of the roads 
roadbuffer <- st_buffer(akl.roadline,50)

roadbuffer$crbk <- lengths(st_intersects(roadbuffer,cas.sp.akl)) ## all crashes within 50 m of a road.

roadbuffer <- roadbuffer %>%st_drop_geometry()
roadbuffer <- roadbuffer %>% dplyr::select(ID,crbk)

cas.roadline.akl <- merge(akl.roadline,roadbuffer)

cas.roadline.akl <- st_zm(cas.roadline.akl, drop = TRUE, what = "ZM")
# summary(cas.roadline.akl)
# 
st_write(cas.roadline.akl, "./Output/Bike-CarCrashHotSpots.shp")


## Find Car crash hot spots along the State Highway ####

allSH <- cas.sp %>% filter(crashSHDes == "Yes")

sh.buffer <- st_buffer(sh.centreline, 10)
sh.buffer$cars <- lengths(st_intersects(sh.buffer,allSH)) ## all crashes within 50 m of a road.

sh.buffer <- sh.buffer %>%st_drop_geometry()
sh.buffer <- sh.buffer %>% dplyr::select(ID,cars)

cas.sh <- merge(sh.centreline,sh.buffer)

cas.sh <- st_zm(cas.sh, drop = TRUE, what = "ZM")
# summary(cas.roadline.akl)
# 
# st_write(cas.sh, "./Output/StateHighway-Crashes.Hotspots.shp")



## Additional Exploration ####

#### Question 5: Vehicles involved in a crash per TLA ####
vehicle.cas <- cas.sp %>% dplyr::select(OBJECTID,tlaName, speedLimit, crashSever, crashYear, 
                                        crashSHDes, weatherA, weatherB, light,
                                        carStation, suv, vanOrUtili, taxi, bus, schoolBus, 
                                        truck, motorcycle, moped,bicycle, pedestrian)

## wide to long for easy counting
vh.cas.long <- gather(vehicle.cas, vehicle, count,carStation:pedestrian)
vh.cas.long <- vh.cas.long %>% filter(count>0) ## remove all "0" values 
vh.cas.long.summary <- vh.cas.long %>% group_by(crashYear,vehicle) %>% count() %>% st_drop_geometry()

ggplot(vh.cas.long.summary) +
  geom_bar(aes(crashYear,n, fill = vehicle), stat = "identity")

tla.veh <- vh.cas.long %>% filter(crashYear == 2021) %>% st_drop_geometry() %>% 
  group_by(tlaName, vehicle) %>% count()

tla.veh <- tla.veh %>% ungroup()
colnames(tla.veh)[1] <- "NAME"

vh.cas.wide <- spread(tla.veh, NAME, vehicle)



