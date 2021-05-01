## Geographical Data Science and Spatial Data Analysis: An Introduction in R
## Lex Comber and Chris Brunsdon
## The code correct as of July 2020. Please let us know if it needs updating
## email: a.comber@leeds.ac.uk

## Chapter 8: Alternative Spatial Summaries and Visualisations


library(maptools) 
library(tmap)
library(sf)
library(cartogram)
library(geogrid)
library(GISTools)
library(RSQLite)
library(rpart)
library(visNetwork)
library(tidyverse)


load("ch4_db.RData")


# join spatial and attribute data and pipe to tmap
lsoa_sf %>% left_join(social) %>%
  tm_shape() +
  tm_fill("llti", palette = "GnBu", 
          style = "kmeans", legend.hist = T) +
  tm_layout(title = "Limiting Long Term Illness", 
            frame = F, legend.outside = T, 
            legend.hist.width = 1, legend.format = list(digits = 1), 
            legend.outside.position = c("left", "top"), 
            legend.text.size = 0.7, legend.title.size = 1)


# create clip box
ymax = 53.00; ymin = 52.907; xmin = -1.240; xmax = -1.080
pol = st_polygon(list(matrix(c(xmin,ymin,xmax,ymin,
                               xmax,ymax,xmin,ymax,xmin,ymin), 
                             ncol=2, byrow=TRUE)))
pol = st_sfc(st_cast(pol, "POLYGON"), crs = 4326)
pol = st_transform(pol, 27700)
# clip out from lsoa_sf and join with social
lsoa_sf[pol,] %>% left_join(social) -> nottingham
# create a map of the original data 
p1 = tm_shape(nottingham) +
  tm_polygons("unemployed", palette = "viridis", style = "kmeans") +
  tm_layout(title = "Original", frame = F, legend.show=FALSE)


library(cartogram)
# contiguous cartogram 
n_cart <- cartogram_cont(nottingham, "unemployed", itermax=10)
p2 = tm_shape(n_cart)+
  tm_polygons("unemployed", palette = "viridis", style = "kmeans")+
  tm_layout(title = "Contiguous", frame = F, legend.show=FALSE)


n_ncart <- cartogram_ncont(nottingham, "unemployed")
p3 = tm_shape(nottingham)+tm_borders(col = "lightgrey")+
  tm_shape(n_ncart)+
  tm_polygons("unemployed", palette = "viridis", style = "kmeans") +
  tm_layout(title = "Non-contiguous", frame = F, legend.show=FALSE)


# without parameter tweaking! 
# n_dorl <- cartogram_dorling(nottingham, "unemployed")
# with parameter tweaking! 
n_dorl <- cartogram_dorling(nottingham, 
                            "unemployed", k = 0.5, 1, 5000)
p4 = tm_shape(nottingham)+tm_borders(col = "lightgrey")+
  tm_shape(n_dorl)+
  tm_polygons("unemployed", palette = "viridis", style = "kmeans")+
  tm_layout(title = "Dorling", frame = F, legend.show=FALSE)
tmap_arrange(p1,p2,p3,p4, ncol = 2)


# rescale the value
nottingham$u_sq = (nottingham$unemployed)^3
nottingham$u_exp = exp(nottingham$unemployed)
# create cartograms
n_cart_sq <- cartogram_cont(nottingham, "u_sq", itermax=10)
n_cart_ex <- cartogram_cont(nottingham, "u_exp", itermax=10)
# create tmap plots
p3 = 
  tm_shape(n_cart_sq)+
  tm_polygons("u_sq", palette = "viridis", style = "kmeans")+
  tm_layout(title = "Squares", frame = F, legend.show=FALSE)
p4 =   
  tm_shape(n_cart_ex)+
  tm_polygons("u_exp", palette = "viridis", style = "kmeans")+
  tm_layout(title = "Exponential", frame = F, legend.show=FALSE)
# plot with original 
tmap_arrange(p1, p2,p3,p4, ncol = 2)


hg = calculate_grid(nottingham, learning_rate = 0.05, 
                    grid_type = "hexagonal", verbose = F)


tm_shape(hg[[2]], bbox = nottingham)+tm_borders()+
  tm_shape(nottingham)+tm_borders(col = "darkgrey")+
  tm_layout(title = "Hex-bins", frame = F, legend.show=FALSE)


hg = assign_polygons(nottingham, hg)


sg = calculate_grid(nottingham, learning_rate = 0.05,
                    grid_type = "regular", verbose = F)
sg = assign_polygons(nottingham, sg)


p2 = tm_shape(hg)+
  tm_polygons("unemployed", palette = "viridis", style = "kmeans")+
  tm_layout(title = "Hexagon bins", frame = F, legend.show=FALSE)
p3 = tm_shape(sg)+
  tm_polygons("unemployed", palette = "viridis", style = "kmeans")+
  tm_layout(title = "Square bins", frame = F, legend.show=FALSE)
tmap_arrange(p1, p2,p3, ncol = 3)


load("ch7.RData")

# transform to OSGB projection
props = st_transform(properties, 27700)
ymax = 53.5066; ymin = 53.32943; xmin = -3.092724; xmax = -2.790667
pol = st_polygon(list(matrix(c(xmin,ymin,xmax,ymin,
                               xmax,ymax,xmin,ymax,xmin,ymin), 
                             ncol=2, byrow=TRUE)))
# convert polygon to sf object with correct projection
pol = st_sfc(st_cast(pol, "POLYGON"), crs = 4326)
# re-project it
pol = st_transform(pol, 27700)
# clip out Liverpool from lsoa_sf
liverpool = lsoa_sf[pol,]
# map
lg = st_make_grid(liverpool, 500, what = "polygons", square = F)
tm_shape(lg)+ tm_borders()+ tm_shape(st_geometry(props))+
  tm_dots(col = "#FB6A4A", alpha = 0.5, size = 0.15)


# convert to sp - the processing of values is just easier!
lg = as(lg, "Spatial")
props = as(props, "Spatial")
lg$house_count = colSums(gContains(lg, props, byid = TRUE))

# the sf code for doing this is below
# lg <- st_as_sf(lg)
# lg$ID = 1:nrow(lg)
# find points within polygons
# intersection <- st_intersection(lg, y = props)
# int_result <- intersection %>% group_by(ID) %>% count() %>% st_drop_geometry()
# lg <- lg %>% left_join(int_result) %>% rename(house_count = n)
# lg$house_count[is.na(lg$house_count)]<-0

# create a map with an OSM backdrop
tmap_mode("view")
tm_shape(lg)+
  tm_polygons("house_count", title = "Hexbin Counts",
              breaks = c(0, 1, 5, 10, 20, 40),
              palette = "Greens", alpha = 0.5, lwd = 0.2)+
  tm_layout(frame = F, legend.outside = T)+
  tm_basemap('OpenStreetMap') + tm_view(set.view = 10)
# reset viewing mode
tmap_mode("plot")


rm(list = ls())


# connect to the full database
db <- dbConnect(SQLite(), dbname="prescribing.sqlite")
# list tables and fields
dbListTables(db)
dbListFields(db, "patients")


tbl(db, "prescriptions") %>% 
  # step 1
  filter(bnf_code %like% '040303%') %>% 
  group_by(practice_id) %>%
  summarise(prac_cost = sum(act_cost, na.rm = T), 
            prac_items = sum(items, na.rm = T)) %>% 
  ungroup() %>%
  # step 2
  left_join(
    tbl(db, "patients") %>%
      group_by(practice_id) %>%
      summarise(prac_pats = sum(count, na.rm = T)) %>% 
      ungroup() %>%
      left_join(tbl(db, "patients")) %>% 
      mutate(pats_prop = as.numeric(count) / as.numeric(prac_pats))
  ) %>%
  # step 3
  mutate(lsoa_cost = prac_cost*pats_prop,
         lsoa_items = prac_items*pats_prop) %>%
  group_by(lsoa_id) %>%
  summarise(lsoa_tot_cost = sum(lsoa_cost, na.rm = T),
            lsoa_tot_items = sum(lsoa_items, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(lsoa_tot_cost)) %>%
  # step 4
  left_join(
    tbl(db, "social") %>% 
      mutate(population = as.numeric(population)) 
  ) %>%
  mutate(cost_pp = lsoa_tot_cost/population, 
         items_pp = lsoa_tot_items/population) %>%
  collect() -> lsoa_result


dbDisconnect(db)
names(lsoa_result) 
lsoa_result = lsoa_result[, c(1, 2, 3, 18,19, 4:17)]
lsoa_result %>% arrange(-cost_pp)


library(rpart)
library(visNetwork)
lsoa_result$oac = as.factor(lsoa_result$oac)
m1 <- rpart(cost_pp ~
              unemployed+noqual+l4qual+ptlt15+pt1630+ft3148+ft49+llti,
            data = lsoa_result, method  = "anova")
visTree(m1,legend=FALSE,collapse=TRUE,direction='LR')


load("ch4_db.RData")

lsoa_sf %>% left_join(lsoa_result) %>%
  tm_shape() + tm_fill("cost_pp", style = "kmeans",
                       palette = "YlOrRd", legend.hist = T) +
  tm_layout(frame = F, legend.outside = T, legend.hist.width = 1,
            legend.format = list(digits = 1),
            legend.outside.position = c("left", "top"),
            legend.text.size = 0.7, legend.title.size = 1)


hg = st_make_grid(lsoa_sf, 10000, what = "polygons", square = F)
hex = data.frame(HexID = 1:length(hg))
st_geometry(hex) = hg


# link to the LSOA areas and convert to points
lsoa_sf %>% left_join(lsoa_result) %>% st_centroid() %>%
  # intersect with hexbin layer 
  st_join(hex, join = st_within) -> ol


st_drop_geometry(ol) %>% group_by(HexID) %>%
  summarise(hex_cost = sum(lsoa_tot_cost),
            hex_items = sum(lsoa_tot_items),
            hex_pop = sum(population)) %>%
  ungroup() %>%
  mutate(cost_pp = hex_cost/hex_pop, 
         items_pp = hex_items/hex_pop) %>% 
  right_join(hex) -> hex_ssri
# add the geometry back 
st_geometry(hex_ssri) = hg


tm_shape(hex_ssri)+
  tm_polygons(c("cost_pp", "items_pp"), 
              palette = "viridis", style = "kmeans")


lsoa_sf %>% left_join(lsoa_result) %>% 
  select(lsoa_tot_cost, lsoa_tot_items, population) %>%
  st_make_valid() %>% st_interpolate_aw(hex, extensive = T) %>%
  mutate(cost_pp = lsoa_tot_cost/population, 
         items_pp = lsoa_tot_items/population) -> hex_ssri2


# map
tm_shape(hex_ssri2)+
  tm_polygons(c("cost_pp", "items_pp"), 
              palette = "viridis", style = "kmeans")

