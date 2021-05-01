## Geographical Data Science and Spatial Data Analysis: An Introduction in R
## Lex Comber and Chris Brunsdon
## The code correct as of July 2020. Please let us know if it needs updating
## email: a.comber@leeds.ac.uk

## Chapter 2: Data and Spatial Data in R


library(tidyverse)
library(sf)
library(tmap)
library(sp)
library(datasets)


install.packages("tidyverse", dep = T)


# load the data
data(mtcars)
# create an identifier variable, ID, from the data frame row names
mtcars$ID = rownames(mtcars)
# sequentially number the data frame rows
rownames(mtcars) = 1:nrow(mtcars)
# extract a subset of the data
mtcars_subset = mtcars[1:5,c(12, 1:3)]
# pivot to long format
mtcars_long = pivot_longer(mtcars_subset, -ID)
# pivot to wide format
mtcars_wide = pivot_wider(mtcars_long)
mtcars_long
mtcars_wide



# create 4 variables
type = as.character(mtcars$ID)
weight = mtcars$wt
horse_power = mtcars$hp
q_mile <- mtcars$qsec
# create a new data.frame
df <- data.frame(type, weight, horse_power, q_mile)


str(df)


unique(df$type)


df <- data.frame(type, weight, horse_power, q_mile, 
                 stringsAsFactors = FALSE)
str(df)


tb <- tibble(type, weight, horse_power, q_mile)


str(tb)


head(df$ty)
head(tb$ty)


# a single column - the second one
head(df[,2])
head(tb[,2])
class(df[,2])
class(tb[,2])
# the first 2 columns
head(df[,1:2])
head(tb[,1:2])
class(df[,1:2])
class(tb[,1:2])


tb
df


data.frame(tb)
as_tibble(df)


vignette("tibble")


write.csv(df, file = "df.csv", row.names=FALSE)


df2 = read.csv("df.csv", stringsAsFactors = F)
str(df2)


# write a tab-delimited text file
write.table(df, file = "df.txt", sep = "\t", row.names = F,
            qmethod = "double")
df = read.table(file = "df.txt", header = T, sep= "\t",
                stringsAsFactors = F)
head(df2)
str(df2)



tb2 = read_csv("df.csv")
write_csv(tb, "tb.csv")
# write a tab-delimited text file
write_delim(tb, path = "tb.txt", delim = "\t",
            quote_escape = "double")


save(list = c("df"), file = "df.RData")


save(list = c("df", "tb"), file = "data.RData")


load(file = "data.RData")


ls()
rm(list = c("df", "tb"))
ls()
load(file = "data.RData")
ls()


save.image(file = "wksp.RData")


# load 9 R objects in a single but remote RData file
url = url("http://www.people.fas.harvard.edu/~zhukov/Datasets.RData")
load(url)
ls()
# read_csv / read.csv
real_estate_url="https://raw.githubusercontent.com/lexcomber/bookdata/main/sac.csv"
real_estate = read_csv(real_estate_url)
real_estate


# read the data
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = T)



# do a quick tmap of the data
qtm(nc)


# sf to sp
nc_sp <-as(nc, "Spatial")
# sp to sf
nc_sf = st_as_sf(nc_sp)


dim(nc)
head(nc)
class(nc)
str(nc)


nc


summary(nc)


# sf format
plot(nc)
plot(st_geometry(nc), col = "red")
# add specific counties to the plot
plot(st_geometry(nc[c(5,6,7,8),]), col = "blue", add = T)
# sp format
plot(nc_sp, col = "indianred")
# add specific counties to the plot
plot(nc_sp[c(5,6,7,8),], col = "dodgerblue", add = T)

# calculate rates
nc$NWBIR74_rate = nc$NWBIR74/nc$BIR74
nc$NWBIR79_rate = nc$NWBIR79/nc$BIR79
qtm(nc, fill = c("NWBIR74_rate", "NWBIR79_rate"))


# look at the field names
names(nc)
# select single field using $
nc$AREA
nc$AREA * 2
# select multiple fields
st_drop_geometry(nc[,c("AREA", "BIR79", "SID79")])
st_drop_geometry(nc[,c(1,12,13)])


names(nc)
# select first 50 elements from a single field using $
nc$AREA[1:50]
# randomly select 50 elements from a single field using $
nc$AREA[sample(50, nrow(nc))]
# select first 10 records from multiple fields
st_drop_geometry(nc[1:10 ,c("AREA", "BIR79", "SID79")])
st_drop_geometry(nc[1:10,c(1,12,13)])


# selecting spatial data with specific fields
nc[,1]
nc[,"AREA"]
nc[,c(1,12,13)]
nc[, c("AREA", "BIR79", "SID79")]
# selecting specific records and fields
nc[1:6 ,c(1,12,13)]
nc[c(1,5,6,7,8,19), c("AREA", "BIR79", "SID79")]
# plot records 5,6,7,8
plot(st_geometry(nc[c(5,6,7,8),]), col = "red")


library(sf)
vignette(package = "sf")


vignette("sf1", package = "sf")


save(list = "nc", file = "nc.RData")


load(file = "nc.RData")


st_write(nc, "nc.shp", delete_layer = T)


st_write(nc, dsn = "nc.shp", layer = "nc.shp",
         driver = "ESRI Shapefile", delete_layer = T)


# as GeoPackage
st_write(nc, "nc.gpkg", delete_layer = T)


vignette("sf2", package = "sf")


new_nc = st_read("nc.shp")
new_nc1
new_nc = st_read("nc.gpkg")
new_nc2


1. sf format
# create a copy of the nc data
nc2 <- nc
# 1a. remove the geometry by setting to NULL
st_geometry(nc2) <- NULL
class(nc2)
head(nc2)
# 1b. remove geometry using st_drop_geometry
nc2 = st_drop_geometry(nc)
class(nc2)
head(nc2)
2. sp format
# 2a. using data.frame
class(data.frame(nc_sp))
head(data.frame(nc_sp))
# 2b. using @data
class(nc_sp@data)
head(nc_sp@data)


AirPassengers


AirPassengers_df <-  matrix(datasets::AirPassengers,12,12,
                            byrow = TRUE)
rownames(AirPassengers_df) <- 1949:1960
colnames(AirPassengers_df) <- c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec")
AirPassengers_df <- data.frame(AirPassengers_df)
head(AirPassengers_df)


AirPassengers_df$Year <- rownames(AirPassengers_df)
AirPassengers_tidy <- gather(AirPassengers_df, key=Month, 
                             value=Count,-Year)
head(AirPassengers_tidy)


head(filter(AirPassengers_tidy,Month=="Mar"))


filter(AirPassengers_tidy,Month=="Mar",Year > 1955)


head(arrange(AirPassengers_tidy,Year))


head(arrange(AirPassengers_tidy,desc(Year)))


state_tidy <- as.data.frame(state.x77)
state_tidy$Division <- as.character(state.division)
state_tidy$Region <- as.character(state.region)
state_tidy$Name <- rownames(state.x77)


head(state_tidy)


head(arrange(state_tidy,Region,Division,`Life Exp`))


filter(arrange(state_tidy,Region,Division,`Life Exp`),`Life Exp` < 70)



tmp = mutate(state_tidy, pop_dens = Population/Area)
head(tmp)


tmp = select(
  mutate(state_tidy, pop_dens = Population/Area),
  Name, pop_dens)
head(tmp)


summarise(
  group_by(state_tidy, Division),
  mean_PopD = mean(Illiteracy),
  mean_Income = mean(Income),
  mean_HSG = mean(`HS Grad`)
  )


tmp = arrange(
        summarise(
          group_by(
            mutate(state_tidy, Inc_Illit = Income/Illiteracy),
          Division
        ),
        mean_II = mean(Inc_Illit)),
      mean_II)


ggplot(data = tmp, aes(x=mean_II, 
                       y=fct_reorder(Division, mean_II))) + 
  geom_point(stat='identity', fill="black", size=3)  +
  geom_segment(aes(y = Division, x = 0, yend = Division, 
                   xend = mean_II), color = "black") +
  labs(y = "Ordered Division", 
       x = "Mean Income to Illiteracy ratio") +
  theme(axis.text=element_text(size=7))


state_tidy %>%
  # create Inc_Illit
  mutate(Inc_Illit = Income/Illiteracy) %>%
  # group by Division
  group_by(Division) %>%
  # calculate group summaries
  summarise(mean_II = mean(Inc_Illit)) %>%
  # pass to ggplot
  ggplot(aes(x=mean_II, y=fct_reorder(Division, mean_II))) +
  geom_point(stat='identity', fill="black", size=3) +
  geom_segment(aes(y = Division, x = 0,yend = Division, xend = mean_II),
  	color = "black") +
  labs(y = "Ordered Division", x = "Mean Income to Illiteracy ratio") +
  theme(axis.text=element_text(size=7))


state_tidy %>%
  mutate(Inc_Illit = Income/Illiteracy)


state_tidy %>%
  mutate(Inc_Illit = Income/Illiteracy) %>%
  group_by(Division)


state_tidy %>%
  mutate(Inc_Illit = Income/Illiteracy) %>%
  group_by(Division) %>%
  summarise(mean_II = mean(Inc_Illit))


nc = mutate(nc, Rate = NWBIR74 / BIR74)
qtm(nc, "Rate", fill.palette = "Greens")


vignette("dplyr", package = "dplyr")


rm(list = ls())
getwd()


download.file("http://archive.researchdata.leeds.ac.uk/731/1/ch2.RData",
	"./ch2.RData", mode = "wb")
load("ch2.RData")
ls()


lsoa_data
lsoa
plot(st_geometry(lsoa), col = "thistle")


lsoa_join = inner_join(lsoa, lsoa_data)


lsoa$code = as.character(lsoa$code)
inner_join(lsoa, lsoa_data)


lsoa$code = as.character(lsoa$code)


names(lsoa_data)[1] = "ID"
lsoa_join = inner_join(lsoa, lsoa_data, by = c("code" = "ID"))
names(lsoa_data)[1] = "code"


## result <- JOIN_TYPE(x, y)


set.seed(1984)
lsoa_mis = sample_n(lsoa_data, 250)
# nest the join inside dim
dim(inner_join(lsoa_mis, lsoa))


dim(inner_join(lsoa, lsoa_mis))
dim(left_join(lsoa, lsoa_mis))
dim(right_join(lsoa, lsoa_mis))
dim(semi_join(lsoa, lsoa_mis))
dim(anti_join(lsoa, lsoa_mis))


?inner_join


vignette("two-table", package = "dplyr")


qtm(
  filter(
    mutate(
      inner_join(lsoa, lsoa_data),
    unemp_pc = (econ_active-employed)/emp_pop),
  unemp_pc > quantile(unemp_pc, 0.75)), "tomato")


lsoa %>% inner_join(lsoa_data) %>%
  mutate(unemp_pc = (econ_active-employed)/emp_pop) %>%
  filter(unemp_pc > quantile(unemp_pc, 0.75)) %>%
  qtm("tomato")


## tm_shape(data = <data>)+
##   tm_<function>()


tm_types = c("tm_dots, tm_bubbles",  "tm_lines", 
             "tm_fill, tm_borders, tm_polygons",
             "tm_text",
             "tm_format, tm_facet, tm_layout", 
             "tm_view, tm_compass, tm_credits, tm_scale_bar")
tm_desc = c("for points either simple or sized according to a variable", "for line features", 
            "for polygons / areas, with and without attributes and polygon outlines", 
            "for adding text to map features",
            "for adjusting cartographic appearance and map style, including legends",
            "for including traditional cartography elements")
df <- data.frame(Function= tm_types, Description = tm_desc)
kable (df, booktabs = TRUE, 
       #format = "html",
       caption = 'Commonly used `tmap` functions')



lsoa_join$UnempPC = (lsoa_join$unemployed/lsoa_join$econ_active)*100
lsoa_join$u25PC = (lsoa_join$u25/lsoa_join$age_pop)*100
lsoa_join$o65PC = (lsoa_join$o65/lsoa_join$age_pop)*100


tm_shape(lsoa_join)+
  tm_polygons("UnempPC")


p1 = tm_shape(lsoa_join)+
  tm_polygons("UnempPC", palette = "GnBu", border.col = "salmon",
    breaks = seq(0,35, 5), title = "% Unemployed")+
  tm_layout(legend.position = c("right", "top"), legend.outside = T)
p1


p1 + tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c(0.1, 0.1))


boundary = st_union(lsoa)
p1 + tm_shape(boundary) + tm_borders(col = "black", lwd = 2)


boundary = st_union(lsoa)


tm_shape(lsoa_join)+
  tm_fill(c("UnempPC", "o65PC"), palette = "YlGnBu",
    breaks = seq(0,40, 5), title = c("% Unemp", "% Over 65"))+
  tm_layout(legend.position = c("left", "bottom"))


tm_shape(lsoa_join)+
  tm_polygons("u25PC", title = "% Under 25", palette = "Reds",
    style = "kmeans", legend.hist = T)+
  tm_layout(title = "Under 25s in \nLiverpool",
            frame = F, legend.outside = T, 
            legend.hist.width = 1,
            legend.format = list(digits = 1), 
            legend.outside.position = c("left", "top"),
            legend.text.size = 0.7,
            legend.title.size = 1)+
  tm_compass(position = c(0.1, "0.1"))+
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(boundary) + tm_borders(col = "black", lwd = 2)


?tm_layout


lsoa_pts = st_centroid(lsoa_join)


# the 1st layer
tm_shape(boundary) +
  tm_fill("olivedrab4") +
  tm_borders("grey", lwd = 2) +
  # the points layer
  tm_shape(lsoa_pts) +
  tm_bubbles("o65PC", title.size = "% over 65s", scale = 0.8, col = "gold")


# the 1st layer
tm_shape(boundary) +
  tm_fill("grey70") +
  tm_borders(lwd = 2) +
  # the points layer
  tm_shape(lsoa_pts) +
  tm_dots("o65PC", size = 0.2, title = "% over 65s", shape = 19) +
  tm_layout(legend.outside = T, frame = F)




vignette("tmap-getstarted", package = "tmap")


library(png)
library(grid)
img <- readPNG("figures/lsoa_join.png")
grid.raster(img)
tmap_mode("view")


tmap_mode("view")
tm_shape(lsoa_join) +
    tm_fill(c("UnempPC"), palette = "Reds", alpha = 0.8) +
    tm_basemap('OpenStreetMap')


img <- readPNG("figures/lsoa_join2.png")
grid.raster(img)


tm_shape(lsoa_join) +
    tm_fill(c("UnempPC"), palette = "Reds", alpha = 0.8) +
    tm_basemap('Esri.WorldImagery')


tmap_mode("plot")


p1 <- tm_shape(lsoa_join) + 
  tm_fill("UnempPC", palette = "Reds", style = "quantile", 
          n = 5, title = "% Unemployed")+
  tm_layout(legend.position = c("left", "bottom"))+
  tm_shape(boundary) + tm_borders(col = "black", lwd = 2)
p2 <- tm_shape(lsoa_join) + 
  tm_fill("o65PC", palette = "YlGn", style = "quantile", 
          n = 5, title = "% Over 65")+
  tm_layout(legend.position = c("left", "bottom"))+
  tm_shape(boundary) + tm_borders(col = "black", lwd = 2)
p3 <- tm_shape(lsoa_join) + 
  tm_fill("u25PC", palette = "YlOrRd", style = "quantile", 
          n = 5, title = "% Under 25")+
  tm_layout(legend.position = c("left", "bottom"))+
  tm_shape(boundary) + tm_borders(col = "black", lwd = 2)
tmap_arrange(p1,p2,p3, nrow = 1)

