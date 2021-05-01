## Geographical Data Science and Spatial Data Analysis: An Introduction in R
## Lex Comber and Chris Brunsdon
## The code correct as of July 2020. Please let us know if it needs updating
## email: a.comber@leeds.ac.uk

## Chapter 5: EDA and Finding Structure in Data

library(tidyverse)
library(RColorBrewer)
library(GGally)
library(data.table)
library(sf)
library(ggspatial)
library(tmap)
library(grid)
library(gridExtra)


load("lsoa_result.RData")
load("ch4_db.RData")
ls()


summary(lsoa_result)      # summary
mean(social$unemployed)   # mean
sd(social$unemployed)     # standard deviation
var(social$unemployed)    # variance
IQR(social$unemployed)    # interquartile range


# overall
social %>%
  summarise(meanUn = mean(unemployed), sdUN = sd(unemployed))
# grouped by OAC
social %>%
  group_by(oac) %>%
  summarise(Count = n(), Mean_Un = mean(unemployed), SD_Un = sd(unemployed)) %>%
  ungroup() %>%
  arrange(desc(Mean_Un))


round(cor(social[, c(4, 5:11)]), 3)


cor.test(~unemployed+noqual, data = social)


cor.test(social$unemployed, social$noqual)


# specify ggplot with some mapping aesthetics
ggplot(data = <data>, mapping = <aes>) +
  geom_<function>()


p = ggplot(data = social, aes(x = unemployed, y = noqual)) +
  geom_point(alpha = 0.1)


p


p + geom_smooth(method = "lm")


p = ggplot(data = social, aes(x = unemployed, y = noqual)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")
p


ggplot(social) +
  geom_point(aes(x = unemployed, y = noqual), alpha = 0.1) +
  geom_smooth(method = "lm")


ggplot(social) +
  geom_point(aes(x = unemployed, y = noqual), alpha = 0.1) +
  geom_smooth(aes(x = unemployed, y = noqual), method = "lm")


ggplot(data = social, aes(x = unemployed, y = noqual)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")


ggplot(data = social, aes(x = unemployed, y = noqual)) +
  # specify point characteristics
  geom_point(alpha = 0.1, size = 0.7, colour = "#FB6A4A", shape = 1) +
  # specify a trend line and a theme/style
  geom_smooth(method = "lm", colour = "#DE2D26") +
  theme_minimal() 


ggplot(data = social, mapping = aes(x = unemployed,
                                    y = noqual, colour = oac)) +
  geom_point(size = 0.05, alpha = 0.3) +
  scale_colour_brewer(palette = "Set1") +
  # specify a trend line
  geom_smooth(method = "lm")


ggplot(data = social, aes(x = unemployed, y = noqual)) +
  # specify point characteristics
  geom_point(alpha = 0.1, size = 0.7, colour = "#FB6A4A", shape = 1) +
  # add a trend line
  geom_smooth(method = "lm", colour = "#DE2D26") +
  # specify the faceting and a theme / style
  facet_wrap("oac", nrow = 2) +
  theme_minimal() 


# a density plot
ggplot(social, aes(x = llti)) +
  geom_density()
# a histogram
ggplot(social, aes(x = llti)) +
  geom_histogram(bins = 30, col = "red", fill = "salmon")
# a boxplot
ggplot(social, aes(x = "", y = llti)) +
  geom_boxplot(fill = "dodgerblue", width = 0.2) +
  xlab("LLTI") + ylab("Value")


ggplot(social, aes(x = llti)) +
  geom_histogram(aes(y=..density..),bins = 30, 
                 fill="indianred", col="white") +
  geom_density(alpha=.5, fill="#FF6666")


ggplot(social, aes(x = llti)) +
	geom_density(aes(y=..density..), alpha=.5, fill="#FF6666")


ggplot() +
  geom_histogram(data = social, aes(x = llti, y=..density..),
                 bins = 30, fill="indianred", col="white") +
  geom_density(data = social, aes(x = llti),
               alpha=.5, fill="#FF6666")


ggplot(data = social, aes(y=..density..)) +
  geom_histogram(aes(x = llti), bins = 30,
                 fill="indianred", col="white") +
  geom_density(aes(x = noqual), alpha=.5, fill="#FF6666") +
  xlab("LLTI (bins) with lack of qualifications (curve)")


ggplot(social, aes(llti, stat(count), fill = oac)) +
  geom_density(position = "fill") +
  scale_colour_brewer(palette = "Set1")


library(RColorBrewer)
display.brewer.all()
brewer.pal(11, "Spectral")
brewer.pal(9, "Reds")



ggplot(social, aes(x = llti)) +
  geom_histogram(fill="firebrick3", bins = 30, col = "white") +
  facet_wrap( ~ oac, nrow = 2, scales = "fixed")


social %>%
  ggplot(aes(y=llti,fill = oac)) +
    # specify the facets
    facet_wrap(~oac, ncol = 2) +
    # flip the coordinates and specify the boxplot
    coord_flip() + geom_boxplot() +
    # specify no legend and the colour palette
    theme(legend.position = "none") +
    scale_fill_manual(name = "OAC class",
                      values = brewer.pal(8, "Spectral"))


social %>% 
  ggplot(aes(x = reorder(oac, llti, FUN = median), 
             y=llti, fill = oac)) + 
    # specify the box plot
    geom_boxplot(aes(group = oac), outlier.alpha=0.4, 
                 outlier.colour="grey25") +
    # specify the colour palette
    scale_fill_manual(name = "OAC class", 
                      values = brewer.pal(8, "Spectral")) +
    # flip the coordinates and specify the axis labels
    coord_flip() + xlab("") + ylab("LLTI") +
    # specify some styling 
    theme_minimal() + theme(legend.position = "none")


social %>% ggplot(mapping = aes(x = ft49, y = llti)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth()


plot(social[, c(4, 5:11)], cex = 0.2, col = grey(0.145,alpha=0.2))


plot(social[, c(4, 5:11)], cex = 0.2, col = grey(0.145,alpha=0.2),
     upper.panel=panel.smooth)


social %>% sample_n(2500) %>%  select(-c(1,2, 12:15)) %>% 
  ggpairs(lower = list(continuous = wrap("points", 
                                         alpha = 0.3, size=0.1))) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())  


social %>% sample_n(2500) %>% .[-c(1,2, 12:15)] %>%
  mutate(unemp_high = ifelse(unemployed > median(unemployed),
    "H","L")) %>%
  ggpairs(aes(colour = unemp_high, alpha = 0.4),
          upper = list(continuous = wrap('cor', size = 2.5)),
          lower = list(continuous = 
                         wrap("points", alpha = 0.7, size=0.1))) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())


# 1. create an index to extract the numeric variables 
index = which(as.vector(unlist(sapply(social, class))) == "numeric")
df = social[, index]
# 2. construct the correlation matrix
cor.mat <- round(cor(df),3)
# 3. pivot the correlation matrix to long format
lengthened <- pivot_longer(as_tibble(cor.mat,rownames = "row"),-row)
# checks: uncomment the below to see
# cor.mat
# head(lengthened)
# 4. then construct the main ggplot with geom_tile
ggplot(lengthened, aes(row, name, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#CB181D", high = "#2171B5", 
                       mid = "white", midpoint = 0, 
                       limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +  
  # make sure x and y have the same scaling
  coord_fixed() +
  # add the values to the tiles, using row, name as coordinates 
  geom_text(aes(row, name, label = value), 
            color = "black", size = 2) +
  # specify theme elements
  theme_minimal() +
  # adjust text direction on x-axis
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1,hjust = 1)) +
  # remove the axis labels by making them blank
  xlab("") +ylab("")


social %>% 
  ggplot(mapping = aes(x = llti, y = unemployed)) +
    geom_hex() + labs(fill='Count') +
    scale_fill_gradient(low = "lightgoldenrod1", high = "black")


social %>% 
  ggplot(mapping = aes(x = llti, y = noqual)) +
    geom_hex(bins=15) + 
    facet_wrap(~oac, nrow = 2) + labs(fill='Count') +
    scale_fill_gradient(low = "lemonchiffon", high = "darkblue")


social %>%
  # select the data
  select_if(is.numeric) %>%
  select(-oac_code, -population, -employed) %>%
  # generate variable means summaries
  mutate_if(is.numeric, mean) %>%
  # select a single row and transpose to something like
  # a tidy data frame with a fake "group"
  slice(1) %>% t() %>% data.frame(Group = "All") %>%
  # create a variable of the rownames and name the values
  rownames_to_column() %>%
  `colnames<-`(c("name", "value", "group")) %>%
  # arrange by name
  arrange(name) %>%
  # initiate the plot
  ggplot(aes(x=factor(name), y=value, group= group,
             colour=group, fill=group)) +
    # specify the points and areas for shading
    geom_point(size=2,) + geom_polygon(size = 1, alpha= 0.2) +
    # specify the polar plot
    coord_polar() +
    # apply some style settings
    theme_light() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_text(size = 6))


social %>%  
  # select the data 
  select_if(is.numeric) %>% select(-oac_code) %>%
  # rescale using z-scores for the numeric variables
  mutate_if(is.numeric, scale) %>%
  # generate group summaries
  aggregate(by = list(social$oac), FUN=mean)  %>%
  # lengthen to a tidy data frame
  pivot_longer(-Group.1) %>% 
  # rename one of the OAC classes (for plot labels that fit!)
  mutate(Group.1 = 
      str_replace(Group.1, 
                  pattern = "Cosmopolitan student neighbourhoods",
                  replacement = "Cosmopolitan student areas")) %>%
  # sort by the variable names - this is needed for the plot
  arrange(name) %>%
  # initiate the plot
  ggplot(aes(x=factor(name), y=value, 
             group= Group.1, colour=Group.1, fill=Group.1)) +
    # specify the points and areas for shading
    geom_point(size=2) + geom_polygon(size = 1, alpha= 0.2) + 
    # specify the shading
    scale_color_manual(values= brewer.pal(8, "Set2")) +
    scale_fill_manual(values= brewer.pal(8, "Set2")) +
    # specify the faceting and the polar plots
    facet_wrap(~Group.1, ncol = 4) +
    coord_polar() +
    # apply some style settings
    theme_light() +
    theme(legend.position = "none", axis.title = element_blank(),
          axis.text = element_text(size = 6))


pdf()
png()
tiff()


## pdf(file = "MyPlot.pdf", other settings)
## <code to produce figure or map>
## dev.off()


# open the file
png(filename = "Figure1.png", w = 7, h = 5, units = "in", res = 300)
# make the figure
social %>% ggplot(mapping = aes(x = llti, y = unemployed)) +
  geom_hex() + labs(fill='Count') +
  scale_fill_gradient(low = "lightgoldenrod1", high = "black")
# close the file
dev.off()


# using table in base R
as.data.frame(table(social$oac))
# using count from dplyr
count(social, oac)
# ordered by count/n
arrange(count(social, oac), n)
# in descending order
arrange(count(social, oac),-n)
# piped
social %>% count(oac) %>% arrange(-n)


social %>% count(oac) %>%  arrange(-n) %>% mutate(freq = n / sum(n))


# standard plot
ggplot(social, aes(x = factor(oac_code))) +
  geom_bar(stat = "count") + xlab("OAC")
# ordered using fct_infreq() from the forcats package
# (loaded with tidyverse)
ggplot(social, aes(fct_infreq(factor(oac_code)))) +
  geom_bar(stat = "count") + xlab("OAC")
# orientated a different way
ggplot(social, aes(y = factor(oac))) +
  geom_bar(stat = "count") + ylab("OAC")
# with colours - note the use of the fill in the mapping aesthetics
ggplot(social, aes(x = factor(oac_code), fill = oac)) +
  geom_bar(stat = "count")
# with bespoke colours with scale_fill_manual and a brewer palette
ggplot(social, aes(x = factor(oac_code), fill = oac)) +
  geom_bar(stat = "count") +
  scale_fill_manual("OAC class label",
                    values = brewer.pal(8, "Set1")) +
  xlab("OAC")


# using a y aesthetic and stat = "identity" to represent the values
g1 = social %>% group_by(oac_code) %>% 
  # summarise
  summarise(NoQual = sum(population * noqual), 
            L4Qual = sum(population * l4qual)) %>% 
  # make the result long
  pivot_longer(-oac_code) %>%
  # plot
  ggplot(aes(x=factor(oac_code), y=value, fill=name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual("Qualifications", 
                    values = c("L4Qual"="red", "NoQual"="orange")) +
  xlab("OAC") + theme_minimal()
# using just an x aesthetic and stat = "count" with fill and weight
g2 = social %>% group_by(oac_code) %>% 
  # summarise
  summarise(NoQual = sum(population * noqual), 
            L4Qual = sum(population * l4qual)) %>% 
  # make the result long
  pivot_longer(-oac_code) %>%
  # plot
  ggplot(aes(factor(oac_code))) + 
  geom_bar(stat="count", aes(fill=name, weight=value)) +
  scale_fill_manual("Qualifications", 
                    values = c("L4Qual"="red", "NoQual"="pink")) +
  xlab("OAC") + theme_minimal()
# plot both plots
gridExtra::grid.arrange(g1, g2, ncol=2)


social %>% group_by(oac_code) %>%
  summarise(NoQual = sum(population * noqual),
            L4Qual = sum(population * l4qual)) %>%
  pivot_longer(-oac_code) %>%
  ggplot(aes(x=factor(oac_code), y=value, fill=name)) +
  # Option 1: stacked on top (comment out the below)
  geom_col() +
  # Option 2: stacked side by side (uncomment the below)
  # geom_col(position=position_dodge()) +
  scale_fill_manual("Qualifications",
                    values = c("L4Qual"="red", "NoQual"="pink")) +
  xlab("OAC") + theme_minimal()


social %>% 
  select(oac, ruc11_code) %>% xtabs( ~ ., data = .) 


social %>% 
  select(oac, ruc11_code) %>%
  # recode the classes
  mutate(UR = ifelse(str_detect(ruc11_code , "A1|B1|C1|C2"), 
                     "Urban","Rural")) %>%
  # select the UR and OAC variables and pipe to xtabs
  select(oac, UR) %>% xtabs( ~ ., data = .) 


social %>% 
  select(oac, ruc11_code) %>%
  # create the summaries
  mutate(SP = ifelse(str_detect(ruc11_code , "1"), 
                     "Non-Sparse","Sparse")) %>%
  # select the SP and OAC variables and pipe to xtabs
  select(oac, SP) %>% xtabs( ~ ., data = .)


social %>% 
  mutate(`Employment quantile` = ntile(unemployed, 4)) %>%
  select(oac, `Employment quantile`) %>%
  xtabs( ~ ., data = .)


social %>%
  select(oac, ruc11_code) %>% xtabs( ~ ., data = .) %>%
  data.frame() %>%
  ggplot(aes(ruc11_code, oac, fill= Freq)) +
  geom_tile(col = "lightgray") +
  xlab("") + ylab("") + coord_fixed() +
  scale_fill_gradient(low = "white", high = "#CB181D") +
  theme_bw()


colours()
colors()


rgb_cols = col2rgb(c("red", "darkred", "indianred", "orangered"))
rgb_cols


rgb(rgb_cols, maxColorValue = 255)


brewer.pal(11, "Spectral")
brewer.pal(9, "Reds")


col_name = "orangered1"
col_rgb = t(col2rgb(col_name)/255)
col_hex = rgb(col_rgb, maxColorValue = 255)
# named colour
ggplot(data = social, aes(x = unemployed, y = noqual)) +
  geom_point(alpha = 0.1, size = 0.7,
             colour = col_name, shape = 1) +
  theme_minimal()
# hexadecimal colour
ggplot(data = social, aes(x = unemployed, y = noqual)) +
  geom_point(alpha = 0.1, size = 0.7,
             colour = rgb(col_rgb), shape = 1) +
  theme_minimal()
# RGB colour with conversion
ggplot(data = social, aes(x = unemployed, y = noqual)) +
  geom_point(alpha = 0.1, size = 0.7,
             colour = rgb(col_rgb), shape = 1) +
  theme_minimal()


set.seed(91234)
index = sample(1:length(colours()), 64)
# select colours
cols <- colours()[index]
# create a matrix of RGB values for each named colour
rgbmat <- t(col2rgb(cols))
# turn into hex formats
hexes <- apply(rgbmat, 1, function(x)
  sprintf("#%02X%02X%02X",x[1],x[2],x[3]))
# create backgrounds - foreground either black or white depending
# on lightness of colour
textcol <- ifelse(rowMeans(rgbmat) > 156, 'black', 'white')
# now create a data frame of cells with foreground/background colour
# spec text size
text_size = round(30/str_length(cols), 0)
df = data.frame(expand_grid(x = 1:8, y = 1:8), colours = cols,
                background = hexes, colour = textcol,
                text_size = text_size)
# apply to a plot
ggplot(df, aes(x, y, label = as.character(colours),
               fill = as.character(background),
               col = as.character(colour))) +
  geom_tile(col = "lightgray") +
  #geom_label(label.size = 0.05, col = as.character(df$colour))+
  geom_text(size = df$text_size, col = as.character(df$colour))+
  scale_fill_identity() +
  theme_minimal() + xlab("") +ylab("")+
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())


# extract data
prescriptions %>% filter(str_starts(bnf_code, "04030")) %>% head()


# add date
prescriptions %>% filter(str_starts(bnf_code, "04030")) %>%
  mutate(date = as.Date(paste0("2018-", month, "-01"))) %>% head()


prescriptions %>%
  # extract data
  filter(str_starts(bnf_code, "04030")) %>%
  # add date
  mutate(Date = as.Date(paste0("2018-", month, "-01"))) %>%
  # group by data and summarise
  group_by(Date) %>% summarise(Count = n()) %>%
  # and plot the line with a trend line
  ggplot(aes(x=Date, y=Count)) + geom_line() +
  geom_smooth(method = "loess")


prescriptions %>% 
  # extract data
  mutate(Condition = 
    ifelse(str_starts(bnf_code , "04030"), "SAD", 
    ifelse(str_starts(bnf_code , "03010"), "Asthma", "Others"))) %>% 
  filter(Condition != "Others") %>% 
  # add date
  mutate(Date = as.Date(paste0("2018-", month, "-01"))) %>% 
  # group by date and summarise
  group_by(Date, Condition) %>% summarise(Count = n()) %>% 
  ungroup() %>%
  # and plot the line with a trend line
  ggplot(aes(x=Date, y=Count, colour = Condition)) + geom_line() +
    geom_smooth(method = "loess") + theme_bw() +
    scale_color_manual(values = c("#00AFBB", "#E7B800"))


library(RSQLite)
db <- dbConnect(SQLite(), dbname="prescribing.sqlite")


library(RSQLite)
db <- dbConnect(SQLite(), dbname="data.in/prescribing.sqlite")


tbl(db, "prescriptions") %>%
  # extract data
  filter(bnf_code %like% '04030%') %>%
  # group by date and summarise
  group_by(month) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  # plot
  ggplot(aes(x=as.Date(paste0("2018-", month, "-15")), y=Count)) +
   geom_line() + xlab("Date") + geom_smooth(method = "loess")


tbl(db, "prescriptions") %>%
  # extract data
  filter(bnf_code %like% '04030%' | bnf_code %like% "03010%") %>%
  mutate(Condition = if_else(bnf_code %like% '04030%', "SAD", "Asthma")) %>%
  # group by date and condition and summarise
  group_by(month, Condition) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  # plot
  ggplot(aes(x=as.Date(paste0("2018-", month, "-15")),
             y=Count, colour = Condition)) +
    geom_line() + geom_smooth(method = "loess") +
    theme_bw() + xlab("Date") +
    facet_wrap(~Condition, nrow = 2, scales = "free") +
    scale_color_manual(values = c("#00AFBB", "#E7B800"))


dbDisconnect(db)


# define a clip polygon
ymax = 53.00; ymin = 52.907; xmin = -1.240; xmax = -1.080
pol = st_polygon(list(matrix(c(xmin,ymin,xmax,ymin,
                               xmax,ymax,xmin,ymax,xmin,ymin),
                             ncol=2, byrow=TRUE)))
# convert polygon to sf object with correct projection
pol = st_sfc(st_cast(pol, "POLYGON"), crs = 4326)
# re-project it
pol = st_transform(pol, 27700)
# clip out Nottingham from lsoa_sf making the geometry valid
nottingham = lsoa_sf[pol,] %>% inner_join(lsoa_result) %>% 
  st_make_valid() 


ggplot(nottingham) + geom_sf()


ggplot(nottingham) + geom_sf(aes(geometry = geom))


# directly
ggplot(nottingham) +
  geom_sf(aes(geometry = geom), colour = "darkgrey", fill = "azure")
# using a continuous variable and specify a thin line size
ggplot(nottingham) +
  geom_sf(aes(geometry = geom, fill = cost_pp), size = 0.1) +
  scale_fill_viridis_c(name = "Opioid costs \nper person")


ggplot(nottingham) +
  geom_sf(aes(geometry = geom, fill = cost_pp), size = 0.1) +
  scale_fill_viridis_c(name = "Opioid costs \nper person") + 
  annotation_scale(location = "tl") +
  annotation_north_arrow(location = "tl", which_north = "true", 
        pad_x = unit(0.2, "in"), pad_y = unit(0.25, "in"),
        style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = gray(.5), 
                                        linetype="dashed", size=0.5),
        panel.background = element_rect(fill="white"))


# select locations
set.seed(13)
labels.data = nottingham[sample(1:nrow(nottingham), 10), ]
labels.data = cbind(labels.data,
                    st_coordinates(st_centroid(labels.data)))
# create ggplot
ggplot(nottingham) +
  geom_sf(aes(geometry = geom), fill = "white", size = 0.1) +
  geom_point(data = labels.data, aes(x=X, y=Y), colour = "red") +
  geom_text(data = labels.data, aes(x=X, y=Y, label=lsoa_id),
    fontface = "bold", check_overlap = T) +
  theme_bw() +
  theme(axis.title = element_blank())


# tmap
p1 = nottingham %>% 
  mutate(cost_high = ifelse(cost_pp > median(cost_pp), 
                            "High","Low")) %>%
  tm_shape() + 
    tm_graticules(ticks = FALSE, col = "grey") +
    tm_fill("cost_high", title = "Cost pp.",
            legend.is.portrait = FALSE) +
    tm_layout(legend.outside = F, 
              legend.position = c("left", "bottom"))
# ggplot
p2 = nottingham %>% 
  mutate(cost_high = ifelse(cost_pp > median(cost_pp), 
                            "High","Low")) %>%
  ggplot() + 
    geom_sf(aes(geometry = geom, fill = cost_high), size = 0.0) +
    scale_fill_discrete(name = "Cost pp.") +
    theme_bw() +
    theme(panel.grid.major = element_line(
      color=gray(.5), linetype="dashed", size=0.3),
          panel.background = element_rect(fill="white"), 
      legend.position="bottom", 
          axis.text = element_text(size = 6)) 
# plot together using grid
library(grid)
# clear the plot window
dev.off()
pushViewport(viewport(layout=grid.layout(2,1)))
print(p1, vp=viewport(layout.pos.row = 1))
print(p2, vp=viewport(layout.pos.row = 2))


ggplot(nottingham) +
  geom_sf(aes(geometry = geom, fill = cost_pp), size = 0.0) +
  scale_fill_gradient2(low="#CB181D", mid = "white", high="#2171B5",
                       midpoint = median(nottingham$cost_pp),
                       name = "Unemployment\n around median") +
  theme_bw() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position = "bottom",
         legend.direction = "horizontal")


df = nottingham %>% st_drop_geometry() %>% inner_join(social) %>%
  select(-c(lsoa_id, population, employed, tot_cost,
            ruc11_code, ruc11, oac, oac_code))
regression.model <- lm(cost_pp ~ ., data = df)
round(summary(regression.model)$coefficients, 4)
ggcoef(regression.model, errorbar_height=.2,
       color ="red", sort="ascending") + theme_bw()


annotation_north_arrow

