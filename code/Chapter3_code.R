## Geographical Data Science and Spatial Data Analysis: An Introduction in R
## Lex Comber and Chris Brunsdon
## The code correct as of July 2020. Please let us know if it needs updating
## email: a.comber@leeds.ac.uk

## Chapter 3: A Framework for Processing Data: the Piping Syntax and dplyr



library(datasets)
library(tidyverse)
library(sf)
library(tmap)


getwd()


# download the package zip file
download.file("http://archive.researchdata.leeds.ac.uk/733/1/IBDSDA_0.1.2.tar.gz",
	"./IBDSDA_0.1.2.tar.gz", mode = "wb")
# install the package
install.packages("IBDSDA_0.1.2.tar.gz", type="source", repos=NULL)


library(IBDSDA)


# state_tidy
state_tidy <- as.data.frame(state.x77)
state_tidy$Division <- as.character(state.division)
state_tidy$Region <- as.character(state.region)
state_tidy$Name <- rownames(state.x77)
# AirPassengers_tidy
AirPassengers_df <- matrix(datasets::AirPassengers,12,12,byrow = TRUE)
rownames(AirPassengers_df) <- 1949:1960
colnames(AirPassengers_df) <-c("Jan","Feb","Mar","Apr","May","Jun",
                               "Jul","Aug","Sep","Oct","Nov","Dec")
AirPassengers_df <- data.frame(AirPassengers_df)
AirPassengers_df$Year <- rownames(AirPassengers_df)
AirPassengers_tidy <- gather(AirPassengers_df, 
                             key = Month, value = Count, -Year)
as_tibble(AirPassengers_tidy)



filter(arrange(state_tidy,Region,Division,`Life Exp`),`Life Exp` < 70)


arrange(
 summarise(
   group_by(
     mutate(state_tidy, Inc_Illit = Income/Illiteracy),
     Division),
   mean_II = mean(Inc_Illit)),
 mean_II)


state_tidy %>% arrange(Region,Division,`Life Exp`) %>%
 filter(`Life Exp` < 70)


state_tidy %>%
 mutate(Inc_Illit = Income/Illiteracy) %>%
 group_by(Division) %>%
 summarise(mean_II = mean(Inc_Illit))


state_tidy %>%
  mutate(Inc_Illit = Income/Illiteracy) %>% head()


# make a tibble
state_tidy %>% as_tibble() %>%
  mutate(Inc_Illit = Income/Illiteracy)


state_low_lifex <- 
 state_tidy %>% arrange(Region,Division,`Life Exp`) %>% 
 filter(`Life Exp` < 70)


state_tidy %>% arrange(Region,Division,`Life Exp`) %>% 
 filter(`Life Exp` < 70) -> state_low_lifex


state_tbl <- as_tibble(state_tidy)
state_tbl


state_tbl %>% select(Region, Division, Name, `Life Exp`)


state_tbl %>% select(Name, Division, Region, `Life Exp`)


state_tbl %>% select(Population:Murder)


state_tbl %>% select(-Population)


state_tbl %>% select(-(Population:Murder))


state_tbl %>% select(starts_with("Life"))


state_tbl %>% select(Name,`Life Exp`,everything())


state_tbl %>% dplyr::select(Name,`Life Exp`,everything())


state_tbl %>% mutate(LogIncome=log10(Income)) %>%
  select(Name, Income, LogIncome)


state_tbl %>% mutate(Income=log10(Income))


state_tbl %>% mutate(`Per Capita`=Income/Population) %>%
  select(Name, Income, Population, `Per Capita`)


state_tbl %>% mutate(`Per Capita`=Income/Population) %>%
 arrange(desc(`Per Capita`)) %>% 
 select(Name,`Per Capita`, everything()) -> state_per_cap
state_per_cap


state_per_cap <-
  select(
    arrange(
      mutate(state_tbl,`Per Capita`=Income/Population),
    desc(`Per Capita`)),
  Name,`Per Capita`, everything())


state_tidy %>% mutate(Division=as.character(state.division)) ->
 state_tidy


state_tbl %>% transmute(LogIncome=log10(Income),
                        LogPop=log10(Population))


state_tbl %>% 
 transmute(Name,Region,LogIncome=log10(Income),
           LogPop=log10(Population))


state_tbl %>% summarise(TotPop=sum(Population),TotInc=sum(Income))


state_tbl %>% group_by(Region)


state_tbl %>% group_by(Region) %>%
 summarise(TotPop=sum(Population),TotInc=sum(Income))


state_tbl %>% group_by(Division) %>%
 summarise(TotPop=sum(Population),TotInc=sum(Income))


state_tbl %>% group_by(Division) %>%
 summarise(TotPop=sum(Population),TotInc=sum(Income)) %>% 
 mutate(`Per Capita`=TotInc/TotPop) %>% arrange(desc(`Per Capita`)) %>% 
 select(Division,`Per Capita`,TotInc,TotPop) ->
 division_per_capita
division_per_capita


state_tbl %>% mutate(Dense=Population/Area > 0.25) %>% 
  group_by(Dense,Region) %>% 
  summarise(`Life Exp`=median(`Life Exp`), N=n())


state_tbl %>% transmute(Name,sle = 100*`Life Exp`/mean(`Life Exp`))


state_tbl %>% group_by(Region) %>%
 transmute(Name,region_sle = 100*`Life Exp`/mean(`Life Exp`))


state_tbl %>% group_by(Region) %>%
 transmute(Name,
 region_sle = 100*`Life Exp`/weighted.mean(`Life Exp`,Population))


state_tbl %>% transmute(Name,`Rank LE`=rank(`Life Exp`))


state_tbl %>% transmute(Name,`Rank LE`=rank(-`Life Exp`))


state_tbl %>% group_by(Region) %>%
 transmute(Name,`Rank LE`=rank(-`Life Exp`))


state_tbl %>% group_by(Region) %>%
 transmute(Name,`Illiteracy group`=ntile(Illiteracy,3)) %>%
 arrange(desc(`Illiteracy group`))


state_tbl %>% group_by(Region) %>% 
  filter(Illiteracy > mean(Illiteracy)) %>%
  select(Name,Illiteracy,everything())


state_tbl %>% group_by(Region) %>%
 arrange(Region,Illiteracy) %>% select(Name,Illiteracy)


AirPassengers_tbl <- as_tibble(AirPassengers_tidy)
AirPassengers_tbl %>% arrange(Year,Month) %>%
  transmute(Change=Count - lag(Count,n=12),Month) %>%
  group_by(Month) %>% summarise(Change=mean(Change, na.rm=TRUE))


## output <- method(input,method modifiers...)


input %>% method(method modifiers...) -> output


input %>% method1(method1 modifiers...) %>%
  method2(method modifiers2...) ...

real_estate_url="https://raw.githubusercontent.com/lexcomber/bookdata/main/sac.csv"
real_estate = read_csv(real_estate_url)


spec(real_estate)


as.character(95838)


real_estate %>% mutate(zip = as.character(zip)) -> real_estate2
real_estate2


spec(real_estate2)


real_estate <- read_csv(real_estate_url,
                        col_types=cols(zip=col_character()))


real_estate <- read_csv(real_estate_url,
                        col_types=cols(
                          zip=col_character(),
                          sq__ft=col_double(),
                          price=col_double()))
spec(real_estate)


real_estate <- read_csv(real_estate_url,
                        na=c("0","1551","2000","4897"),
                        col_types=cols(
                          zip=col_character(),
                          sq__ft=col_double(),
                          price=col_double()))


stem1 <- "https://www.psni.police.uk//globalassets/inside-the-psni/"
stem2 <- "our-statistics/police-recorded-crime-statistics"
filepath <- "/2017/november/monthly-crime-summary-tables-period-ending-nov-17.xls"
download.file(paste0(stem1,stem2,filepath),"psni.xls")


library(readxl)
crime_tab <- read_excel(path="psni.xls",
                        sheet="Bulletin Table 2",
                        range="A5:E31")
print(crime_tab, n = 26)


library(readxl)
crime_tab <- read_excel(path="psni.xls",
                        sheet="Bulletin Table 2",
                        range="A5:E31")


AirPassengers_df %>% gather(key="Month",
                            value="Count",-Year) %>% head


AirPassengers_df %>% gather(key="Month",value="Count",-Year) %>%
  as_tibble %>%
  mutate(Year=parse_integer(Year),
         Month=parse_factor(Month,
            levels=c("Jan","Feb","Mar","Apr","May","Jun",
                     "Jul","Aug","Sep","Oct","Nov","Dec")),
         Count=as.integer(Count))


flawed_data <-
  AirPassengers_df %>% gather(key="Month",value="Count",-Year) %>%
  as_tibble %>%
  mutate(Year=parse_integer(Year),
         Month=parse_integer(Month),
         Count=as.integer(Count))


AirPassengers_df %>% gather(key="Month",value="Count",-Year) %>%
  as_tibble %>%
  mutate(Year=parse_integer(Year),
         Month=parse_factor(Month,
            levels=c("Jan","Feb","Mar","Apr","May","Jun",
                     "Jul","Aug","Sep","Oct","Nov","Dec")),
         Count=as.integer(Count))  %>%
  group_by(Year) %>% summarise(Count=sum(Count))


AirPassengers_df %>% gather(key="Month",value="Count",-Year) %>%
  as_tibble %>%
  mutate(Year=parse_integer(Year),
         Month=parse_factor(Month,
            levels=c("Jan","Feb","Mar","Apr","May","Jun",
                     "Jul","Aug","Sep","Oct","Nov","Dec")),
         Count=as.integer(Count))  %>%
  group_by(Year) %>%
  mutate(`Pct of Yearly Total`=100*Count/sum(Count))


colnames(crime_tab)


crime_tab <- crime_tab %>%
  transmute(`Crime Type`=`...1`,
            `Year to Nov 2016`=`12 months to November 2016`,
            `Year to Nov 2017`=`12 months to \nNovember 20171,2`,
            `Change`=`Change between years`,
            `Change (Pct)`=`% change between years3`)
crime_tab


crime_tab <- crime_tab %>% transmute(`Crime Type`=`...1`,
                        `Year to Nov 2016`=`12 months to November 2016`,
                        `Year to Nov 2017`=`12 months to \nNovember 20171,2`,
                        `Change`=`Change between years`,
                        `Change (Pct)`=`% change between years3`) 


crime_tab %>% 
  mutate(`Broad class` = ifelse(
    cumany(`Crime Type`=="OTHER CRIMES AGAINST SOCIETY"),
    "Other",
    "Victim-Based")) 


crime_tab %>% 
  mutate(`Broad class` = ifelse(
    cumany(`Crime Type`=="OTHER CRIMES AGAINST SOCIETY"),
    "Other",
    "Victim based")) %>%
  filter(! is.na(Change))


toupper("Violence with injury")


`Crime Type` == toupper(`Crime Type`)


subclass_name = c("Violence against the person", "Sexual Offence",
                  "Robbery","Theft", "Criminal Damage","Drugs",
                  "Possession of Weapons", "Public Order", "Misc")
crime_tab %>%
  mutate(`Broad class` = ifelse(
    cumany(`Crime Type`=="OTHER CRIMES AGAINST SOCIETY"),
    "Other",
    "Victim based")) %>%
  filter(! is.na(Change)) %>%
  mutate(Subclass = subclass_name[cumsum(
    `Crime Type` == toupper(`Crime Type`))]) %>%
  filter(`Crime Type` != toupper(`Crime Type`))


subclass_name = c("Violence against the person","Sexual Offence",
                  "Robbery","Theft","Criminal Damage","Drugs",
                  "Possession of Weapons","Public Order","Misc")

crime_tab %>% 
  mutate(`Broad class` = ifelse(
    cumany(`Crime Type`=="OTHER CRIMES AGAINST SOCIETY"),
    "Other",
    "Victim based")) %>%
  filter(! is.na(Change)) %>%
  mutate(Subclass = subclass_name[cumsum(
    `Crime Type` == toupper(`Crime Type`))]) %>%
  add_count(Subclass) %>% 
  filter(`Crime Type` != toupper(`Crime Type`) | n == 1) %>% 
  select(-n) %>%
  mutate(`Crime Type` = sub("4","",`Crime Type`)) -> crime_tidy
crime_tidy


crime_tidy %>% group_by(Subclass) %>%
  transmute(`Crime Type`,
            `Pct of Subclass`=
              100*`Year to Nov 2016`/sum(`Year to Nov 2016`))


library(IBDSDA)
data(newhaven)


tracts <- newhaven$tracts
roads  <- newhaven$roads
callouts <- newhaven$callouts
blocks <- newhaven$blocks


library(tmap)
tmap_mode('plot')
tm_shape(blocks) + tm_borders() + 
  tm_shape(roads) + tm_lines(col='darkred') +
  tm_shape(callouts) + tm_dots(col="Callout",size=0.5,alpha=0.3) +
  tm_scale_bar(position=c('left','bottom')) + tm_compass() +
  tm_layout(legend.position = c('left',0.1))


unique(roads$AV_LEGEND)


roads %>% filter(AV_LEGEND=='HWAY PRIM') -> p_highway
p_highway


blocks %>%
  mutate(RentOcc = HSE_UNITS*P_RENTROCC/100 ) %>%
  group_by(TRACTBNA) %>%
  summarise(RentOcc=sum(RentOcc),TotVac=sum(VACANT)) %>%
  mutate(Vac2Rent=TotVac/RentOcc) %>%
  tm_shape() +
  tm_fill(col = c("RentOcc", "TotVac", "Vac2Rent"),
          style = "kmeans",
          title =c("Rent Occ.", "Vacant Props.", "Ratio"),
          palette = "YlGnBu", format = "Europe_wide") +
  tm_layout(legend.position = c("left", "bottom"))


callouts %>% st_join(tracts) %>% colnames


callouts %>% 
  filter(str_detect(Callout,"Burglary")) %>% 
  mutate(Callout = fct_drop(Callout)) -> 
  burgres


burgres %>% select(-geometry) %>% colnames


burgres %>% 
  st_join(tracts) %>% 
  as_tibble %>%
  select(-geometry) ->burgres_tab
burgres_tab


burgres %>% 
  st_join(tracts) %>% 
  st_drop_geometry() -> burgres_tab



burgres_tab %>% count(Callout,TRACTBNA) 


burgres_tab %>%
  group_by(Callout,TRACTBNA) %>%
  summarise(n=n()) %>%
  ungroup


burgres %>% 
  st_join(tracts) %>% 
  st_drop_geometry() %>%
  count(TRACTBNA,Callout) %>%
  complete(TRACTBNA,Callout,fill=list(n=0L)) ->
  burgres_counts
burgres_counts


full_mdl <- 
  glm(n~TRACTBNA*Callout,data=burgres_counts,family = poisson())
main_mdl <- 
  glm(n~TRACTBNA+Callout,data=burgres_counts,family = poisson())
anova(main_mdl,full_mdl)


callouts %>% 
  filter(!str_detect(Callout,"Burglary")) %>% 
  mutate(Callout = fct_drop(Callout)) %>% 
  st_join(tracts) %>% 
  st_drop_geometry() %>%
  count(TRACTBNA,Callout) %>%
  complete(TRACTBNA,Callout,fill=list(n=0L)) ->
  disturb_counts


full_mdl2 <- 
  glm(n~TRACTBNA*Callout,data=disturb_counts,family = poisson())
main_mdl2 <- 
  glm(n~TRACTBNA+Callout,data=disturb_counts,family = poisson())
anova(main_mdl2,full_mdl2)


disturb_counts %>% spread(Callout,n) 


disturb_counts %>% 
  spread(Callout,n) %>%
  transmute(TRACTBNA,
            PC_DISPUTE=100*`Family Dispute`/
              (`Family Dispute` + `Breach of Peace`)) %>%
  left_join(tracts,.) -> tracts2


tm_shape(tracts2) + 
  tm_polygons(col='PC_DISPUTE', 
              title='Family Disputes (%)') +
  tm_layout(legend.position = c('left','bottom'))


disturb_counts %>% 
  spread(Callout,n) %>%
  transmute(TRACTBNA,
          PC_FAM = 100*`Family Dispute`/sum(`Family Dispute`),
          PC_BOP = 100*`Breach of Peace`/sum(`Breach of Peace`)) %>%
  left_join(tracts,.) -> tracts3


tm_shape(tracts3) + 
  tm_polygons(col=c("PC_FAM","PC_BOP"),
              title=c("Family Disputes (%)",
                      "Breach of Peace (%)")) + 
  tm_layout(legend.position = c('left','bottom'))


roads %>%
  st_intersection(tracts) %>%
  filter(TRACTBNA==1408) ->
  roads1408
tm_shape(tracts) + tm_borders() +
  tm_shape(roads1408) + tm_lines()


roads %>% 
  st_intersection(tracts) %>% 
  filter(TRACTBNA==1408) ->
  roads1408


roads1408 %>% st_buffer(50)


roads1408 %>% 
  st_buffer(50) %>% 
  select(-TRACTBNA) %>%
  st_intersection(tracts) %>%
  filter(TRACTBNA == 1408) ->
  bufroads1408


tm_shape(bufroads1408) + tm_fill() + 
  tm_shape(roads1408) + tm_lines() + 
  tm_shape(tracts) + tm_borders() 


blocks %>% group_by(TRACTBNA) %>% 
  summarise(OO_SD=sqrt(Hmisc::wtd.var(
    P_OWNEROCC, OCCUPIED, method="ML"))) -> 
  tract_sd


tm_shape(tract_sd) +
  tm_polygons(col='OO_SD',
              title="Owner Occupation SD") +
  tm_layout(legend.position = c("left","bottom"))


tracts %>% 
  mutate(gp=1) %>% 
  group_by(gp) %>% 
  summarise %>% 
  st_boundary() -> bounds


tracts %>% 
  group_by(gp=1) %>% 
  summarise %>% 
  st_boundary() -> bounds


tm_shape(bounds) + tm_lines(lwd=4,col='dodgerblue') +
tm_shape(tracts) + tm_borders(lwd=2,col='darkred') +
tm_shape(blocks) + tm_borders()  

