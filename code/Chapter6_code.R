## Geographical Data Science and Spatial Data Analysis: An Introduction in R
## Lex Comber and Chris Brunsdon
## The code correct as of July 2020. Please let us know if it needs updating
## email: a.comber@leeds.ac.uk

## Chapter 6: Modelling and Exploration of Data


library(tidyverse)
library(dbplyr)
library(biglm)
library(DBI)
library(RSQLite)
library(biglm)
library(lubridate)
library(sf)
library(tmap)
library(spdep)


select <- dplyr::select
count <- dplyr::count
rename <- dplyr::rename
mutate <- dplyr::mutate
filter <- dplyr::filter


library(tidyverse)
set.seed(299792458)
n_tests <- 1000000
probs <- sample(c(0.5,0.6),n_tests,replace=TRUE)
results <- tibble(Pr_head=probs,Heads=rbinom(n_tests,5,probs))
results %>% filter(Heads==4) %>% 
  count(Pr_head) %>% 
  mutate(prop=n/sum(n)) %>%
  select(-n) 


set.seed(299792458)
n_tests <- 1000000
probs <- sample(c(0.5,0.6),n_tests,replace=TRUE,prob=c(0.99,0.01))
results2 <- tibble(Pr_head=probs,Heads=rbinom(n_tests,5,probs))
results2 %>% filter(Heads==4) %>% 
  count(Pr_head) %>% 
  mutate(prop=n/sum(n)) %>%
  select(-n) 


factorial(5)/factorial(4)*(factorial(5-4)) *(0.5^4)*(1-0.5)^(5-4)


MASS::fractions(factorial(5)/factorial(4)*(factorial(5-4)) *(0.5^4)*(1-0.5)^(5-4))


MASS::fractions(factorial(5)/factorial(4)*(factorial(5-4)) *(0.6^4)*(1-0.6)^(5-4))


# prior probabilities
p_fake <- 0.5
p_gen  <- 0.5
# probability of 4 heads
p_4_fake <- dbinom(4,5,0.6)
p_4_gen  <- dbinom(4,5,0.5)
# posterior probability using Bayes
p_fake_after <- p_fake * p_4_fake / (p_fake * p_4_fake + p_gen * p_4_gen)
p_fake_after
# show fraction/ratio
MASS::fractions(p_fake_after)


p_fake <- 0.01
p_gen  <- 0.99
p_4_fake <- dbinom(4,5,0.6)
p_4_gen  <- dbinom(4,5,0.5)
p_fake_after <- p_fake * p_4_fake / (p_fake * p_4_fake + p_gen * p_4_gen)
p_fake_after

MASS::fractions(p_fake_after)


library(ggplot2)
set.seed(602214086)
n_tests <- 1000000
probs <- runif(n_tests)
results <- tibble(Pr_head=probs,Heads=rbinom(n_tests,20,probs))
ggplot(results %>% filter(Heads==13),aes(x=Pr_head)) + 
  geom_histogram(bins=40,fill='darkred')


# Here it is necessary to compute the histogram categories manually.
# 1. The following four are helper functions:
# Map each actual theta value to the bin category mid-point
cat_levels <- function(x,n) round(floor(x*n)/n + 1/(2*n),4)
# Compute the mid-points for each category
cat_centre <- function(n) round(seq(1/(2*n),1-1/(2*n),length=n),4)
# Compute the upper point for each category
cat_upper  <- function(n) seq(1/n,1,length=n)
# Compute the upper point for each category
cat_lower  <- function(n) seq(0,1-1/n,length=n)
# 2. Select out the results with 13 heads, map the thetas to the 
# bin mid-points and count them.  Label this the 'experimental'
# counts and store in 'results_h13'
results %>% filter(Heads==13) %>% 
  mutate(Pcat=cat_levels(Pr_head,40)) %>%
  count(Pcat) %>% 
  mutate(Type='Experimental') -> results_h13
# 3. Work out the theoretical counts and append them
n_13 <- sum(results_h13$n)
# 4. Compile a data table of the theoretical counts 
tibble(n=n_13 * 
             (pbeta(cat_upper(40),14,8) - 
              pbeta(cat_lower(40),14,8)),
              Pcat=cat_centre(40),
              Type="Theoretical") %>% 
  # 5. Join them to the experimental counts
  bind_rows(results_h13) %>%
  # Rename the columns and overwrite 'results_h13'
  rename(Count=n,Theta=Pcat) -> results_h13
# 6. Compare distributions via a plot
ggplot(results_h13,aes(x=Theta,y=Count,fill=Type)) +
  geom_col(position='dodge') 


results %>% 
  filter(Heads==13) %>% 
  mutate(in_tol = Pr_head > 0.49 & Pr_head < 0.51) %>% 
  count(in_tol) %>%
  transmute(in_tol,prob=n/sum(n))


set.seed(6371)
n_sims <- 1000000
theta <- ifelse(runif(n_sims) < 0.5,runif(n_sims),0.5)
results3 <- tibble(Theta=theta,Heads=rbinom(n_sims,20,theta))


results3 %>% 
  filter(Heads==13) %>% 
  mutate(Hp5=Theta==0.5) %>% 
  count(Hp5) %>% 
  transmute(Hp5,prob=n/sum(n))


results3 %>% 
  filter(Heads==13) %>% 
  ggplot(aes(x=Theta)) + geom_histogram(bins=40,fill='navyblue')


set.seed(271828) # Reproducibility!
# Simulate the future coin flipping
results3 %>% 
  filter(Heads==13) %>%
  mutate(`Heads predicted`=factor(rbinom(nrow(.),20,Theta),levels=0:20)) ->
predictions
# Show predicted outcome as a histogram
predictions %>%
  ggplot(aes(x=`Heads predicted`)) + 
  geom_bar(fill='firebrick') + 
  scale_x_discrete(drop=FALSE)


# define a factor to integer conversion function
factor_to_int <- function(x) as.integer(as.character(x))
predictions %>% 
  mutate(`Heads predicted`=factor_to_int(`Heads predicted`)) %>%
  summarise(`E(Heads predicted))`=mean(`Heads predicted`))


predictions %>% 
  mutate(`Heads predicted`=factor_to_int(`Heads predicted`)) %>%
  summarise(`Heads predicted - 2.5%`=quantile(`Heads predicted`,0.025),
            `Heads predicted -97.5%`=quantile(`Heads predicted`,0.975))


set.seed(6371)
# Because there are many more possible outcomes we need to
# increase the number of flips to get a reasonable sample with exactly 1300
# heads
n_sims <- 100000000
# Then things are done pretty much as before
theta <- ifelse(runif(n_sims) < 0.5,runif(n_sims),0.5)
results4 <- tibble(Theta=theta,Heads=rbinom(n_sims,2000,theta))
results4 %>% 
  filter(Heads==1300) %>% 
  mutate(Hp5=Theta==0.5) %>% 
  count(Hp5) %>% 
  transmute(Hp5,prob=n/sum(n))


set.seed(5780) # Reproducibilty!
# Simulate the future coin flipping
results4 %>% 
  filter(Heads==1300) %>%
  mutate(`Heads predicted`=factor(rbinom(nrow(.),20,Theta),levels=0:20)) ->
predictions2
# Show predicted outcome as a histogram
predictions2 %>%
  ggplot(aes(x=`Heads predicted`)) + 
  geom_bar(fill='firebrick') + 
  scale_x_discrete(drop=FALSE)


set.seed(12011)
# define an integer to factor conversion function
int_to_factor <- function(x) factor(x,levels=min(x):max(x))
results3 %>% 
  filter(Heads==13) %>%
  mutate(`Heads predicted`=
           int_to_factor(rnbinom(nrow(.),size=6,prob=Theta)+6)) -> predictions
predictions %>%
  ggplot(aes(x=`Heads predicted`)) + 
  geom_bar(fill='firebrick') + scale_x_discrete(drop=FALSE)


# load packages
library(tidyverse)
library(dbplyr)
library(sf)
library(tmap)
library(RColorBrewer)
library(spdep)


# get the current working directory
getwd()
# clear the workspace
rm(list = ls())
# download data
download.file("http://archive.researchdata.leeds.ac.uk/740/1/ch6.Rdata",
	"./ch6.RData", mode = "wb")
	
	
# load the data
load("ch6.RData")
# examine
ls()


log(Price)~unmplyd+Terraced+gs_area+ Ensuite+Garage+as.numeric(Beds)+Northing+u65


log(Price)~OAC+Terraced+Ensuite+Garage+as.numeric(Beds)+Northing


data_anal <- properties %>% 
  # 1. transform and calculate easting and northing
  st_transform(27700) %>% 
  mutate(Easting =  (properties %>% st_coordinates() %>% .[,1]) ) %>%
  mutate(Northing = (properties %>% st_coordinates() %>% .[,2]) ) %>% 
  # 2. join with OA data to attach socio-economic data
  st_join(oa) %>% 
  # 3. join to LSOA to attach the LSOA code
  select(-code) %>%
  st_join(lsoa %>% select(code)) %>%
  # 4. remove the sf geometry
  st_drop_geometry() %>%
  # 5. remove any records with NA values and select the desired variables
  drop_na() %>%
  select(Price,OAC,Northing,Easting,gs_area,code,unmplyd,
         Terraced,`En suite`,Garage,Beds,u65,gs_area) %>%
  mutate(code=as.character(code))


model1 <- lm(log(Price)~unmplyd+Terraced+`En suite`+Garage+as.numeric(Beds)+
               Northing+u65+gs_area, data = data_anal)

model2 <- lm(log(Price)~Terraced+`En suite`+Garage+as.numeric(Beds)+OAC+
               Northing, data = data_anal)


summary(model1)
summary(model2)


# Helper functions - here 
# 'm' refers to a model
# 'p' a prediction
# 's' a simulated set of prices
get_s2 <- function(m) sqrt(sum(m$residuals^2)/m$df.residual)
simulate <- function(m) {
  # get the model predictions of price
  p <- predict(m)
  # a random distribution with sd equal to the variance 
  r <- rnorm(length(p),mean=0,sd=get_s2(m))
  # create a simulated set of prices and return the result
  s <- exp(p + r)
  return(s)} 
# set up results matrices
m1_sims <- matrix(0,length(data_anal$Price),1000)
m2_sims <- matrix(0,length(data_anal$Price),1000)
# set a random seed and run the simulations
set.seed(19800518)
for (i in 1:1000) {
  m1_sims[,i] <- simulate(model1)
  m2_sims[,i] <- simulate(model2)
}


# determine LSOA mean house price
lsoa_mp <- data_anal %>% 
  group_by(code) %>% 
  summarise(mp=mean(Price)) %>% 
  left_join(lsoa,.) %>% 
  filter(!is.na(mp))
# create a weighted neighbour list of the LSOA areas
lw <- nb2listw(poly2nb(lsoa_mp))
# determine the spatial autocorrelation of LSOA mean house price 
target <- moran.test(lsoa_mp$mp,lw)$estimate[1]


# simulate Moran's I value 
sim_moran <- function(m_sims,lw) {
  res <- NULL
  for (i in 1:ncol(m_sims)) {
    data_sim <- data_anal %>% mutate(Price=m_sims[,i])
    lsoa_mp <- data_sim %>% 
      group_by(code) %>% 
      summarise(mp=mean(Price)) %>% 
      left_join(lsoa,.,by=c('code'='code')) %>% 
      filter(!is.na(mp))
    res <- c(res,moran.test(lsoa_mp$mp,lw)$estimate[1])
  }
  return(res)
}
# evaluate both models
test1 <- sim_moran(m1_sims,lw)
test2 <- sim_moran(m2_sims,lw)


t1 <- sum(abs(test1 - target) < 0.05)
t2 <- sum(abs(test2 - target) < 0.05)
c(t1,t2)


library(dbplyr)
library(dplyr)
library(biglm)
library(DBI)
library(RSQLite)


db <- dbConnect(SQLite(), dbname="prescribing.sqlite")


library(RSQLite)
db <- dbConnect(SQLite(), dbname="data.in/prescribing.sqlite")


cost_tab <-
  tbl(db, "prescriptions") %>% 
  # get the antidepressants
  filter(bnf_code %like% '0403%') %>%
  # join to practices (this will be done by practice_id)
  left_join(tbl(db, "practices")) %>% 
  # join to postcodes (this will be done by postcode)
  left_join(tbl(db, "postcodes")) %>% 
  select(act_cost, postcode, lsoa_id) %>%
  left_join(tbl(db,"social"))


class(cost_tab)


sql_render(cost_tab) %>% as.character() %>% cat()


# Obtain the query used to obtain the required data
cost_query <- sql_render(cost_tab) %>% as.character()
# Set up the query to the database connection db
cost_res <- dbSendQuery(db,cost_query)
# Fetch the first chunk
chunk <- dbFetch(cost_res,n=400000)
cost_lm <- biglm(act_cost~ unemployed + noqual,data=chunk)
# Keep on fetching chunks and updating the model until there are none left
repeat{
  chunk <- dbFetch(cost_res,n=400000)
  if (nrow(chunk) == 0) break
  cost_lm <- update(cost_lm,chunk)
}
# Close down the query
dbClearResult(cost_res)


# Obtain the query used to obtain the required data
cost_query <- sql_render(cost_tab) %>% as.character()
# Set up the query to the database connection db
cost_res <- dbSendQuery(db,cost_query)
# Fetch the first chunk
chunk <- dbFetch(cost_res,n=400000)
cost_lm <- biglm(act_cost~ unemployed + noqual,data=chunk)
# Keep on fetching chunks and updating the model until there are none left
counter = 1 # a counter to help with progress
repeat{
  chunk <- dbFetch(cost_res,n=400000)
  if (nrow(chunk) == 0) break
  cost_lm <- update(cost_lm,chunk)
  cat(counter, "\t")
  counter = counter+1
}
# Close down the query
dbClearResult(cost_res)


# Print the results
summary(cost_lm)


core_seq <- seq(0,15,l=101)
dists <- seq(1,6,by=1) %>%
  map(~tibble(y=core_seq,pdf=dgamma(core_seq,.x),nu=.x)) %>%
  bind_rows()
ggplot(dists,aes(x=y,y=pdf)) + geom_area(fill='indianred',alpha=0.8) + facet_wrap(~nu,scales='free_y')


make_fetcher <-function(db, query, chunksize,...){
     res <- NULL
     function(reset=FALSE){
     if(reset){
       res <<- dbSendQuery(db,query)
     } else{
       rval <-fetch(res,chunksize)
       if (nrow(rval)==0) {
            rval<-NULL
            dbClearResult(res)
       }
       return(rval)
     }
  }
}


library(lubridate) # To time the process
# Call the 'fetcher' function
cost_df <- make_fetcher(db,cost_query,500000)
# Run the glm
starting_point <- now()
cost_glm <- bigglm(act_cost~ unemployed + noqual,
                   data=cost_df,
                   family=Gamma(log),
                   start=c(4.2,-0.004,1.007),
                   maxit=35)
duration <- now() - starting_point
duration


summary(cost_glm)$mat


# Augment the dataquery 
set.seed(290162)
cost_glm_sample <- cost_tab %>% 
  mutate(r = abs(random() %% 1000000))  %>% 
  filter(r < 1000000*4443.0/3678372) %>% collect()
ss_cost_glm <- glm(act_cost~ unemployed + noqual,
                   data=cost_glm_sample,
                   family=Gamma(log))
coef(ss_cost_glm)


dbWriteTable(db,"glm_prescription_sample",cost_glm_sample)


# Call the 'fetcher' function
cost_df <- make_fetcher(db,cost_query,500000)
# Timing
start_point_1it <- now()
# Run the glm
cost_glm_1it <- bigglm(act_cost~ unemployed + noqual,
                   data=cost_df,
                   family=Gamma(log),
                   start=c(4.2,-0.004,1.007),
                   maxit=1)
duration_1it <- now() - start_point_1it
summary(cost_glm_1it)$mat


duration_1it


dbDisconnect(db)

