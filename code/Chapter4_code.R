## Geographical Data Science and Spatial Data Analysis: An Introduction in R
## Lex Comber and Chris Brunsdon
## The code correct as of July 2020. Please let us know if it needs updating
## email: a.comber@leeds.ac.uk

## Chapter 4: Creating Databases and Queries in R


library(stringr)
library(tidyverse)
library(DBI)
library(tmap)


# check your current working directory
getwd()
download.file("http://archive.researchdata.leeds.ac.uk/732/1/ch4_db.Rdata",
	"./ch4_db.RData", mode = "wb")


load("ch4_db.RData")
ls()


str(prescriptions)
head(practices)
as_tibble(postcodes)
summary(social)
head(patients)
lsoa_sf


library(png)
library(grid)
img <- readPNG("figures/ch4_data_table_relations.png")
grid.raster(img)


db = DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")


copy_to(db, prescriptions, name = "prescripts_db")


tbl(db, "prescripts_db")


dbWriteTable(conn = db, name = "prescripts_db", 
    value = prescriptions[, c("sha", "bnf_code", "act_cost", "month")],
    row.names = FALSE, header = TRUE, overwrite = T)
tbl(db, "prescripts_db")


copy_to(db,
        prescriptions[, c("sha","bnf_code","act_cost","month")],
        name = "prescripts_db", overwrite = T)


dim(as_tibble(dbReadTable(db, "prescripts_db")))
dim(tbl(db, "prescripts_db"))


tbl(db, "prescripts_db") %>%
  filter(sha == "Q49" & act_cost > 100)


tbl(db, "prescripts_db") %>%
  group_by(sha) %>%
  summarise(mean_cost = mean(act_cost, na.rm = T))


dbDisconnect(db)


dir.create("./data.db")
setwd("./data.db")


if (file.exists("prescribing_lite.sqlite") == TRUE) 
    file.remove("prescribing_lite.sqlite")
db = DBI::dbConnect(RSQLite::SQLite(), 
                    dbname="prescribing_lite.sqlite")


dbWriteTable(conn = db, name = "prescriptions", 
             value = prescriptions, 
             row.names = FALSE, header = TRUE)
dbWriteTable(conn = db, name = "practices", value = practices, 
             row.names = FALSE, header = TRUE)
dbWriteTable(conn = db, name = "patients", value = patients, 
             row.names = FALSE, header = TRUE)
dbWriteTable(conn = db, name = "postcodes", value = postcodes, 
             row.names = FALSE, header = TRUE)
dbWriteTable(conn = db, name = "social", value = social, 
             row.names = FALSE, header = TRUE)
dbDisconnect(db)


db <- DBI::dbConnect(RSQLite::SQLite(), 
                     dbname="prescribing_lite.sqlite")


# tables in the database
dbListTables(db)
# fields in the table
dbListFields(db, "postcodes")


tbl(db, "prescriptions") %>%
  group_by(sha) %>%
  summarise(
    mean_cost = mean(act_cost, na.rm = T),
    n = n()
  ) %>%
  ungroup() %>%
  arrange(desc(mean_cost)) %>%
  filter(n > 100) %>% print(n=10)


tbl(db, "social")
tbl(db, "practices")
tbl(db, "patients")
colnames(tbl(db, "prescriptions"))


format(object.size(tbl(db, "prescriptions")), unit="auto")
format(object.size(prescriptions), unit="auto")


dbDisconnect(db)


library(RSQLite)
db <- dbConnect(SQLite(), dbname="prescribing_lite.sqlite")


tbl(db, "prescriptions") %>%
  filter(bnf_code == "1404000H0AAAFAF") %>%
  arrange(desc(items))


tbl(db, "prescriptions") %>%
  filter(bnf_code %in% c("1404000H0AAAFAF", "1305020D0BEAAAI")) %>%
  arrange(desc(items))


# one pattern
tbl(db, "prescriptions") %>%
  filter(bnf_code %like% '%1404000H0%')
# multiple patterns
tbl(db, "prescriptions") %>%
  filter(bnf_name %like% 'Dermol%' |
           bnf_name %like% 'Fluad%')


tbl(db, "prescriptions") %>%
  filter(bnf_code == "1404000H0AAAFAF") %>%
  arrange(desc(items)) %>%
  filter(between(row_number(), 1, 5))


tbl(db, "social") %>%
  select(unemployed, llti)


tbl(db, "prescriptions") %>%
  select(starts_with("bnf_"))
tbl(db, "prescriptions") %>%
  select(contains("bnf_"))


?tidyselect::select_helpers


tbl(db, "prescriptions") %>%
  select(num_range = c(1,3))


# size of the call
tbl(db, "prescriptions") %>% object.size()
# size of a longer call
tbl(db, "prescriptions") %>%
  filter(bnf_code %like% '%1404%') %>%
  arrange(desc(act_cost)) %>% object.size()
# size of what is returned with collect
tbl(db, "prescriptions") %>%
  filter(bnf_code %like% '%1404%') %>%
  arrange(desc(act_cost)) %>% collect() %>% object.size()


tbl(db, "prescriptions") %>% 
  filter(bnf_code %like% '%1404000H0%') %>%
  inner_join(tbl(db, "practices")) %>% 
  inner_join(tbl(db, "postcodes")) %>% 
  select(lsoa_id, act_cost, items)


tbl(db, "prescriptions") %>%
  full_join(tbl(db, "practices")) %>% collect() %>% dim()
tbl(db, "prescriptions") %>%
  right_join(tbl(db, "practices")) %>% collect() %>% dim()


tbl(db, "prescriptions") %>% 
  filter(bnf_code %like% '%1404000H0%') %>%
  inner_join(tbl(db, "practices")) %>% 
  inner_join(tbl(db, "postcodes")) %>% 
  dplyr::select(sha, month, easting, northing, bnf_code, act_cost)


# entire dataset
tbl(db, "prescriptions") %>%
  summarise(total = sum(act_cost, na.rm = T))
# grouped by sha
tbl(db, "prescriptions") %>%
  group_by(sha) %>%
  summarise(total = sum(act_cost, na.rm = T)) %>%
  arrange(desc(total))
# grouped by sha and month
tbl(db, "prescriptions") %>%
  group_by(sha, month) %>%
  summarise(total = sum(act_cost, na.rm = T)) %>%
  arrange(desc(total))


tbl(db, "prescriptions") %>% 
  group_by(practice_id) %>%
  summarise(
    cost = sum(act_cost, na.rm = T), 
    n = n()) %>%
  mutate(mean_cost = cost/n) %>%
  arrange(desc(mean_cost))


tbl(db, "prescriptions") %>%
  group_by(practice_id) %>%
  summarise(
    cost = sum(act_cost, na.rm = T),
    n = n())


tbl(db, "prescriptions") %>%
  group_by(practice_id) %>%
  summarise(
    cost = sum(act_cost, na.rm = T),
    n = n()) %>%
  mutate(mean_cost = cost/n)


tbl(db, "prescriptions") %>%
  group_by(practice_id) %>%
  summarise(mean_cost = mean(act_cost, na.rm = T))%>%
  arrange(desc(mean_cost))


object.size(tbl(db, "prescriptions"))
object.size(dbReadTable(db, "prescriptions"))


tbl(db, "prescriptions") %>% 
  group_by(practice_id) %>%
  summarise(mean_cost = mean(act_cost, na.rm = T))%>%
  arrange(desc(mean_cost)) %>%
  show_query()


dbGetQuery(db,
  "SELECT `practice_id`, AVG(`act_cost`) AS `mean_cost`
  FROM `prescriptions`
  GROUP BY `practice_id`
  ORDER BY `mean_cost` DESC") %>% as_tibble()


dbDisconnect(db)

download.file("http://archive.researchdata.leeds.ac.uk/734/1/prescribing.sqlite",
	"./prescribing.sqlite", mode = "wb")


# connect to the database
db <- dbConnect(SQLite(), dbname="prescribing_lite.sqlite")


tbl(db, "prescriptions") %>%
  filter(bnf_code %like% '040702%')


tbl(db, "prescriptions") %>%
  filter(bnf_code %like% '040702%') %>%
  group_by(practice_id) %>%
  summarise(cost = sum(act_cost, na.rm = T)) %>%
  ungroup()


tbl(db, "patients") %>%
  group_by(practice_id) %>%
  summarise(prac_tot = sum(count, na.rm = T)) %>%
  ungroup() %>%
  left_join(tbl(db, "patients")) %>%
  mutate(prac_prop = as.numeric(count) / as.numeric(prac_tot))


tbl(db, "prescriptions") %>%
  filter(bnf_code %like% '040702%') %>%
  group_by(practice_id) %>%
  summarise(cost = sum(act_cost, na.rm = T)) %>%
  ungroup() %>%
  left_join(
    tbl(db, "patients") %>%
      group_by(practice_id) %>%
      summarise(prac_tot = sum(count, na.rm = T)) %>%
      ungroup() %>%
      left_join(tbl(db, "patients")) %>%
      mutate(prac_prop = as.numeric(count) / as.numeric(prac_tot))
  )


tbl(db, "prescriptions") %>%
  filter(bnf_code %like% '040702%') %>%
  group_by(practice_id) %>%
  summarise(cost = sum(act_cost, na.rm = T)) %>%
  ungroup() %>%
  left_join(
    tbl(db, "patients") %>%
      group_by(practice_id) %>%
      summarise(prac_tot = sum(count, na.rm = T)) %>%
      ungroup() %>%
      left_join(tbl(db, "patients")) %>%
      mutate(prac_prop = as.numeric(count) / as.numeric(prac_tot))
  ) %>%
  mutate(lsoa_cost = cost*prac_prop)


tbl(db, "prescriptions") %>%
  filter(bnf_code %like% '040702%') %>%
  group_by(practice_id) %>%
  summarise(cost = sum(act_cost, na.rm = T)) %>%
  ungroup() %>%
  left_join(
    tbl(db, "patients") %>%
      group_by(practice_id) %>%
      summarise(prac_tot = sum(count, na.rm = T)) %>%
      ungroup() %>%
      left_join(tbl(db, "patients")) %>%
      mutate(prac_prop = as.numeric(count) / as.numeric(prac_tot))
  ) %>%
  mutate(lsoa_cost = cost*prac_prop) %>%
  group_by(lsoa_id) %>%
  summarise(tot_cost = sum(lsoa_cost, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(tot_cost)) %>%
  arrange(desc(tot_cost))


tbl(db, "social") %>%
  mutate(population = as.numeric(population)) %>%
  select(lsoa_id, population)


tbl(db, "prescriptions") %>%
  filter(bnf_code %like% '040702%') %>%
  group_by(practice_id) %>%
  summarise(cost = sum(act_cost, na.rm = T)) %>%
  ungroup() %>%
  left_join(
    tbl(db, "patients") %>%
      group_by(practice_id) %>%
      summarise(prac_tot = sum(count, na.rm = T)) %>%
      ungroup() %>%
      left_join(tbl(db, "patients")) %>%
      mutate(prac_prop = as.numeric(count) / as.numeric(prac_tot))
  ) %>%
  mutate(lsoa_cost = cost*prac_prop) %>%
  group_by(lsoa_id) %>%
  summarise(tot_cost = sum(lsoa_cost, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(tot_cost)) %>%
  left_join(
    tbl(db, "social") %>%
      mutate(population = as.numeric(population)) %>%
      select(lsoa_id, population)
  ) %>%
  mutate(cost_pp = tot_cost/population) %>%
  arrange(desc(cost_pp))


# close the connection
dbDisconnect(db)
# connect to the full database
db <- dbConnect(SQLite(), dbname="prescribing.sqlite")


# close the connection 
dbDisconnect(db)
# connect to the full database
db <- dbConnect(SQLite(), dbname="data.in/prescribing.sqlite")


tbl(db, "prescriptions") %>% 
  filter(bnf_code %like% '040702%') %>% 
  group_by(practice_id) %>%
  summarise(cost = sum(act_cost, na.rm = T)) %>%
  ungroup() %>%
  left_join(
    tbl(db, "patients") %>%
      group_by(practice_id) %>%
      summarise(prac_tot = sum(count, na.rm = T)) %>%
      ungroup() %>%
      left_join(tbl(db, "patients")) %>% 
      mutate(prac_prop = as.numeric(count) / as.numeric(prac_tot))
  ) %>%
  mutate(lsoa_cost = cost*prac_prop) %>%
  group_by(lsoa_id) %>%
  summarise(tot_cost = sum(lsoa_cost, na.rm = T)) %>%
  ungroup() %>%
  filter(!is.na(tot_cost)) %>%
  left_join(
    tbl(db, "social") %>% 
      mutate(population = as.numeric(population)) %>% 
      select(lsoa_id, population) 
  ) %>%
  mutate(cost_pp = tot_cost/population) %>%
  collect() -> lsoa_result


dbDisconnect(db)


summary(lsoa_result) 
lsoa_result %>% arrange(desc(tot_cost))


length(which(patients$lsoa_id == "NO2011"))


lsoa_result %>% filter(!is.na(population)) -> lsoa_result


lsoa_result %>% arrange(desc(cost_pp))


tmap_mode("view")
tm_shape(lsoa_sf[lsoa_sf$lsoa_id == "E01018076",]) +
  tm_borders(lwd = 2) +
   tm_basemap(server= "OpenStreetMap")
tmap_mode("plot")


lsoa_sf %>% left_join(lsoa_result) %>%
  tm_shape() + 
  tm_fill("cost_pp", style = "quantile", palette = "GnBu", 
          title = "Rate per person", format = "Europe_wide")+ 
  tm_layout(legend.position = c("left", "top"))


con <- DBI::dbConnect(RMySQL::MySQL(),
  host = "database.rstudio.com",
  user = "hadley",
  password = rstudioapi::askForPassword("Database password")
)


save(list = c("lsoa_result", "lsoa_sf", "social"), 
     file = "lsoa_result.RData")


# save once and comment out
# save(list = c("lsoa_result", "lsoa_sf", "social"), file = "data.in/lsoa_result.RData")

