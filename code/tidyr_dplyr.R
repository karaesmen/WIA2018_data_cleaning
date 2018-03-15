setwd("~/Google Drive/RLadiesColumbus/tidyr_dplyr/")

## ---- Let's start--------------------------------------

# install tidyverse
# install.packages("tidyverse")

# load tidyverse into workspace
library(tidyverse)

#### getting the vignettes ####

# many packages have vignettes that explain the general syntax of a package with examples
# some packages can have multiple vignettes designed for different operations/tasks
# you can simply check for vignettes by
vignette(package="tidyverse")

# open a vignette
vignette("manifesto",package="tidyverse")

# another way to see/open vignettes
browseVignettes(package = "tidyverse")


##### ------- tidyr ----------- #####

# open vignette for tidyr
vignette("tidy-data", package = "tidyr")

# set working direcotry to the directory you saved the files you dowloaded via dropbox
setwd("directory/you saved/the files/you dowloaded/via dropbox")

## -----------data structure 1-------------------------------------------------------------

# read in the csv file in R
(preg <- read.csv("preg.csv", stringsAsFactors = FALSE))


## -----------data structure 2-------------------------------------------------------------
read.csv("preg2.csv", stringsAsFactors = FALSE)


## ---- data semantics----------------------------------------------------

# tidy the data!
preg2 <- 
  preg %>% 
  gather(treatment, n, treatmenta:treatmentb) %>% 
  mutate(treatment = gsub("treatment", "", treatment)) %>%
  arrange(name, treatment)
preg2

######### Tidying messy datasets #########

## Messy data issues

# Column headers are values, not variable names.
# 
# Multiple variables are stored in one column.
# 
# Variables are stored in both rows and columns.
# 
# Multiple types of observational units are stored in the same table.
# 
# A single observational unit is stored in multiple tables.


## ------------ Column headers are values, not variable names ----------------------------

pew <- tbl_df(read.csv("pew.csv", stringsAsFactors = FALSE, check.names = FALSE))
pew

## ------------------------------------------------------------------------
pew %>%
  gather(inc, freq, -religion, `<$10k`, `$10-20k`, `$20-30k`, `$30-40k`:`$100-150k`) #`<$10k`:`$100-150k`

## ------------------------------------------------------------------------
billboard <- tbl_df(read.csv("billboard.csv", stringsAsFactors = FALSE))
billboard

## ------------------------------------------------------------------------
billboard2 <- billboard %>% 
  gather(week, rank, wk1:wk76, na.rm = TRUE)
billboard2 %>% data.frame %>% tail(20)

## ------------------------------------------------------------------------
billboard3 <- billboard2 %>%
  mutate(
    week =  readr::parse_number(week),
    date = as.Date(date.entered) + 7 * (week - 1)) %>%
  select(-date.entered)
billboard3 %>% head

## ------------------------------------------------------------------------
billboard3 %>% arrange(artist, track, week)

## ------------------------------------------------------------------------
billboard3 %>% arrange(date, rank)

## ----------------- Multiple variables stored in one column --------------
tb <- tbl_df(read.csv("tb.csv", stringsAsFactors = FALSE))
tb

## ------------------------------------------------------------------------
tb2 <- tb %>% 
  gather(demo, n, -iso2, -year, na.rm = TRUE)
tb2

## ------------------------------------------------------------------------
tb3 <- tb2 %>% 
  separate(demo, c("sex", "age"), 1)
tb3

## ---------- Variables are stored in both rows and columns-------------
weather <- tbl_df(read.csv("weather.csv", stringsAsFactors = FALSE))
weather

## ------------------------------------------------------------------------
weather2 <- weather %>%
  gather(day, value, d1:d31, na.rm = TRUE)
weather2

## ------------------------------------------------------------------------
weather3 <- weather2 %>% 
  mutate(day = readr::parse_number(day)) %>%
  select(id, year, month, day, element, value) %>%
  arrange(id, year, month, day)
weather3

## ------------------------------------------------------------------------
weather3 %>% spread(element, value)

## ----------- Multiple types in one table ------------------------

billboard3

song <- billboard3 %>% 
  select(artist, track, year, time) %>%
  unique() %>%
  mutate(song_id = row_number())
song

## ------------------------------------------------------------------------
rank <- billboard3 %>%
  left_join(song, c("artist", "track", "year", "time")) %>%
  select(song_id, date, week, rank) %>%
  arrange(song_id, date)
rank


## ----------- Homemade example  --------------------------------

# IceCream <- read.table(text="Person	IceCreamScoops	Toppings
# Jane	1_vanilla,1_rasberry,1_chocolate	walnut,chocolate_sauce
# Bernie	2_vanilla,1_chocolate	sprinkles
# Harry	2_chocolate,1_mint	sorbet
# Sandy	1_mango,1_vanilla	chocolate_sauce", header=T, stringsAsFactors=F, sep="\t")


IceCream <- read.table(text="
Person IceCreamScoops Toppings
Jane	vanilla_1,rasberry_2,chocolate_3	walnut
Bernie	vanilla_2,chocolate_1	sprinkles
Harry	chocolate_2,mint_1	sprinkles
Sandy	mango_1,vanilla_1	chocolate_sauce", header=T, stringsAsFactors=F)

IceCream

# Homemade mess!
# How do you find out number of flavors available?
# How do you get total number vanilla scoops eaten?
# With this form, it's impossible!

# Let's tidy
IceCream_fixed <- 
  IceCream %>% 
  separate(IceCreamScoops, paste0("flavor", 1:3), sep=",") %>% 
  gather(key= "scoop_order", value="scoop_flavor", flavor1:flavor3 ) %>% 
  # remove rows with NA
  na.omit %>% 
  # who cares about scoop order...
  select(-scoop_order) %>% 
  separate(scoop_flavor, c("scoop_flavor","n_scoop"), "_") %>% 
  # set n_scoop to numeric this is a dplyr function
  mutate(n_scoop=as.numeric(n_scoop))

IceCream %>% 
  separate(IceCreamScoops, paste("flavor", letters[1:3]  ,sep="_"), sep=",") %>%
  separate(flavor_a, c("type_of_flavor_a", "n_f_a"))
  


# average ice cream consumption by flavor (units: scoop)
#this is also from dplyr
IceCream_fixed %>% group_by(scoop_flavor) %>%
  summarise(mean(n_scoop)) %>% arrange(`mean(n_scoop)`)
  
##### ------- dplyr ----------- #####

vignette("introduction", package="dplyr")

## ------------------- Data: nycflights13 --------------------------------------
install.packages("nycflights13") # if this doesn't work try the next one

# install.packages("https://cran.r-project.org/src/contrib/nycflights13_0.2.2.tar.gz", 
#                  repos=NULL, method="libcurl")

library(nycflights13)
dim(flights)
head(flights)

## ------------------- Single table verbs -------------------------------------
# filter() (and slice())      : select rows 
# arrange()                   : sort table
# select() (and rename())     : select and rename columns
# distinct()                  : remove duplicated rows
# mutate() (and transmute())  : transform (and remove) a column
# summarise()                 : summarise multiple values to a single value
# sample_n() (and sample_frac()) : select n (or fraction of) rows at random


## ------------------- Filter rows with filter() ------------------------------

filter(flights, month == 1, day == 1)

## same filtering, alternative
#  filter(flights, month == 1 | month == 2)

## base R equivalent
#  flights[flights$month == 1 & flights$day == 1, ]


## ------------------- Select rows by position ------------------------------
slice(flights, 1:10)

## ------------------- Arrange rows with arrange() --------------------------
arrange(flights, year, month, day)
arrange(flights, sched_dep, dep_delay, day)
arrange(flights, desc(arr_delay))

## base R equivalent 
#  flights[order(flights$year, flights$month, flights$day), ]
#  flights[order(flights$arr_delay, decreasing = TRUE), ] or flights[order(-flights$arr_delay), ]

## ------------------- Select columns with select() ----------------------------
# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

# can also rename columns while selecting
select(flights, tail_num = tailnum, day1 = day)

## but rename is the best to change several columns without dropping others
rename(flights, tail_num = tailnum)

## ------------------- Extract distinct (unique) rows ---------------------------
distinct(flights, tailnum)
distinct(flights, origin, dest)

## ------------------- Add new columns with mutate() --------------------
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

## ------------------------------------------------------------------------
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
)

## ---- eval = FALSE-------------------------------------------------------
#  transform(flights,
#    gain = arr_delay - delay,
#    gain_per_hour = gain / (air_time / 60)
#  )
#  #> Error: object 'gain' not found

## ------------------- to keep the new variables, use transmute() ----------------------

transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)

## ------------------- Summarise values with summarise() --------------------
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))

## ------------------- Randomly sample rows with sample_n() and sample_frac() --------------------
sample_n(flights, 10)
sample_frac(flights, 0.01)

## ------------------- Grouped operations --------------------

by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20 , dist < 2000)

# Interestingly, the average delay is only slightly related to the
# average distance flown by a plane.
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

## ------------------------------------------------------------------------
destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
)

## ------------------------------------------------------------------------
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))


#### --------- Chaining Functions ----------- ####

## ---- save df each step  -------------------------------------------------------
#  a1 <- group_by(flights, year, month, day)
#  a2 <- select(a1, arr_delay, dep_delay)
#  a3 <- summarise(a2,
#    arr = mean(arr_delay, na.rm = TRUE),
#    dep = mean(dep_delay, na.rm = TRUE))
#  a4 <- filter(a3, arr > 30 | dep > 30)

## ------------ usual R way ---------------------------------------
filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

## ---- with the magrittr pipe ------------------------------------------------------
 flights %>%
   group_by(year, month, day) %>%
   select(arr_delay, dep_delay) %>%
   summarise(
     arr = mean(arr_delay, na.rm = TRUE),
     dep = mean(dep_delay, na.rm = TRUE)
   ) %>%
   filter(arr > 30 | dep > 30)
