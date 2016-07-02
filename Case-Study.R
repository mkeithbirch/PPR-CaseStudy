library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)


treedata <- read_excel('case study data.xlsx', sheet = "Data", col_names = T)
treedata <- treedata %>% 
  rename(date.inv = `Date Inventoried`, site.dev = `Site Development`, site.size = `Site Size`, 
         sci.name = `Scientific Name`, com.name = `Common Name`)
treedata <- treedata %>% 
  separate(sci.name, into = c("genus", "species"), sep = " ", remove = F) 

#Load ALB host tree table
alb.hosts <- read_excel('case study data.xlsx', sheet = "alb.hosts", col_names = T)

alb.hosts <- alb.hosts %>% 
  separate(sci.name.host, into = c("genus", "species"), sep = " ", extra = "merge", remove = F)

#Condense alb.host tree table to genus level data
alb.host.gen <- alb.hosts %>% distinct(class, genus) 

# Combine with treedata
treedata <- left_join(treedata, alb.host.gen, by = "genus")
# Rename
treedata <- treedata %>% rename(alb.host.class = class)
# ID non-host genera
treedata <- treedata %>% mutate(alb.host.class = ifelse(is.na(alb.host.class), "non-host",alb.host.class))
# Make new table of all genera and respective alb status
genera.alb.status <- treedata %>% 
  select(genus, alb.host.class) %>% unique() %>% 
  arrange(alb.host.class, genus)
  
#Species abundance
treedata %>% 
  group_by(sci.name) %>% 
  summarize(spp.abund = n(), spp.pct = n()/2048) %>% 
  arrange(desc(spp.pct), sci.name)

#Abundance by genus
genus.abund.summary <- treedata %>% 
  group_by(genus) %>% 
  summarize(gen.abund = n(), gen.pct = n()/2048, taxlots.n = n_distinct(Address), taxlots.pct = taxlots.n/taxlots.tot, 
            DBH.mean = mean(DBH), DBH.median = median(DBH)) %>% 
  merge(genera.alb.status, by = "genus") %>% 
  arrange(desc(gen.pct), genus)

#Abundance by alb.host.class
host.class.summary <- treedata %>% 
  group_by(alb.host.class) %>% 
  summarize(alb.host.class.abund = n(), alb.host.class.pct = n()/2048, taxlots.n = n_distinct(Address), taxlots.pct = taxlots.n/taxlots.tot, 
            DBH.mean = mean(DBH), DBH.median = median(DBH)) %>% 
  arrange(desc(alb.host.class.pct), alb.host.class)

#Potential properties impacted...

#First how many taxlots in the data set? Answer = 888
    taxlots.tot <- length(unique(treedata$Address))
# Number of maples per taxlot      
acer.taxlots <- treedata %>% filter(genus == "Acer") %>% group_by(Address) %>% summarise(acer.n = n()) %>% 

  ggplot(aes(x=acer.n)) +
  geom_histogram(binwidth = 1)
# How many taxlots have Acer spp? Answer is number of rows in table of maples by taxlot. 221

# How many taxlots have a potential host tree
taxlot.summary.hosts <- treedata %>% 
  filter(alb.host.class != "non-host") %>% summarize(taxlots.n = n_distinct(Address), taxlots.pct = taxlots.n/taxlots.tot)
  
treedata %>% 
  filter(alb.host.class != "non-host") %>% 
  group_by(Address, alb.host.class) %>% 
  summarize(host.n = n())

tl.summary <- treedata %>% 
  group_by(Address, alb.host.class) %>% 
  summarize(host.n = n(), DBH.min = min(DBH), DBH.max = max(DBH), DBH.mean = mean(DBH))

tl.summary %>% filter(alb.host.class = "p1")
  spread(alb.host.class, c(host.n, DBH.mean))
  

summarize(alb.host.class.abund = n(), alb.host.class.pct = n()/2048, taxlots.n = n_distinct(Address), taxlots.pct = taxlots.n/taxlots.tot, 
            DBH.mean = mean(DBH), DBH.median = median(DBH)) %>% 
  arrange(desc(alb.host.class.pct), alb.host.class)
# Distribution of DBH of all non-host trees compared to Acers and other host trees
treedata %>% ggplot(aes(x = DBH, color = alb.host.class)) +
    geom_freqpoly(binwidth = 1)

treedata %>% ggplot(aes(x = DBH, ..density.., color = alb.host.class)) +
  geom_freqpoly(binwidth = 2)
# Looks like potential host trees have similar size distributions to non-host trees.

# Another way to look at tree sizes
treedata %>% ggplot(aes(x = alb.host.class, y = DBH)) + 
  geom_point(position = "jitter", alpha = 0.5)
  