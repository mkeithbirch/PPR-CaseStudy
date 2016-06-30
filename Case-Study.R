treedata <- read_excel('case study data.xlsx', sheet = "Data", col_names = T)
treedata <- treedata %>% 
  rename(date.inv = `Date Inventoried`, site.dev = `Site Development`, site.size = `Site Size`, 
         sci.name = `Scientific Name`, com.name = `Common Name`)
treedata <- treedata %>% 
  separate(sci.name, into = c("genus", "species"), sep = " ", remove = F) 

#Species abundance
treedata %>% 
  group_by(sci.name) %>% 
  summarize(spp.abund = n(), spp.pct = n()/2048) %>% 
  arrange(desc(spp.pct), sci.name)

#Abundance by genus
treedata %>% 
  group_by(genus) %>% 
  summarize(gen.abund = n(), gen.pct = n()/2048, taxlots.n = n_distinct(Address), taxlots.pct = taxlots.n/taxlots.tot) %>% 
  arrange(desc(gen.pct), genus)

#Load ALB host tree table
alb.hosts <- read_excel('case study data.xlsx', sheet = "alb.hosts", col_names = T)

alb.hosts <- alb.hosts %>% 
  separate(sci.name.host, into = c("genus", "species"), sep = " ", extra = "merge", remove = F)

#Condense alb.host tree table to genus level data
alb.host.gen <- alb.hosts %>% distinct(class, genus) 

treedata <- left_join(treedata, alb.host.gen, by = "genus")

treedata <- treedata %>% rename(alb.host.class = class)
  
#Abundance by alb.host.class
treedata %>% 
  group_by(alb.host.class) %>% 
  summarize(alb.host.class.abund = n(), alb.host.class.pct = n()/2048, taxlots.n = n_distinct(Address), taxlots.pct = taxlots.n/taxlots.tot) %>% 
  arrange(desc(alb.host.class.pct), alb.host.class)

#Potential properties impacted...

#First how many taxlots in the data set? Answer = 888
    taxlots.tot <- length(unique(treedata$Address))
# Number of maples per taxlot      
acer.taxlots <- treedata %>% filter(genus == "Acer") %>% group_by(Address) %>% summarise(acer.n = n()) %>% 
ggplot(aes(x=acer.n)) +
  geom_histogram(binwidth = 1)
# How many taxlots have Acer spp? Answer is number of rows in table of maples by taxlot. 221

# Distribution of DBH of all trees compared to Acers
treedata %>% ggplot(aes(x = DBH)) +
  geom_histogram() + 
  geom_histogram(aes(fill = alb.host.class))

# Another way to look at tree sizes
treedata %>% ggplot(aes(x = alb.host.class, y = DBH)) + 
  geom_point(position = "jitter")
  