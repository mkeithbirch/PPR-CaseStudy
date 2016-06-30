treedata <- read_excel('case study data.xlsx', sheet = "Data", col_names = T)
treedata <- treedata %>% 
  rename(date.inv = `Date Inventoried`, site.dev = `Site Development`, site.size = `Site Size`, 
         sci.name = `Scientific Name`, com.name = `Common Name`)
treedata <- treedata %>% 
  separate(sci.name, into = c("genus", "species"), sep = " ", remove = F) 

#Species abundance
treedata %>% 
  group_by(sci.name) %>% 
  summarize(spp.abund = n(), spp.pct = n()/2048) 

#Abundance by genus
treedata %>% 
  group_by(genus) %>% 
  summarize(gen.abund = n(), gen.pct = n()/2048) %>% 
  arrange(desc(gen.pct), genus)



  
treedata %>% filter(genus == "Acer")

  