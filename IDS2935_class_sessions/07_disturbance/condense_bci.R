library(tidyverse)
load("./IDS2935_class_sessions/07_disturbance/doi_10.15146_5xcp-0d46__v2/bci.spptable.RData")
load("./IDS2935_class_sessions/07_disturbance/doi_10.15146_5xcp-0d46__v2/bci.tree/bci.tree8.RData")
taxonomy<-bci.spptable %>% 
  select(sp,Genus,Species,Family) %>% 
  mutate(species=paste(Genus, Species, sep= " ")) %>% 
  select(sp,species, family=Family)
names(taxonomy)
names(bci.tree8)
bci <- bci.tree8 %>% 
  left_join(taxonomy) %>% 
  filter(status=="A") %>% 
  select(treeID, sp, dbh, species, family) %>% 
  # filter(CensusID==171) %>% 
  # filter(CensusID==6) %>% 
  group_by(treeID) %>% 
  group_by(species) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% 
  filter(count>0)

head(bci,20)
str(bci)
write_csv(bci,"./IDS2935_class_sessions/07_disturbance/bci_for_class.csv")

bci<-bci %>% 
  mutate(csum = cumsum(count)) %>% 
  mutate(cperc = csum/sum(count))
bci
bci_last_50perc<-bci %>% slice_tail(n=(nrow(bci)/2))

p<-ggplot(bci, aes(x=reorder(species, desc(count)), y=count)) + 
  geom_bar(stat = "identity")+
  xlab("Species")+
  # scale_y_continuous(name = "Number of Trees", limits = c(0, 30000)) +
  scale_y_continuous(breaks=seq(0, 30000, 2000))+
  theme(axis.text.x = element_text(angle = 90))

p

hist(bci$count)
sum(bci$count)