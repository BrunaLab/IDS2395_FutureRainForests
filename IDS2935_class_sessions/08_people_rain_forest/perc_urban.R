library(tidyverse)
library(readxl)
library(here)
perc_urb<- read_excel("./IDS2935_class_sessions/08_people_rain_forest/WUP2018-F21-Proportion_Urban_Annual.xls", 
                      sheet = "Data",skip=16) 
rows<-nrow(perc_urb)
perc_urb <- perc_urb %>% 
  slice_tail(n=rows-12) %>% 
  select(-Index, -Note) %>% 
  rename("region"="Region, subregion, country or area", "country_code"="Country\ncode")

names(perc_urb)
region<-c("AFRICA"
,"Eastern Africa"
,"Middle Africa"
,"Northern Africa"
,"Southern Africa"
,"Western Africa"
,"ASIA"
,"Eastern Asia"
,"South-Central Asia"
,"Central Asia"
,"Southern Asia"
,"South-Eastern Asia"
,"Western Asia"
,"EUROPE"
,"Eastern Europe"
,"Northern Europe"
,"Southern Europe"
,"Western Europe"
,"LATIN AMERICA AND THE CARIBBEAN"
,"Caribbean"
,"Central America"
,"South America"
,"NORTHERN AMERICA"
,"OCEANIA"
,"Australia/New Zealand"
,"Melanesia"
,"Micronesia"
,"Polynesia")

region<-as.data.frame(region)
perc_urb<-anti_join(perc_urb,region)

non_trop<-c(
"Algeria"
,"Egypt"
,"Libya"
,"Morocco"
,"Sudan"
,"Tunisia"
,"Western Sahara"
,"Lesotho"
,"Swaziland"
,"Japan"
,"Mongolia"
,"Republic of Korea"
,"Kazakhstan"
,"Kyrgyzstan"
,"Tajikistan"
,"Turkmenistan"
,"Uzbekistan"
,"Afghanistan"
,"Bhutan"
,"Iran (Islamic Republic of)"
,"Nepal"
,"Pakistan"
,"Armenia"
,"Azerbaijan"
,"Bahrain"
,"Cyprus"
,"Georgia"
,"Iraq"
,"Israel"
,"Jordan"
,"Kuwait"
,"Lebanon"
,"Oman"
,"Qatar"
,"Saudi Arabia"
,"State of Palestine"
,"Syrian Arab Republic"
,"Turkey"
,"United Arab Emirates"
,"Yemen"
,"Belarus"
,"Bulgaria"
,"Czechia"
,"Hungary"
,"Poland"
,"Republic of Moldova"
,"Romania"
,"Russian Federation"
,"Slovakia"
,"Ukraine"
,"Channel Islands"
,"Denmark"
,"Estonia"
,"Faeroe Islands"
,"Finland"
,"Iceland"
,"Ireland"
,"Isle of Man"
,"Latvia"
,"Lithuania"
,"Norway"
,"Sweden"
,"United Kingdom"
,"Albania"
,"Andorra"
,"Bosnia and Herzegovina"
,"Croatia"
,"Gibraltar"
,"Greece"
,"Holy See"
,"Italy"
,"Malta"
,"Montenegro"
,"Portugal"
,"San Marino"
,"Serbia"
,"Slovenia"
,"Spain"
,"TFYR Macedonia"
,"Austria"
,"Belgium"
,"France"
,"Germany"
,"Liechtenstein"
,"Luxembourg"
,"Monaco"
,"Netherlands"
,"Switzerland"
,"Argentina"
,"Chile"
,"Uruguay"
,"Bermuda"
,"Canada"
,"Greenland"
,"Saint Pierre and Miquelon"
,"United States of America"
,"Australia"
,"New Zealand"
)

perc_urb<-perc_urb %>% 
  rename_with( ~ paste0("perc_urban_", .x), starts_with("19")|starts_with("20")) %>% 
  rename("country"="region")

non_trop<-as.data.frame(non_trop)
names(non_trop)<-c("country")

perc_urb<-anti_join(perc_urb,non_trop)

perc_urb$country<-tolower(perc_urb$country)

perc_urb_long<-perc_urb %>%
  pivot_longer(
    cols = starts_with("perc_urban_"),
    names_to = "year",
    names_prefix = "perc_urban_",
    values_to = "perc_urban",
    values_drop_na = TRUE
  )



data<-filter(perc_urb_long, country=="ecuador")
data<-filter(perc_urb_long, country=="indonesia")
data<-filter(perc_urb_long, country=="nigeria")
data
urban_plot<-ggplot(data, aes(x=year, y=perc_urban, group=country)) +
  geom_line()+
  geom_point()

urban_plot + theme_light()+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))