library(lattice)
library(reshape2)


load("pisa2018.Rdata")
ls()
pisa_df <- as.data.frame(newdata)


sum(is.na(newdata$MATH))
sum(is.na(newdata$READ))
sum(is.na(newdata$SCIE))
sum(is.na(newdata$GLCM))
sum(is.na(newdata$ST004D01T))
sum(is.na(newdata$CNTRYID))
sum(is.na(newdata$CNT))
unique(newdata$CNTRYID)
unique(newdata$CNT)

which(is.na(pisa_df$SCIE))
which(is.na(pisa_df$MATH))
which(is.na(pisa_df$READ))

NAs_subset <- pisa_df[606628:nrow(pisa_df), ]
subset_noNAs <- pisa_df[1:606627, ]
sum(is.na(subset_noNAs$MATH))
sum(is.na(subset_noNAs$SCIE))
sum(is.na(subset_noNAs$READ))
sum(is.na(subset_noNAs$ST004D01T))
which(is.na(subset_noNAs$ST004D01T))
subset_noNAs <- subset_noNAs[c(-108035,-121526), ]

sum(is.na(subset_noNAs$ST004D01T))
unique(subset_noNAs$ST004D01T)
sum(is.na(subset_noNAs$CNTRYID))


unique(NAs_subset$CNTRYID)
NAs_subset <- NAs_subset[rowSums(is.na(NAs_subset[, c("MATH", "READ", "SCIE")])) > 0, ]
na_counts <- table(NAs_subset$CNTID, useNA = "always")
print(na_counts)
length(unique(subset_noNAs$CNTRYID))
unique(subset_noNAs$ST003D03T)
"Vietnam" %in% subset_noNAs$CNTRYID
sum(is.na(subset_noNAs$ST005Q01TA))


unique(subset_noNAs$CNTRYID)
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "B-S-J-Z (China)", "China", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Baku (Azerbaijan)", "Azerbaijan", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Moscow Region (RUS)", "Russia", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Tatarstan (RUS)", "Russia", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Chinese Taipei", "China", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Russian Federation", "Russia", CNTRYID))

subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "United States", "USA", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Slovak Republic", "Slovakia", CNTRYID))



country_abbreviations <- c(
  "Albania" = "Albania", "United Arab Emirates" = "UAE", "Argentina" = "Argentina", "Australia" = "Australia",
  "Austria" = "Austria", "Belgium" = "Belgium", "Bulgaria" = "Bulgaria", "Bosnia and Herzegovina" = "Bosnia",
  "Belarus" = "Belarus", "Brazil" = "Brazil", "Brunei Darussalam" = "Brunei", "Canada" = "Canada",
  "Switzerland" = "Switzerland", "Chile" = "Chile", "Colombia" = "Colombia", "Costa Rica" = "Costa Rica",
  "Czech Republic" = "Czechia", "Germany" = "Germany", "Denmark" = "Denmark", "Dominican Republic" = "Dominican Rep.",
  "Spain" = "Spain", "Estonia" = "Estonia", "Finland" = "Finland", "France" = "France", "United Kingdom" = "UK",
  "Georgia" = "Georgia", "Greece" = "Greece", "Hong Kong" = "Hong Kong", "Croatia" = "Croatia", "Hungary" = "Hungary",
  "Indonesia" = "Indonesia", "Ireland" = "Ireland", "Iceland" = "Iceland", "Israel" = "Israel", "Italy" = "Italy",
  "Jordan" = "Jordan", "Japan" = "Japan", "Kazakhstan" = "Kazakhstan", "Korea" = "Korea", "Kosovo" = "Kosovo",
  "Lebanon" = "Lebanon", "Lithuania" = "Lithuania", "Luxembourg" = "Lux/bourg", "Latvia" = "Latvia", "Macao" = "Macao",
  "Morocco" = "Morocco", "Moldova" = "Moldova", "Mexico" = "Mexico", "North Macedonia" = "N. Macedonia", "Malta" = "Malta",
  "Montenegro" = "Montenegro", "Malaysia" = "Malaysia", "Netherlands" = "Netherlands", "Norway" = "Norway",
  "New Zealand" = "NZ", "Panama" = "Panama", "Peru" = "Peru", "Philippines" = "Philippines", "Poland" = "Poland",
  "Portugal" = "Portugal", "Qatar" = "Qatar", "Azerbaijan" = "Azerbaijan", "China" = "China",
  "Moscow Region (RUS)" = "Moscow", "Tatarstan (RUS)" = "Tat/stan", "Romania" = "Romania",
  "Russia" = "Russia", "Saudi Arabia" = "S. Arabia", "Singapore" = "Singapore", "Serbia" = "Serbia",
  "Slovakia" = "Slovakia", "Slovenia" = "Slovenia", "Sweden" = "SE", "Chinese Taipei" = "TW",
  "Thailand" = "Thailand", "Turkey" = "Turkey", "Ukraine" = "Ukraine", "Uruguay" = "Uruguay",
  "USA" = "USA"
)
library(dplyr)
library(tidyverse)

subset_noNAs$StateAb <- country_abbreviations[subset_noNAs$CNTRYID]


country_continent <- c(
  "Albania" = "Europe", "United Arab Emirates" = "Asia", "Argentina" = "South America",
  "Australia" = "Oceania", "Austria" = "Europe", "Belgium" = "Europe",
  "Bulgaria" = "Europe", "Bosnia and Herzegovina" = "Europe", "Belarus" = "Europe",
  "Brazil" = "South America", "Brunei Darussalam" = "Asia", "Canada" = "North America",
  "Switzerland" = "Europe", "Chile" = "South America", "Colombia" = "South America",
  "Costa Rica" = "North America", "Czech Republic" = "Europe", "Germany" = "Europe",
  "Denmark" = "Europe", "Dominican Republic" = "North America", "Spain" = "Europe",
  "Estonia" = "Europe", "Finland" = "Europe", "France" = "Europe", "United Kingdom" = "Europe",
  "Georgia" = "Europe", "Greece" = "Europe", "Hong Kong" = "Asia", "Croatia" = "Europe",
  "Hungary" = "Europe", "Indonesia" = "Asia", "Ireland" = "Europe", "Iceland" = "Europe",
  "Israel" = "Asia", "Italy" = "Europe", "Jordan" = "Asia", "Japan" = "Asia",
  "Kazakhstan" = "Asia", "Korea" = "Asia", "Kosovo" = "Europe", "Lebanon" = "Asia",
  "Lithuania" = "Europe", "Luxembourg" = "Europe", "Latvia" = "Europe", "Macao" = "Asia",
  "Morocco" = "Africa", "Moldova" = "Europe", "Mexico" = "North America",
  "North Macedonia" = "Europe", "Malta" = "Europe", "Montenegro" = "Europe",
  "Malaysia" = "Asia", "Netherlands" = "Europe", "Norway" = "Europe", "New Zealand" = "Oceania",
  "Panama" = "North America", "Peru" = "South America", "Philippines" = "Asia",
  "Poland" = "Europe", "Portugal" = "Europe", "Qatar" = "Asia", "Baku (Azerbaijan)" = "Asia",
  "B-S-J-Z (China)" = "Asia", "Moscow Region (RUS)" = "Europe",
  "Tatarstan (RUS)" = "Europe", "Romania" = "Europe", "Russian Federation" = "Europe",
  "Saudi Arabia" = "Asia", "Singapore" = "Asia", "Serbia" = "Europe",
  "Slovak Republic" = "Europe", "Slovenia" = "Europe", "Sweden" = "Europe",
  "Chinese Taipei" = "Asia", "Thailand" = "Asia", "Turkey" = "Asia",
  "Ukraine" = "Europe", "Uruguay" = "South America", "United States" = "North America"
)

subset_noNAs$Continent <- country_continent[match(subset_noNAs$CNTRYID, names(country_continent))]
any(is.na(subset_noNAs$Continent))


continent_colors <- c(
  "Europe" = "firebrick2", "Asia" = "steelblue3", "South America" = "green4",
  "North America" = "goldenrod3", "Oceania" = "black", "Africa" = "darkviolet"
)

mean_math_grade <- aggregate(cbind(MATH) ~ CNTRYID + ST004D01T, data = subset_noNAs, FUN = mean)
M_mean_math_grade <- mean_math_grade[mean_math_grade$ST004D01T == "Male", ]
F_mean_math_grade <- mean_math_grade[mean_math_grade$ST004D01T == "Female", ]

merged_math_grade <- merge(M_mean_math_grade, F_mean_math_grade, by = "CNTRYID", suffixes = c("_male", "_female"))
merged_math_grade$Continent <- country_continent[match(merged_math_grade$CNTRYID, names(country_continent))]
greece_row <- merged_math_grade[merged_math_grade$CNTRYID == "Greece", ]

continent_order <- c("Europe", "Asia", "South America", "North America", "Oceania", "Africa")

unique_continents <- unique(merged_math_grade$Continent)
unique_continents <- unique_continents[match(continent_order, unique_continents)]
unique_continents <- unique_continents[order(unique_continents)]

legend_colors <- continent_colors[unique_continents]

############################################################
######### Visualization 1: Scatter Plot with grades of ##### 
######### Males and Females in MATH ########################
############################################################

par(mfrow=c(1,1))
plot(x=merged_math_grade$MATH_male, y=merged_math_grade$MATH_female,
     xlab = "Mean Math Score for Males",
     ylab = "Mean Math Score for Females",
     main = "Mean Math Scores by Country and by Gender",
     pch = ifelse(merged_math_grade$CNTRYID == "Greece", 3, 20),  
     ylim = c(300,650), xlim = c(300,650),
     col = adjustcolor(continent_colors[merged_math_grade$Continent], alpha.f = 0.88))
abline(a = 0, b = 1, col = "black",lty=3)  # y = x line (gender equality)
legend("right", legend = unique_continents, fill = legend_colors)
text(
  greece_row$MATH_male, greece_row$MATH_female,
  labels = "GR", pos = 1, offset = -1)
text(320, 600, "Countries with higher female\n student performance", pos = 4, cex = 0.8) 
text(550, 340, "Countries with higher male\n student performance", pos = 4, cex = 0.8) 

###############################################
######## Visualization 2: Trellis Plot ########
###############################################

mean_grades <- aggregate(cbind(SCIE, MATH, READ) ~ StateAb, data = subset_noNAs, FUN = mean)
mean_grades_melt <- melt(mean_grades, id.vars = "StateAb", variable.name = "Subject")



b <- bwplot(value ~ Subject | StateAb, data = mean_grades_melt,
       main = "Mean Subject Scores for each Country",
       xlab = "Subject", ylab = "Mean Grade",
       scales = list(alternating = FALSE, x = list(labels = c("","","")), tick.number=4),
       horizontal = FALSE, ylim = c(300,640),
       layout = c(10,8), 
       panel = function(x, y, ...) {
         panel.bwplot(x, y, xlab="", ...)
         panel.points(x, y, col = ifelse(x == "SCIE", "purple", ifelse(x == "MATH", "darkred", "forestgreen")), pch = 16)  
       },
       key = list(
         space = "bottom",  
         columns = 3,  
         points = list(pch = 16, col = c("purple", "darkred", "forestgreen")), 
         text = list(c("Science", "Math", "Reading")),  
         title = "Subjects",  
         horiz = TRUE  
       ))

b


### Creating map graph ###
install.packages("tidyverse")
install.packages('maps')
library(tidyverse)  
library(maps)       
install.packages('mapdata')
library(mapdata)    
install.packages('sf')
library(sf)  
install.packages("RColorBrewer")
library(RColorBrewer)
library(countrycode)
library(ggmap)
library(ggplot2)
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(rnaturalearth)
library(rnaturalearthdata)


country_mapping <- data.frame(
  original_name = c("United Arab Emirates", "Brunei Darussalam", "Hong Kong", "Indonesia",
                    "Israel", "Jordan", "Japan", "Kazakhstan", "Korea", "Lebanon",
                    "Macao", "Malaysia", "Philippines", "Qatar", "Baku (Azerbaijan)",
                    "B-S-J-Z (China)", "Saudi Arabia", "Singapore", "Chinese Taipei",
                    "Thailand", "Turkey", "Albania", "Austria", "Belgium", "Bosnia and Herzegovina", "Belarus",
                    "Switzerland", "Czech Republic", "Germany", "Denmark", "Spain", "Romania", "Bulgaria",
                    "Estonia", "Finland", "France", "United Kingdom", "Georgia", "Greece",
                    "Croatia", "Hungary", "Ireland", "Iceland", "Italy", "Kosovo", "Lithuania",
                    "Luxembourg", "Latvia", "Moldova", "North Macedonia", "Malta", "Montenegro",
                    "Netherlands", "Norway", "Poland", "Portugal", "Serbia",
                    "Slovak Republic", "Slovenia", "Sweden", "Ukraine", "Canada", "Costa Rica", "Dominican Republic", "Mexico", "United States",
                    "Argentina", "Brazil", "Chile", "Colombia", "Peru", "Uruguay", "Panama",
  "Australia","New Zealand","Morocco", "Russian Federation","Moscow Region (RUS)","Tatarstan (RUS)"), 
  iso_standard_name = c("ARE", "BRN", "HKG", "IDN", "ISR", "JOR", "JPN", "KAZ", "KOR", "LBN",
                        "MAC", "MYS", "PHL", "QAT", "AZE", "CHN", "SAU", "SGP", "TWN", "THA",
                        "TUR",  "ALB", "AUT", "BEL", "BIH", "BLR", "CHE", "CZE", "DEU", "DNK", "ESP",
                        "ROU","BGR","EST", "FIN", "FRA", "GBR", "GEO", "GRC", "HRV", "HUN", "IRL", "ISL",
                        "ITA", "XKX", "LTU", "LUX", "LVA", "MDA", "MKD", "MLT", "MNE", "NLD",
                        "NOR", "POL", "PRT", "SRB", "SVK", "SVN", "SWE", "UKR", "CAN", "CRI", "DOM", "MEX", "USA",
                        "ARG", "BRA", "CHL", "COL", "PER", "URY","PAN","AUS","NZL","MAR","RUS","RUS","RUS")) 


subset_noNAs <- merge(subset_noNAs, country_mapping, by.x = "CNTRYID", by.y = "original_name", all.x = TRUE)

mismatched_codes <- subset_noNAs$CNTRYID[is.na(subset_noNAs$iso_standard_name)]
missing_codes <- country_mapping$original_name[!country_mapping$original_name %in% subset_noNAs$CNTRYID]
missing_values <- sum(is.na(subset_noNAs$iso_standard_name))

print(paste("Mismatched codes:", unique(mismatched_codes)))
print(paste("Missing codes in mapping:", unique(missing_codes)))
print(paste("Missing values in dataset:", missing_values))

install.packages("countrycode")
library(countrycode)

iso_standard_name <- c("ARE", "BRN", "HKG", "IDN", "ISR", "JOR", "JPN", "KAZ", "KOR", "LBN",
                       "MAC", "MYS", "PHL", "QAT", "AZE", "CHN", "SAU", "SGP", "TWN", "THA",
                       "TUR",  "ALB", "AUT", "BEL", "BIH", "BLR", "CHE", "CZE", "DEU", "DNK", "ESP",
                       "ROU","BGR","EST", "FIN", "FRA", "GBR", "GEO", "GRC", "HRV", "HUN", "IRL", "ISL",
                       "ITA", "XKX", "LTU", "LUX", "LVA", "MDA", "MKD", "MLT", "MNE", "NLD",
                       "NOR", "POL", "PRT", "SRB", "SVK", "SVN", "SWE", "UKR", "CAN", "CRI", "DOM", "MEX", "USA",
                       "ARG", "BRA", "CHL", "COL", "PER", "URY","PAN","AUS","NZL","MAR","RUS","RUS","RUS")

valid_codes <- countrycode(iso_standard_name, origin = "iso2c", destination = "iso2c")

# Print the valid ISO country codes
print(valid_codes)



print("Merged dataset columns:")
print(colnames(merged_mapdata))
print("Unique ISO codes in merged dataset:")
print(unique(merged_mapdata$iso_a3))
print("Unique ISO codes in original datasets:")
print(unique(world$iso_a3))
print(unique(merged_mean_read_grade$iso_standard_name))

print("Missing values in merged dataset:")
print(sum(is.na(merged_mapdata)))

print("Unique ISO codes in merged dataset:")
print(length(unique(merged_mapdata$iso_a3)))
print("Unique ISO codes in original datasets:")
print(length(unique(world$iso_a3)))
print(length(unique(merged_mean_read_grade$iso_standard_name)))

subset_noNAs<- subset_noNAs[,c(-18,-19,-20,-21)] 



subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Russian Federation", "Russia", CNTRYID))

subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "United States", "USA", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Slovak Republic", "Slovakia", CNTRYID))


pisa_data_summ <- subset_noNAs %>%
  group_by(CNTRYID, ST004D01T) %>%
  summarise(mean_SCIENCE = mean(SCIE)) %>%
  spread(ST004D01T, mean_SCIENCE) %>%
  mutate(diff_SCIENCE = Male - Female)


country_borders <- map_data("world") 
pisa_data_summ <- left_join(pisa_data_summ, country_borders, by = c("CNTRYID" = "region"))

unique(pisa_data_summ$CNTRYID)
unique(country_boundaries$region)

c <- ggplot(pisa_data_summ, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = diff_SCIENCE),color="black") +geom_path(color="black")+ 
  scale_fill_gradient2(name = expression("Gap between Average Science\nScores of Males and Females"), 
                       low = "darkgreen", mid = "goldenrod1", high = "darkred", 
                       midpoint = 0) + 
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(angle = 0, size = 8.5),
        legend.position = "right",  
        plot.title = element_text(size = 15),  
        legend.text = element_text(size = 10), 
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.5, "cm")) + 
  labs(title = "Gender Gap in PISA Science Scores by Country") +
  coord_cartesian(xlim = c(-20, 45),
                  ylim = c(37.5, 70))


c
############################################################################
##### Visualization 4: Color Map of Europe based on Reading Grades #########
############################################################################

read_data <- subset_noNAs %>%
  group_by(CNTRYID) %>%
  summarise(read_mean = mean(READ)) 
read_data <- left_join(read_data, country_borders, by = c("CNTRYID" = "region"))


d <- ggplot(read_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = read_mean), color = "black") +
  geom_path(color = "black") +
  scale_fill_gradient(name = expression("Grade on\nReading"),
                      low = "yellow1", high = "red") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(angle = 0, size = 8.5),
        legend.position = "right",  
        plot.title = element_text(size = 15),  
        legend.text = element_text(size = 10), 
        legend.key.height = unit(0.9, "cm"),
        legend.key.width = unit(0.5, "cm")) + 
  labs(title = "Mean Score on Reading by Country") +
  coord_cartesian(xlim = c(-20, 45),
                  ylim = c(37.5, 70))

d

##################################################
###### Visualization 5: Trellis density plot #####
##################################################
y_breaks <- c(0, 0.002, 0.005)
y_labels <- c("0","0.2%", "0.5%")
x_breaks <- c(250,450,600)
x_labels <- c("250", "450", "600")

e <- ggplot(subset_noNAs, aes(x = MATH)) +
  geom_density(fill = "red3", color = "black") +
  facet_wrap(~ CNTRYID, scales = "free") +
  labs(x = "Math Grade", y = "Density", 
       title = "Density Plots of Math Score by Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        strip.text = element_text(size = 8),  
        axis.ticks.x = element_blank(),  
        plot.margin = unit(c(1, 1, 2, 1), "lines")) +  
  scale_y_continuous(breaks = y_breaks, labels = y_labels)+
  scale_x_continuous(breaks = x_breaks, labels = x_labels)

e



################################################
library(foreign)
gdp_expenditure_df <- read.csv(file = "API_SE.XPD.TOTL.GD.ZS_DS2_en_csv_v2_277592.csv", 
                               header = FALSE, 
                               col.names = c("Source", "Country Code","Developement Indicators","Col1","1960","1961","1962","1964","1965","1966","1967","1968","1969","1970","1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","2025"))

expenditure_subset <- subset(gdp_expenditure_df, select = c(1,2,45,46,47,62))
expenditure_subset2 <- expenditure_subset[c(-(1:8),-10,-11,-15,-16,-20, -(22:24),-26,-27,-(30:32),-34,-(36:38),-40,-42,-45,-46,-47,-48,-50,-51,-(53:57),-61,-(64:73),-(76:78),-80,-(82:84),-(87:92),-(94:99),-(101:102),-104,-(106:109),c(-(111:114)),-116,-117,-121,c(-(125:129)),-131,-132,-133,c(-(135:146)),-151,-153,-155,-156,-157,-159,-160,-162,-164,-165,c(-(167:172)),c(-(174:179)),-182,-183,c(-(185:188)),-192,-193,-195,-196,-197,c(-(199:203)),-207,-208,-210,-211,c(-(213:217)),c(-(219:224)),c(-(228:236)),-238,-239,-(240:247),-(249:251),-253,-(256:264),-(266:269)),]


unique(subset_noNAs$CNTRYID)
length(unique(subset_noNAs$CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "B-S-J-Z (China)", "China", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Baku (Azerbaijan)", "Azerbaijan", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Moscow Region (RUS)", "Russia", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Tatarstan (RUS)", "Russia", CNTRYID))
subset_noNAs <- subset_noNAs %>% 
  mutate(CNTRYID = if_else(CNTRYID == "Chinese Taipei", "China", CNTRYID))


sum(is.na(expenditure_subset2$X2002))
sum(is.na(expenditure_subset2$X2018))


expenditure_subset2 <- expenditure_subset2 %>% 
  mutate(Source = if_else(Source == "Czechia", "Czech Republic", Source))
expenditure_subset2 <- expenditure_subset2 %>% 
  mutate(Source = if_else(Source == "Turkiye", "Turkey", Source))
expenditure_subset2 <- expenditure_subset2 %>% 
  mutate(Source = if_else(Source == "Hong Kong SAR, China", "Hong Kong", Source))

expenditure_subset2 <- expenditure_subset2 %>% 
  mutate(Source = if_else(Source == "Korea, Rep.", "Korea", Source))
expenditure_subset2 <- expenditure_subset2 %>% 
  mutate(Source = if_else(Source == "United States", "USA", Source))
expenditure_subset2 <- expenditure_subset2 %>% 
  mutate(Source = if_else(Source == "Macao SAR, China", "Macao", Source))
expenditure_subset2 <- expenditure_subset2 %>% 
  mutate(Source = if_else(Source == "Russian Federation", "Russia", Source))
expenditure_subset2 <- expenditure_subset2 %>% 
  mutate(Source = if_else(Source == "Korea, Rep.", "Korea", Source))
expenditure_subset2 <- expenditure_subset2 %>% 
  mutate(Source = if_else(Source == "Slovak Republic", "Slovakia", Source))

expenditure_subset2[67, "X2018"] <- 0 ## Saudi arabia
expenditure_subset2[78, "X2018"] <- 4.5 #kosovo
expenditure_subset2[10, "X2018"] <- 0 #bosnia
expenditure_subset2[13, "X2018"] <- 0 # brunei
expenditure_subset2[2, "X2018"] <- 0 ## UAE
expenditure_subset2[52, "X2018"] <- 3.70 ## North Macedonia
expenditure_subset2[54, "X2018"] <- 0  ## montenegro
expenditure_subset2[36, "X2018"] <- 2.3 #Ireland




which(expenditure_subset2$Source=="Montenegro")
sum(is.na(expenditure_subset2$X2018))
nrow(expenditure_subset2)

data_gdp_exp <- left_join(subset_noNAs, expenditure_subset2, by = c("CNTRYID" = "Source"))
data_gdp_exp <- data_gdp_exp[,c(-(19:21))]
sum(is.na(data_gdp_exp$X2018))
unique(data_gdp_exp$CNTRYID)
#################################################

#################################################################
################ Visualization 6: Bubble Plot ###################
#################################################################


bubble_plot_data <- aggregate(cbind(READ, MATH, X2018) ~ CNTRYID, data = data_gdp_exp, FUN = mean)
merged_bubble_data <- merge(bubble_plot_data, data_gdp_exp[, c("CNTRYID", "Continent")], by = "CNTRYID", all.y = TRUE)
merged_bubble_data <- distinct(merged_bubble_data)
merged_bubble_data$Continent <- as.factor(merged_bubble_data$Continent)
sum(is.na(merged_bubble_data$Continent))
mean_math <- mean(merged_bubble_data$MATH)
mean_reading <- mean(merged_bubble_data$READ)


f <- ggplot(merged_bubble_data, aes(x = READ, y = MATH, size = X2018, color = Continent)) +
  geom_point(alpha = 0.4) +  
  geom_text(data = merged_bubble_data[merged_bubble_data$CNTRYID == "Greece", ], aes(label = "GR", size=0.015), 
            vjust = -0.5, hjust = 0.8) + 
  scale_size(range = c(5, 10)) +  
  scale_color_manual(values = continent_colors) + geom_vline(xintercept = mean_reading, linetype = "dashed", color = "black") + 
  geom_hline(yintercept = mean_math, linetype = "dashed", color = "black") + annotate("text", x = mean_glcm, y = max(merged_bubble_data2$MATH), label = "Mean Read Score", color = "black", angle = 90, vjust = -5.5, hjust = 0.8) +
  annotate("text", x = max(merged_bubble_data2$GLCM), y = mean_math, label = "Mean Math Score", color = "black", hjust = 0.9, vjust = -0.7) +  
  labs(x = "Reading Score",
       y = "Math Score",
       size = "Expenditure on\nEducation\n(% of GDP)",
       color = "Continent",
       title = "Mean Scores in Math and Reading by Country\nvs. Expenditure on Education") +
  theme_minimal()

f



############################################################################
####### Visualization 7: Horizontal Bar Plot for Gender Gap in Math ########
############################################################################

merged_math_grade$gender_gap <- merged_math_grade$MATH_male - merged_math_grade$MATH_female
library(ggplot2)

g <- ggplot(merged_math_grade, aes(x = gender_gap, y = reorder(CNTRYID, gender_gap), fill = factor(gender_gap > 0))) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_bar(data = merged_math_grade[merged_math_grade$CNTRYID == "Greece", ],
           aes(x = gender_gap, y = reorder(CNTRYID, gender_gap)), 
           stat = "identity", 
           fill = "lightgrey", 
           alpha = 0.5, 
           width = 1.5) +
  scale_fill_manual(name = "Mean Grade Comparison", values = c("darkred", "darkgreen"),
                    labels = c("Higher Female Performance", "Higher Male Performance")) +
  labs(x = "Gender Gap in Mean Grade of MATH",
       y = "Country",
       title = "Gender Gap in Mean Math Scores by Country") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, hjust = 0),  
        axis.title.y = element_blank(),  
        axis.title.x = element_blank(),  
        panel.grid.major.x = element_line(color = "gray", size = 0.2), 
        panel.grid.major.y = element_blank(),  
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5))

g

#################################################################
###### Visualization 8: Bar Plot for Gender Gap in Reading ######
#################################################################

read_data_gap <- subset_noNAs %>%
  group_by(CNTRYID, ST004D01T) %>%
  summarise(mean_READ = mean(READ)) %>%
  spread(ST004D01T, mean_READ) %>%
  mutate(diff_READ = Male - Female)

mean_readgender_grade <- subset_noNAs %>%
  group_by(CNTRYID, ST004D01T) %>%
  summarise(mean_READ = mean(READ, na.rm = TRUE))

i <- ggplot(mean_readgender_grade, aes(x = ST004D01T, y = mean_READ, fill = ST004D01T)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ CNTRYID, scales = "free_y") +
  labs(x = "Gender", y = "Mean Score in Reading", title = "Mean Scores in Reading by Gender and Country") +
  scale_fill_manual(values = c("Male" = "darkgreen", "Female" = "darkred")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        strip.text = element_text(size = 8))+ 
  scale_y_continuous(breaks = c(300, 400,500),  
    labels = function(x) paste0(x, ""),  
        limits = c(0, 600))

i
##################################################################
######### Visualization 9: Box plots for Gender Gap ##############
##################################################################

filtered_data2 <- subset(subset_noNAs, CNTRYID %in% c("Greece", "France", "Germany", "Belgium","Italy", "Spain", "Albania", "Bulgaria","Turkey", "Finland", "United Kingdom", "USA" ,"Russia" , "China"))
par(mfrow = c(3, 5)) 
mar=c(3,3,3,3)

for (country in unique(filtered_data2$CNTRYID)) {
  country_data <- subset(filtered_data2, CNTRYID == country)
  boxplot(SCIE ~ ST004D01T, data = country_data,
          main = country, xlab = "Gender", ylab = "Science Score",
          col = c("darkgreen", "darkred"))
}

par(mfrow=c(1,1))
#################################################################
GLCM_NA_count <- subset_noNAs %>%
  group_by(CNTRYID) %>%
  summarise(NA_Count = sum(is.na(GLCM)))

GLCM_countries <- GLCM_NA_count %>%
  filter(NA_Count == 0)

bubble_plot_data2 <- aggregate(cbind(GLCM, MATH, X2018) ~ CNTRYID, data = data_gdp_exp, FUN = mean)
merged_bubble_data2 <- merge(bubble_plot_data2, data_gdp_exp[, c("CNTRYID", "Continent")], by = "CNTRYID", all.y = TRUE)
merged_bubble_data2 <- distinct(merged_bubble_data2)
merged_bubble_data2 <- na.omit(merged_bubble_data2)
mean_glcm <- mean(merged_bubble_data2$GLCM)
mean_math <- mean(merged_bubble_data$MATH)

k <- ggplot(merged_bubble_data2, aes(x = MATH, y = GLCM, size = X2018, color = Continent)) +
  geom_point(alpha = 0.55) +  
  geom_text(data = merged_bubble_data2[merged_bubble_data2$CNTRYID == "Greece", ], aes(label = "GR", size=0.10), 
            vjust = -0.5, hjust = 0.8) + 
  scale_size(range = c(4, 10)) +  
  scale_color_manual(values = continent_colors) + geom_vline(xintercept = mean_glcm, linetype = "dashed", color = "black") + 
  geom_hline(yintercept = mean_math, linetype = "dashed", color = "black") + 
  annotate("text", x = mean_glcm, y = max(merged_bubble_data2$MATH), label = "Mean Math Score", color = "black", angle = 90, vjust = -0.8, hjust = 0.8) +
  annotate("text", x = max(merged_bubble_data2$GLCM), y = mean_math, label = "Mean GLCM Score", color = "black", hjust = 0.9, vjust = -0.7)+
  labs(x = "Math Score",
       y = "Global Competence Score",
       size = "Expenditure on\nEducation\n(% of GDP)",
       color = "Continent",
       title = "Scores in Global Competence and Math by Country\nvs. Expenditure on Education") +
  theme_minimal()

k



#################################################################

png(filename = "gendergap_scatter_720px.png", type = "windows", width = 720,height = 720)
plot(merged_math_grade$MATH_male, merged_math_grade$MATH_female,
     xlab = "Mean Math Score for Males",
     ylab = "Mean Math Score for Females",
     main = "Mean Math Scores by Country and by Gender",
     pch = ifelse(merged_math_grade$CNTRYID == "Greece", 3, 20),  
     ylim = c(300,650), xlim = c(300,650),
     col = adjustcolor(continent_colors[merged_math_grade$Continent], alpha.f = 0.88))
abline(a = 0, b = 1, col = "black",lty=3)  # y = x line (gender equality)
legend("right", legend = unique_continents, fill = legend_colors)
text(
  greece_row$MATH_male, greece_row$MATH_female,
  labels = "GR", pos = 1, offset = -1)
text(320, 600, "Countries with higher female\n student performance", pos = 4, cex = 0.8) 
text(550, 340, "Countries with higher male\n student performance", pos = 4, cex = 0.8) 
dev.off()

png(filename = "trellisdotplot.png", type = "windows", width = 720,height = 720)
b
dev.off()

png(filename = "gendergap_map.png", type = "windows", width = 720,height = 720)
c
dev.off()

png(filename = "gendergap_map_1080px.png", type = "windows", width = 1080,height = 1080)
c
dev.off()

png(filename = "reading_countriesmap.png", type = "windows", width = 720,height = 720)
d
dev.off()

png(filename = "density_plot.png", type = "windows", width = 720,height = 720)
e
dev.off()

png(filename = "bubbleplot.png", type = "windows", width = 720,height = 720)
f
dev.off()

png(filename = "gendergap_barplot.png", type = "windows", width = 720,height = 720)
g
dev.off()

png(filename = "gendergap_barplot_reading.png", type = "windows", width = 720,height = 720)
i
dev.off()

png(filename = "gendergap_barplot_reading940px.png", type = "windows", width = 940,height = 940)
i
dev.off()

png(filename = "math_boxplots.png", type = "windows", width = 720,height = 720)
par(mfrow = c(4, 5)) 
for (country in unique(filtered_data2$CNTRYID)) {
  country_data <- subset(filtered_data2, CNTRYID == country)
  boxplot(SCIE ~ ST004D01T, data = country_data,
          main = country, xlab = "Gender", ylab = "Science Score",
          col = c("darkgreen", "darkred"))
}
dev.off()

png(filename = "glcm_bubbleplot.png", type = "windows", width = 720,height = 720)
k
dev.off()
