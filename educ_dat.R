require(dplyr)

setwd("C:/Users/g38f293/Desktop/eags/data")

#NOTE: j2jod_mt_subset is made in "eag_data_clean.R"
j2j <- read.csv("j2jod_mt_subset.csv", header = TRUE, stringsAsFactors = FALSE)

#Inflows by Education  into Montana (2018Q3 - 2019Q2)
j2j_educ <- subset(j2j, j2j$agg_level == 197649 &
                      ((j2j$year == 2018 & j2j$quarter ==3)
                        |
                      (j2j$year == 2018 & j2j$quarter ==4)
                        |
                      (j2j$year == 2019 & j2j$quarter ==1)
                        |
                      (j2j$year == 2019 & j2j$quarter == 2))
                )

#drop people whos education status is unknown 
j2j_educ <- subset(j2j_educ, j2j_educ$education != "E5")

#make education labels
j2j_educ$educ <- ifelse(j2j_educ$education == "E1", "Less than High School",
                      ifelse(j2j_educ$education == "E2", "High school or equivalent, no college",
                              ifelse(j2j_educ$education == "E3", "Some college or Associate degree",
                                     ifelse(j2j_educ$education == "E4", "Bachelor's degree or higher", NA))))



#collapse by state to make total flows by state/educ
j2j_educ <- j2j_educ %>%
  group_by(geography_orig, educ) %>%
  summarise(totin = sum(totin),
            EE = sum(EE),
            AQHire = sum(AQHire))

#Make percentages by state for total inflows by educ
j2j_educ <- j2j_educ %>%
  group_by(educ) %>%
  mutate(totinA = sum(totin),
         pct_totin = (totin / sum(totin)) * 100,
         eeA = sum(EE),
         pct_ee = (EE / sum(EE)) * 100,
         aqhireA = sum(AQHire),
         pct_aqhire = (AQHire / sum(AQHire)) * 100
        )

write.csv(j2j_educ, file = "educ_dat.csv", row.names = FALSE)



#make map? 
require(usmap)
require(ggplot2)
require(RColorBrewer)

j2j_educ$fips <- j2j_educ$geography_orig
map <- as.data.frame(cbind(j2j_educ$fips, j2j_educ$pct_totin))
map <- map %>%
  rename(fips = V1,
         pct = V2)
map <- cbind(map, j2j_educ$educ)
map <- map %>% rename(educ = `j2j_educ$educ`)
map$pct <- round(map$pct, digits = 2)

#make groups for map.
map$pct_grp <- ifelse(map$pct < 1, "1",
                      ifelse(map$pct >= 1 & map$pct < 70, "2",
                             ifelse(map$pct >70, "3", NA)))

tiff("educ_map.tiff", width = 500, height =  400 )
plot_usmap(data = map,
           values = "pct_grp",
           labels = FALSE,
           theme = theme_void(),
           exclude = c("Alaska", "Arkansas", "Mississippi")) +
  scale_colour_manual(values = c("1" = "wheat1", "2" = "orange1", "3" = "red1"),
                      aesthetics = c("colour", "fill"),
                      breaks = c("1", "2", "3"),
                      labels = c("0.00-0.99%", "1.00-3.01%", "73.62-79.67%"),
                      na.value = "grey75"
  ) +
  theme(legend.position = "bottom") +
  labs(fill = "Percent of Total Inflows") +
  facet_wrap( ~ educ)
dev.off()



#Make a barchart for montana by educ
bc <- subset(j2j_educ, j2j_educ$geography_orig == 30)
bc$pct_totin <- round(bc$pct_totin, digits = 2)

tiff("educ_bar.tiff", width = 1000, height =  250 )
ggplot(data = bc, aes(x=reorder(as.factor(educ), pct_totin), y=pct_totin, fill=educ)) + geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_brewer(palette="Spectral") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label=paste(as.character(pct_totin),"%")), vjust=1, color="black",
            position = position_dodge(0.9), size=3.5) +
  ylab("Percent of Total Flows to Montana, Originating From Montana (by Education)") +
  xlab("")
dev.off()
  