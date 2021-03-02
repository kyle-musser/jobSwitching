require(dplyr)

setwd("C:/Users/kylem/Google Drive/EAG_J2J/data")

#NOTE: j2jod_mt_subset is made in "eag_data_clean.R"
j2j <- read.csv("j2jod_mt_subset.csv", header = TRUE, stringsAsFactors = FALSE)

#Overall Percent of all job-to-job flows into Montana (2018Q3 - 2019Q2)
j2j_overall <- subset(j2j, j2j$agg_level == 197633 & 
                      ((j2j$year == 2018 & j2j$quarter ==3)
                        |
                      (j2j$year == 2018 & j2j$quarter ==4)
                        |
                      (j2j$year == 2019 & j2j$quarter ==1)
                        |
                      (j2j$year == 2019 & j2j$quarter == 2))
                )

#collapse by state to make total flows by state
j2j_overall <- j2j_overall %>%
  group_by(geography_orig) %>%
  summarise(totin = sum(totin),
            EE = sum(EE),
            AQHire = sum(AQHire))

#Make percentages by state for total inflows
j2j_overall <- j2j_overall %>%
  mutate(totinA = sum(totin),
         pct_totin = (totin / sum(totin)) * 100,
         eeA = sum(EE),
         pct_ee = (EE / sum(EE)) * 100,
         aqhireA = sum(AQHire),
         pct_aqhire = (AQHire / sum(AQHire)) * 100
        )

require(cdlTools)
j2j_overall$abrv <- fips(j2j_overall$geography_orig, to = "Abbreviation")

write.csv(j2j_overall, file = "overall_dat.csv", row.names = FALSE)

#make map? 
require(usmap)
require(ggplot2)
require(RColorBrewer)

j2j_overall$fips <- j2j_overall$geography_orig
map <- as.data.frame(cbind(j2j_overall$fips, j2j_overall$pct_totin))
map <- map %>%
  rename(fips = V1,
         pct = V2)
map$pct <- round(map$pct, digits = 2)

#make groups for map.
map$pct_grp <- ifelse(map$pct < 1, "1",
                      ifelse(map$pct >= 1 & map$pct < max(map$pct), "2",
                          ifelse(map$pct == max(map$pct), "3", NA)))

tiff("overall_map.tiff", width = 500, height =  400 )
plot_usmap(data = map,
           values = "pct_grp",
           labels = FALSE,
           theme = theme_void()) +
  scale_colour_manual(values = c("1" = "wheat1", "2" = "orange1", "3" = "red1"),
                      aesthetics = c("colour", "fill"),
                      breaks = c("1", "2", "3"),
                      labels = c("0.00-0.99%", "1.00-2.05%", paste(as.character(max(map$pct)), "%", sep = "")),
                      na.value = "grey75"
                      ) +
  theme(legend.position = "bottom") +
  labs(fill = "Percent of Total Inflows")
dev.off()


  