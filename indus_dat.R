require(dplyr)

setwd("C:/Users/g38f293/Desktop/eags/data")

#NOTE: j2jod_mt_subset is made in "eag_data_clean.R"
j2j <- read.csv("j2jod_mt_subset.csv", header = TRUE, stringsAsFactors = FALSE)

# Inflows by Industry  into Montana (2018Q3 - 2019Q2)
j2j_indus <- subset(j2j, j2j$agg_level == 197889 & 
                      ((j2j$year == 2018 & j2j$quarter ==3)
                       |
                         (j2j$year == 2018 & j2j$quarter ==4)
                       |
                         (j2j$year == 2019 & j2j$quarter ==1)
                       |
                         (j2j$year == 2019 & j2j$quarter == 2))
                )

#Make EAG industry Groups.
j2j_indus <- j2j_indus %>%
  
  mutate(dli_cat  =  ifelse(industry == "11", "Agriculture",
                            
                            ifelse(industry == "54" | industry == "55" | industry == "56", "Business Services",
                                   
                                   ifelse(industry == "23", "Construction",
                                          
                                          ifelse(industry == "52" | industry == "53", "Financial Activities",
                                                 
                                                 ifelse(industry == "92", "Government",
                                                        
                                                        ifelse(industry == "61" | industry == "62", "Healthcare & Education",
                                                               
                                                               ifelse(industry == "71" | industry == "72", "Leisure Activities",
                                                                      
                                                                      ifelse(industry == "31-33", "Manufacturing",
                                                                             
                                                                             ifelse(industry == "21", "Mining",
                                                                                    
                                                                                    ifelse(industry == "51" | industry == "81", "Other Services",
                                                                                           
                                                                                           ifelse(industry == "42" | industry == "44-45", "Trade",
                                                                                                  
                                                                                                  ifelse(industry == "22" | industry == "48-49", "Transportation & Utilities",
                                                                                                         
                                                                                                         ""
                                                                                                         
                                                                                                  )))))))))))))


#collapse by state to make total flows by state/industry
j2j_indus <- j2j_indus %>%
  group_by(geography_orig, dli_cat) %>%
  summarise(totin = sum(totin),
            EE = sum(EE),
            AQHire = sum(AQHire))

#Make percentages by state for total inflows
j2j_indus <- j2j_indus %>%
  group_by(dli_cat) %>%
  mutate(totinA = sum(totin),
         pct_totin = (totin / sum(totin)) * 100,
         eeA = sum(EE),
         pct_ee = (EE / sum(EE)) * 100,
         aqhireA = sum(AQHire),
         pct_aqhire = (AQHire / sum(AQHire)) * 100
        )

write.csv(j2j_indus, file = "indus_dat.csv", row.names = FALSE)

#Collapse flows by industry to see total flows 
total_inflows_by_indus <- j2j_indus %>% 
  group_by(dli_cat) %>%
  summarize(total_flows = sum(totin),
            total_flows_ee = sum(EE),
            total_flows_aq = sum(AQHire))
write.csv(total_inflows_by_indus, file = "indus_dat_total_inflows.csv", row.names = FALSE)
  



# make an overall map for all industries and states.  
require(usmap)
require(ggplot2)
require(RColorBrewer)

j2j_indus$fips <- j2j_indus$geography_orig
map <- as.data.frame(cbind(j2j_indus$fips, j2j_indus$pct_totin))
map <- map %>%
  rename(fips = V1,
         pct = V2)
map <- cbind(map, j2j_indus$dli_cat)
map <- map %>% rename(dli_cat = `j2j_indus$dli_cat`)
map$pct <- round(map$pct, digits = 2)

#make groups for map.
map$pct_grp <- ifelse(map$pct < 1, "1",
                      ifelse(map$pct > 1 & map$pct < 10, "2",
                             ifelse(map$pct >= 10 & map$pct < 50, "3",
                                    ifelse(map$pct >= 50 & map$pct < 70, "4",
                             ifelse(map$pct >= 70, "5", NA)))))

map <- subset(map, !is.na(map$dli_cat))

png("indus_map.png", width = 792, height =  562 )
plot_usmap(data = map,
           values = "pct_grp",
           labels = FALSE,
           theme = theme_void(),
           exclude = c("Alaska", "Arkansas", "Mississippi")) +
  scale_colour_manual(values = c("1" = "wheat1", "2" = "orange1", "3" = "gold1", "4" = "coral2", "5" = "red1"),
                      aesthetics = c("colour", "fill"),
                      breaks = c("1", "2", "3", "4", "5"),
                      labels = c("0.00-0.99%", "1.00-5.33%", "12.18%", "56.39%", "70.36-86.71%")
                      
  ) +
  theme(legend.position = "bottom") +
  labs(fill = "Percent of Total Inflows") +
  facet_wrap( ~ dli_cat)
dev.off()


#Make a barchart for montana by industry
bc <- subset(j2j_indus, j2j_indus$geography_orig == 30)
bc$pct_totin <- round(bc$pct_totin, digits = 2)

png("indus_bar.png", width = 792, height =  562 )
ggplot(data = bc, aes(x=reorder(as.factor(dli_cat), pct_totin), y=pct_totin, fill=dli_cat)) + geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_brewer(palette="Set3") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label=paste(as.character(pct_totin),"%")), vjust=1, color="black",
            position = position_dodge(0.9), size=3.5) +
  ylab("Percent of Total Flows to Montana, Originating From Montana (by Industry)") +
  xlab("")
dev.off()




#Make a barchart for montana by industry and then for the next 5 largest statess
#find 5 largest states besides montana. 
  bc2 <- j2j_indus %>% 
    group_by(dli_cat) %>%
    mutate(catvar = order(order(pct_totin, decreasing = TRUE)))
  
  bc2 <- subset(bc2, bc2$catvar <= 6)
  bc2$MT <- ifelse(bc2$geography_orig == 30, "1", "0")
  bc2$pct_totin <- round(bc2$pct_totin, digits = 0)
  
  #get state names
  require(cdlTools)
  bc2$abrv <- cdlTools::fips(bc2$geography_orig, to = "Abbreviation")
  
  bc2 <- arrange(bc2, dli_cat, pct_totin)
  bc2$order <- rownames(bc2)

tiff("indus_bar2.tiff", width = 1000, height =  565)
ggplot(data = bc2, aes(x=reorder(as.factor(dli_cat), pct_totin), y=pct_totin, fill= reorder(as.factor(abrv), desc(pct_totin)))) + 
  geom_bar(stat="identity", position="stack", colour = "black") +
  scale_fill_brewer(palette="Set3") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ylab("Percent of Total Flows to Montana, Originating From Montana and Next 5 Top States (by Industry)") +
  xlab("") +
  coord_flip() + 
  geom_text(aes(label=paste(as.character(pct_totin), sep="")), color="black",
            position = position_stack(vjust = 0.5), size=3) +
 labs(fill = "State")
dev.off()

