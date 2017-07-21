# CODE created by Claire Levy, Blair DeBuysscher; adapted by Christina Yacoob for Stamatatos Lab BLI use



# Load libraries: stringr, dplyr, ggplot2, reshape2, tidyr

library("stringr", lib.loc="~/R/win-library/3.3")
library("dplyr", lib.loc="~/R/win-library/3.3")
library("ggplot2", lib.loc="~/R/win-library/3.3")
library("reshape2", lib.loc="~/R/win-library/3.3")
library("tidyr", lib.loc="~/R/win-library/3.3")



# Set working directory to Home (My Documents or similar)

setwd("~/")



# import files

setwd("~/")
folder <- "Results_01"

files <- list.files(folder, pattern = "[A-H]\\d|//d//d.xls")
filesPaths<-paste(folder, "/", files, sep = "")
datList <- lapply(filesPaths, read.table, sep ="\t", skip = 4, fill = TRUE, header = TRUE)
names(datList)<- str_replace(files, pattern = ".xls", replacement = "")
dat <- bind_rows(datList, .id = "Sensor.Location")
dat<- dat %>% select(1:3)
names(dat)[2:3]<-c("Time","Shift")
View(dat)



# Importing Key / Legend
key<- read.csv("Results_01/kineticanalysistableresults.csv")
key <- key %>% select(Sensor.Location, Sample.ID, Loading.Sample.ID)



# Merge Key with compiled data file

keyAndDat<-merge(key, dat, by = "Sensor.Location", all.y=TRUE, all.x=TRUE)


# Check for NA and/or unmerged data

keyAndDat%>%
  filter(is.na(Loading.Sample.ID)) %>%
  unique()

keyAndDat <- keyAndDat %>%
  arrange(Sensor.Location, Time)


# Round time to .1 s

keyAndDat$Time <- round(keyAndDat$Time, digits = 1)

sum(is.na(keyAndDat$Time))
keyAndDat%>%
  filter(is.na(Time)) %>%
  unique()


# Zero time

keyAndDat <- keyAndDat %>%
  mutate(AdjTime = Time - min(Time))



# Export a compiled Excel file for Prism input

forPrism <- keyAndDat %>%
  select(AdjTime, Shift, Sample.ID, Loading.Sample.ID)%>%
  arrange(Loading.Sample.ID)

forPrism$Loading.Sample.ID <- as.character(forPrism$Loading.Sample.ID)
Loading_List <- split(forPrism, f = forPrism$Loading.Sample.ID)
names(Loading_List) <- unique(forPrism$Loading.Sample.ID)

spreadAll <- lapply(Loading_List, FUN = spread, key = Sample.ID, value = Shift)
dir.create("Results_01/prism files")
writeAll <- function(df){write.csv(df, file = paste0("Results_01/prism files/", unique(df$Loading.Sample.ID),".csv"), row.names = FALSE)}

lapply(spreadAll, FUN = writeAll)




# Plot by Sample ID (Ab) and Loading Sample ID (Env) and 
# Export graph files

maxX = max(keyAndDat$AdjTime)
plot_SampleID_Ab <- ggplot(keyAndDat, aes(x = AdjTime, y = Shift)) + geom_line(aes(color = Sample.ID), size = 0.5) + facet_wrap(~Loading.Sample.ID, scales = "free_y") + labs(x = "Time (s)", y = "Response (nm)") + scale_color_discrete(name = "Antibody") + scale_y_continuous("Response (nm)", limits = c(-0.2,2), breaks = c(0,0.5,1,1.5,2), expand = c(0,0)) + scale_x_continuous("Time (s)", limits = c(0,maxX), breaks = c(0, 400, 800, 1200), expand = c(0,0)) + theme(text = element_text(size = 8))

plot_SampleID_Ab

ggsave("plot_SampleID_Ab.tiff", plot = plot_SampleID_Ab, device = "tiff", width = 10, height = 5, path = "Results_01/prism files/") 

dev.off()



plot_LoadingSampleID_Env <- ggplot(keyAndDat, aes(x = AdjTime, y = Shift)) + geom_line(aes(color = Loading.Sample.ID), size = 0.5) + facet_wrap(~Sample.ID, scales = "free_y") + labs(x = "Time (s)", y = "Response (nm)") + scale_color_discrete(name = "Envelope") + scale_y_continuous("Response (nm)", limits = c(-0.2,2), breaks = c(0,0.5,1,1.5,2), expand = c(0,0)) + scale_x_continuous("Time (s)", limits = c(0,maxX), breaks = c(0, 250, 500, 750, 1000), expand = c(0,0)) + theme(text = element_text(size = 8))

plot_LoadingSampleID_Env

ggsave("plot_LoadingSampleID_Env.tiff", plot = plot_LoadingSampleID_Env, device = "tiff", width = 10, height = 5, path = "Results_01/prism files/") 

dev.off()


# Plot by Sample ID (Env) and Loading Sample ID (Ab) and 
# Export graph files


plot_SampleID_Env <- ggplot(keyAndDat, aes(x = AdjTime, y = Shift)) + geom_line(aes(color = Sample.ID), size = 0.5) + facet_wrap(~Loading.Sample.ID, scales = "free_y") + labs(x = "Time (s)", y = "Response (nm)") + scale_color_discrete(name = "Envelope") + scale_y_continuous("Response (nm)", limits = c(-0.2,2), breaks = c(0,0.5,1,1.5,2), expand = c(0,0)) + scale_x_continuous("Time (s)", limits = c(0,maxX), breaks = c(0, 400, 800, 1200), expand = c(0,0)) + theme(text = element_text(size = 8))

plot_SampleID_Env

ggsave("plot_SampleID_Env.tiff", plot = plot_SampleID_Env, device = "tiff", width = 10, height = 5, path = "Results_01/prism files/") 

dev.off()


plot_LoadingSampleID_Ab <- ggplot(keyAndDat, aes(x = AdjTime, y = Shift)) + geom_line(aes(color = Loading.Sample.ID), size = 0.5) + facet_wrap(~Sample.ID, scales = "free_y") + labs(x = "Time (s)", y = "Response (nm)") + scale_color_discrete(name = "Antibody") + scale_y_continuous("Response (nm)", limits = c(-0.2,2), breaks = c(0,0.5,1,1.5,2), expand = c(0,0)) + scale_x_continuous("Time (s)", limits = c(0,maxX), breaks = c(0, 250, 500, 750, 1000), expand = c(0,0)) + theme(text = element_text(size = 8))

plot_LoadingSampleID_Ab

ggsave("plot_LoadingSampleID_Ab.tiff", plot = plot_LoadingSampleID_Ab, device = "tiff", width = 10, height = 5, units = "in", path = "Results_01/prism files/") 

dev.off()
