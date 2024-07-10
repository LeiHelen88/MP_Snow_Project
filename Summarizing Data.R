#A script to merge all MP files into one and generally looking at the data
#Prior from writing the code: make sure all of the files are converted to csv and saved on your desktop 


#Loading in libraries
lapply(c("RColorBrewer", "plyr","dplyr","ggplot2","cowplot", "tidyr",
         "lubridate","tidyverse", "reshape2", 
         "ggExtra", "viridis", "rstatix", "stringr", "RColorBrewer"), require, character.only=T) 

#reading in csv files- these files have the open specy data copied into the observation sheet 
BigMeadow <- read.csv("C:/Users/leihe/Desktop/MP Zip Files/BigMeadow_March182022_1_100.csv")
CSSL <- read.csv("C:/Users/leihe/Downloads/Observations_Snow_FTIR(CSSL_March142022_a_top_100).csv")
CUES <- read.csv("C:/Users/leihe/Desktop/MP Zip Files/CUES_March242022_a_top_100.csv")
DiamondPeak <- read.csv("C:/Users/leihe/Desktop/MP Zip Files/DiamondPeak_March162022_top_100.csv")
MeeksBay <- read.csv("C:/Users/leihe/Desktop/MP Zip Files/MeeksBay_March232022_1_100.csv")
MtRose <- read.csv("C:/Users/leihe/Desktop/MP Zip Files/MtRose_March162022_a_100.csv")
PaigeMeadow <- read.csv("C:/Users/leihe/Desktop/MP Zip Files/PaigeMeadow_March162022_top_100.csv")
Sagehen2 <- read.csv("C:/Users/leihe/Desktop/MP Zip Files/Sagehen_March142022_2_top_100.csv") 


#Scaling up DiamondPeak (106x5=530 particles), MeeksBay (124X5=620 particles), and PaigeMeadow (101x5=505 particles)
DiamondPeak <- do.call(rbind, replicate(5, DiamondPeak, simplify = FALSE))
MeeksBay <- do.call(rbind, replicate(5, MeeksBay, simplify = FALSE))
PaigeMeadow <- do.call(rbind, replicate(5, PaigeMeadow, simplify = FALSE))


#merging the files 
merged_df <- bind_rows(BigMeadow, CSSL, CUES, DiamondPeak, MtRose, MeeksBay, PaigeMeadow, Sagehen2)

view(merged_df)

#using the merged files and sub-setting the columns 
subset_df <- merged_df[,c("particle", "spectra", "file_name", "size_w", "size_l", "morphology", "color",
                          "match_val", "good_correlations", "good_matches", "signal_to_noise",
                          "material_class", "plastic_or_not", "other_observation")]

view(subset_df)

#filter the dataset by matching if correlation is >=.7, signal_to_noise is >=4, and the match == "plastic"  
filtered_data <- subset_df %>%
  filter(match_val >= 0.7, signal_to_noise >=4, plastic_or_not == "plastic")

#add a column that separates the site name out of the "file_name" by extracting the first word using "strsplit"
filtered_data$SiteName <- sapply(strsplit(as.character(filtered_data$file_name), "_"), `[`, 1) #will create a column with only sitename


#Adding a new column with all 1s
filtered_data$new_column <- 1

#Seeing how many plastic particles are in each site
particles <- table(filtered_data$SiteName) 
print(particles)


#######################################Looking broadly###################################################################

#creating a bar chat to show MPs count per site 
particles <- table(filtered_data$SiteName) 

print(particles)

barplot(particles,
        main = "MPs Count Per Site",
        xlab = "Site",
        ylab = "Count",
        col = c("lightblue2", "gray", "purple3"))

#Creating a pie charts to show percentage difference of plastics vs nonplastics between sites 
MtRose_Site <- data.frame(
  Category = c("Plastic", "Not Plastic"),
  Count = c(22, 248))

pie(MtRose_Site$Count, labels = MtRose_Site$Category, main = "MtRose") #change out per site


#using plastic matches, create a figure that separates morphology 
#creating tables to show indicate how many of which morphology appears in the data set
morphology_counts <- table(filtered_data$morphology)
color_count <- table(filtered_data$color)
size_count <- table(filtered_data$size_l)

particles <- table(filtered_data$SiteName) 
print(morphology_counts)

##create a pie chart##
pie(morphology_counts,
    labels = paste(names(morphology_counts), ": ", morphology_counts, sep = ""),
    main = "Distribution of Morphologies")

pie(color_count,
    labels = paste(names(color_count), ": ", color_count, sep = ""),
    main = "Distribution of Morphologies") 

##Creating bar plots for morphology, color, (look up size)##
barplot(morphology_counts,
        main = "Morphology Count",
        xlab = "Morphology",
        ylab = "Count",
        col = c("lightblue2", "gray", "purple3"))

barplot(color_count,
        main = "Color Count",
        xlab = "Color",
        ylab = "Count",
        col = c("gray", "black", "brown4", "lightblue2", "white"),
        cex.names = 0.8)





