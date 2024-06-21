#A script to merge all MP files into one
#Prior from writing the code: make sure all of the files are converted to csv and saved on your desktop 

######script######

#Loading in libraries
install.packages("RColorBrewer")


library(dplyr, tidyr)

lapply(c("plyr","dplyr","ggplot2","cowplot", "tidyr",
         "lubridate","tidyverse", "reshape2", 
         "ggExtra", "viridis", "rstatix", "stringr", "RColorBrewer"), require, character.only=T) 

#reading in csv files- these files have the open specy data copied into the observation sheet 
BigMeadow <- read.csv("C:/Users/leihe/Desktop/Microplastics_R/files/BigMeadow_March182022_1_100_combined.csv")
CUES <- read.csv("C:/Users/leihe/Desktop/Microplastics_R/files/CUES_March242022_a_top_100_combined.csv")
MtRose <- read.csv("C:/Users/leihe/Desktop/Microplastics_R/files/MtRose_March162022_a_100_combined.csv")

#merging the files 
merged_df <- bind_rows(BigMeadow, CUES, MtRose)

#changing column 11 name with an underscore 
colnames(merged_df)[11] <- "other_observation" 

#using the merged files and sub-setting the columns 
subset_df <- merged_df[,c("particle", "spectra", "file_name", "size_w", "size_l", "morphology", "color",
                          "match_val", "good_correlations", "good_matches", "signal_to_noise",
                          "polymer_class", "plastic_or_not", "other_observation")]

#filter the dataset by matching if correlation is >=.7, signal_to_noise is >=4, and the match == "plastic"  
filtered_data <- subset_df %>%
  filter(match_val >= 0.7, signal_to_noise >=4, plastic_or_not == "plastic")

#add a column that separates the site name out of the "file_name" by extracting the first word using "strsplit"
filtered_data$SiteName <- sapply(strsplit(as.character(filtered_data$file_name), "_"), `[`, 1) #will create a column with only sitename



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


#################################Figure 3###########################################################################
#Creating a stacked barplot for polymer class

# Get a set of colors from RColorBrewer
colors <- brewer.pal(12, "Paired")  # Adjust the number (12) as needed

# Adding a new column with all 1s using the $ operator
filtered_data$new_column <- 1

#group data by location
#stacked bar without percent distribution
ggplot(filtered_data, aes(fill=polymer_class, y=new_column, x=SiteName)) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Polymer Classes per Site",
       x= "Site Name",
       y= "Percentage") +
  scale_fill_manual(values= colors) +
  theme_minimal()
#geom_bar(position="fill", stat="identity") <--- replace with this line for percent distribution 


# Other Plastic 
# OTHER PLASTIC 
# POLYAMIDES (POLYLACTAMS) 
# POLYCARBONATES 
# POLYDIGLYCIDYL ETHERS (POLYEPOXIDES, POLYHYDROXYETHERS, PHENOXY) 
# POLYESTERS 
# Polyethers (POLYGLYCOLS, Oxyalkylenes, GLYCIDYL ETHERS & CYCLIC ETHERS) 
# Polyolefins (POLYALKENES) 
# Polyterephthalates 
# Polyvinylesters 

######################################## Figure 2 #############################################################
#Color, size, and morphology 

#using plastic matches, create a figure that separates morphology 
#creating tables to show indicate how many of which morphology appears in the data set
morphology_counts <- table(filtered_data$morphology)
color_count <- table(filtered_data$color)
size_count <- table(filtered_data$size_l)

particles <- table(filtered_data$SiteName) 

print(morphology_counts)

# Creating bar plots for morphology, color, (look up size)
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


#Stacked bar plot for fibers, colors, size (length)


#filter the dataset with only fibers/filament and making that into a new dataset
filtered_fiber <- filtered_data %>% 
  filter(morphology == "fiber/filament")

#custom colors for bars 
color_fiber <- c("gray1", "peachpuff4", "lightblue3")

#creating a stacked barplot for only fibers/filament
ggplot(filtered_fiber, aes(fill=color, y=new_column, x=size_l)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "MP Fiber/Filament Characteristics",
       x= "Fiber/Filament Length",
       y= "Count")+ 
 #      col = c("gray", "black", "brown4")) +
  scale_fill_manual(values= color_fiber) +
  theme_minimal() 



#create a pie chart
pie(morphology_counts,
    labels = paste(names(morphology_counts), ": ", morphology_counts, sep = ""),
    main = "Distribution of Morphologies")

pie(color_count,
    labels = paste(names(color_count), ": ", color_count, sep = ""),
    main = "Distribution of Morphologies") 



