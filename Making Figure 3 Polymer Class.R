#A script to merge all MP files into one
#Prior from writing the code: make sure all of the files are converted to csv and saved on your desktop 

######script######

#Loading in libraries
install.packages("RColorBrewer")

lapply(c("plyr","dplyr","ggplot2","cowplot", "tidyr",
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

#merging the files 
merged_df <- bind_rows(BigMeadow, CSSL, CUES, DiamondPeak, MeeksBay, MtRose, PaigeMeadow, Sagehen2)

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

# Adding a new column with all 1s using the $ operator
filtered_data$new_column <- 1


#################################Figure 3###########################################################################
#Creating a stacked barplot for polymer class
# Get a set of colors from RColorBrewer
#colors <- brewer.pal(12, "Paired")  # Adjust the number (12) as needed

#colorblind friendly palette (need to double check to confirm)
color_palette <- c("#ABD9E9","#6BAED6", "#78C679","#4DAF4A","#FBB4AE", "#CA0020",
                   "#FDB863", "#FD8D3C",  "#C2A5CF","#5E3C99","#F0E442")
                   





filtered_data$material_class <- revalue(as.character(filtered_data$material_class), replace = c("polystyrenes (polyphenylethylenes, -methylstyrene)"="Polystyrenes",
                                                                               "other plastic"="Other Plastic",
                                                                               "polyacrylamides"="Polyacrylamides",
                                                                               "polydiglycidyl ethers (polyepoxides, polyhydroxyethers, phenoxy)"="Polydiglycidyl Ethers",
                                                                               "polyesters"="Polyesters",
                                                                               "polyhaloolefins (vinylhalides)"="Polyhaloolefins",
                                                                               "polyolefins (polyalkenes)" = "Polyolefins",
                                                                               "polyterephthalates" = "Polyterephthalates",
                                                                               "polyurethanes (isocyanates)"= "Polyurethanes",
                                                                               "polyvinylesters" = "Polyvinylesters",
                                                                               "polyamides (polylactams)" = "Polyamides")) 
            
#group data by location
#stacked bar without percent distribution
ggplot(filtered_data, aes(fill=material_class, y=new_column, x=SiteName)) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Polymer Classes per Site",
       x= "Site Name",
       y= "Percentage") +
  scale_fill_manual(values= color_palette, name= "Polymer Class") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 70, hjust=1))+
  theme(plot.title = element_text(hjust = 0.5))
