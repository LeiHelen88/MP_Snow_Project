#A script to merge all MP files into one
#Prior from writing the code: make sure all of the files are converted to csv and saved on your desktop 

######script######

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

######################################## Figure 2 #############################################################
#Color, size, and morphology 
########Stacked bar plot for fibers, colors, size (length)########

#filter the dataset with only fibers/filament and making that into a new dataset
filtered_fiber <- filtered_data %>% 
  filter(morphology == "fiber/filament")

#custom colors for bars 
color_fiber <- c("gray20", "#1F78B4", "#A67B5B", "#27AE60", "#AF7AC5", "#D73027", "#80B1D3", "#F1E1C6")

fiber_size_order_list <- c("50-100",
                              "100-200",
                              "200-300",
                              "300-400",
                              "400-500", 
                              "500-600",
                              "600-700",
                              "700-800",
                              "800-900",
                              "1000-2000",
                              "2000-3000",
                              "3000-4000")


#Rearrange x axis from least to greatest
filtered_fiber$size_l <- factor(filtered_fiber$size_l, levels = fiber_size_order_list)


#creating a stacked barplot for only fibers/filament
ggplot(filtered_fiber, aes(fill=color, y=new_column, x=size_l)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "MP Fiber/Filament Characteristics",
       x= "Fiber/Filament Length",
       y= "Count")+ 
  scale_fill_manual(values= color_fiber) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))



########Stacked bar plot for fragment, colors, size########
#filter the dataset with only fibers/filament and making that into a new dataset
filtered_fragment <- filtered_data %>% 
  filter(morphology == "fragment")


#custom colors for bars 
color_fragment <-  c("gray20", "#A67B5B","#27AE60", "salmon", "#80B1D3", "#BEBCC5","#F1E1C6", "#FADA5E")

## Order for figures by size order
fragment_size_order_list <- c("<50", 
                          "50-100",
                          "100-200",
                          "200-300",
                          "300-400",
                          "400-500", 
                          "500-600",
                          "800-900")


#Rearrange x axis from least to greatest
filtered_fragment$size_l <- factor(filtered_fragment$size_l, levels = fragment_size_order_list)

#fill row 78 column 7 with the color "unknown" 
filtered_fragment[78, 7] <- "unknown"


#creating a stacked barplot for only fragments
ggplot(filtered_fragment, aes(fill=color, y=new_column, x=size_l)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "MP Fragment Characteristics",
       x= "Fragment Length",
       y= "Count")+ 
  #      col = c("gray", "black", "brown4")) +
  scale_fill_manual(values= color_fragment) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))



########filter the dataset with only film and making that into a new dataset########
filtered_film <- filtered_data %>% 
  filter(morphology == "film")


#custom colors for bars 
color_film <- c( "#A67B5B", "#BEBCC5" ,"#80B1D3", "#F1E1C6")


## Order for figures by stream order
film_size_order_list <- c("50-100",
                     "100-200",
                     "200-300",
                     "400-500")


#Rearrange x axis from least to greatest
filtered_film$size_l <- factor(filtered_film$size_l, levels = film_size_order_list)

#fill row 56 column 7 with the color "gray" 
filtered_film[56, 7] <- "gray"


#creating a stacked barplot for only films
ggplot(filtered_film, aes(fill=color, y=new_column, x=size_l)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "MP Film Characteristics",
       x= "Film Length",
       y= "Count")+ 
  scale_fill_manual(values= color_film) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

