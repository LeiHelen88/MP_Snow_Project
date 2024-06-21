# Load ggplot2
library(ggplot2)

install.packages("RColorBrewer")
library(RColorBrewer)


# Create data
data <- data.frame(
  Completed_in_Open_Specy=c("Analyzed","Unanalyzed") ,  
  Number_Completed=c(757,1203)
)


# Create the bar plot
ggplot(data, aes(x = Completed_in_Open_Specy, y = Number_Completed, fill = Completed_in_Open_Specy)) +
  geom_bar(stat = "identity", width=0.6) +   #changes width of the bars
  scale_fill_manual(values = c("Analyzed" = "purple3", "Unanalyzed" = "aquamarine3")) +
  labs(title = "Open Specy Progress",
       x = "Completed in Open Specy",
       y = "Amount Completed") +            #labeling axis 
  theme_minimal()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5)) #puts title in the center





