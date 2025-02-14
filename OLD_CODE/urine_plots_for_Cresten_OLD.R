# plots data trends from pharmaceutical database
install.packages('tidyverse')
library("tidyverse")

# load data 
urine_percent = read.csv("urine_percent.csv")

# reorder data highest urine % to lowest urine % for bar graph
urine_percent$Pharmaceutical = reorder(urine_percent$Pharmaceutical,
                                       -urine_percent$urine_percent)

# select top 20 drugs for better plotting (all drugs can be plotted, but the 
# graph gets super busy)
top_20 = urine_percent[1:20,]

# plot urine composition for top 20 pharmaceuticals
ggplot(top_20, aes(x = Pharmaceutical, y = urine_percent)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Pharmaceutical Composition of Urine",
       x = "Pharmaceuticals",
       y = "% of urine") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot top 20 as pie chart
ggplot(top_20, aes(x = "", y = urine_percent, fill = Pharmaceutical)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Pharmaceutical Composition of Urine", fill = "Pharmaceuticals")