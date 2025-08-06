library(googlesheets4)
library(MASS)
library(tidyr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(ISLR)
library(caTools)
library(randomForest)
library(ggalluvial)
library(ggplot2)

#### conference type transfer
ggplot(transfer.type, aes(axis1 = Type_2023, axis2 = Type_2024)) +
  geom_alluvium(aes(fill = Type_2023)) +
  geom_stratum(width = 1/12) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  annotate("text", x = c(1.09, 1.09, 1.92, 1.92, 1.92, 1.92), y = c(925, 325, 1000, 800, 500, 225), 
           label = c("445","710","231", "296", "214", "414"), color = "black", size = 3) +
  scale_x_discrete(limits = c("2023", "2024")) +
  labs(title = "Movement Between School Type",
       subtitle = "From 2023 to 2024 FBS Schools",
       x = "",
       y = "") +
  theme_minimal() + 
  theme(legend.position = "none")

#### playing time graph


ggplot(data.frame(transfer.full), aes(transfer.full$snap_counts_2023)) +
  geom_histogram(binwidth = bin_width, color = "black", fill = "lightblue") +
  scale_x_continuous(breaks = seq(min(transfer.full$snap_counts_2023), max(transfer.full$snap_counts_2023), by = round(bin_width,0))) + 
  geom_text(stat = "bin", binwidth = bin_width, aes(label = ..count.., y = ..count..), vjust = -0.5) +
  labs(title = "Snap Counts by Transfers in 2023",
       subtitle = "Transfered prior to 2024",
       x = "Snap count Range",
       y = "Player Count") + 
  theme_classic()

# plot outgoing mountain west players
ggplot(mw.transfer, aes(x = snap_counts_2023, y = grades_2023,
                        color = Type_2024, shape = Type_2024)) + geom_point() + 
  geom_vline(xintercept = mean(transfer.full$snap_counts_2023), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 60, linetype = "dashed", color = "black") + 
  labs(title = "Outgoing Mountain West Players",
       subtitle = "Snap counts and Grades for 2023",
       x = "Snap counts",
       y = "Player Grade",
       color = "Destination Conference",
       shape = "Destination Conference") + 
   geom_text(aes(x = 800, y = 90, label = "Quality Starter"), size = 2, color = "black") + 
  geom_text(aes(x = 800, y = 45, label = "Over Played"), size = 2, color = "black") + 
  geom_text(aes(x = 30, y = 45,  label = "Reserve Player"), size = 2, color = "black") + 
  geom_text(aes(x = 30, y = 90,  label = "Under Played"), size = 2, color = "black") +
  theme(legend.position = "bottom") + 
  theme_classic()

ggplot(mw.remain, aes(x = snap_counts_2023, y = grades_2023, color = Poached, shape = Poached)) + 
  geom_point(size = 2.5) + 
  #stat_ellipse() +D95F02
  #coord_cartesian(xlim = c(0, NA)) +
 scale_color_manual(values = c("#D95F02","darkblue","#1B9E77")) + 
  geom_vline(xintercept = 400, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 60, linetype = "dashed", color = "black") +
  labs(title = "Evaluating all MW Players who Could Have Transferred",
       subtitle = "2023 Snaps and PFF Grades",
       x = "Total Snap Counts",
       y = "PFF Grade",
       color = "Transfer Status",
       shape = "Transfer Status")  + 
  scale_x_continuous(breaks = seq(0,1000, by = 100)) + 
  scale_y_continuous(breaks = seq(0, 100, by = 10)) + 
  theme_classic() + 
  theme(legend.position = "bottom")
  