library(MASS)
library(tidyr)
library(dplyr)
library(rpart)
library(ggplot2)
library(ISLR)
library(caTools)



defense_cols <- c("year","player","player_id","position","team_name",
                  "snap_counts_defense","grades_defense")

offense_cols <- c("Year","player","player_id","position","team_name",
                  "snap_counts_offense","grades_offense")

full_names <- c("year","player","player_id","position","team_name",
                "snap_counts","grades")
# Simplify and rename defense, offense
defense.df <- defense[, defense_cols]
colnames(defense.df) <- full_names

offense.df <- offense[, offense_cols]
colnames(offense.df) <- full_names
# Merge defense and offense
df <- rbind(offense.df, defense.df)

df <- left_join(df, mapping, by = "team_name")

df <-  df %>%
  group_by(player_id, year) %>%
  slice_max(snap_counts, with_ties = FALSE) %>%
  ungroup()

# Find the students who transferred by seeing who was in different schools each year

transfer <- df %>%
  filter(year %in% c(2023, 2024)) %>%
  group_by(player_id) %>%
  filter(n() == 2 && n_distinct(team_name) > 1) %>%
  ungroup()

# Total transfers?
length(unique(transfer$player_id))
# 1,157

# Compare Movement by conference and school type
transfer.type <- transfer %>%
  select(player_id, year, Conference, Type) %>%
  pivot_wider(
    names_from = year,
    values_from = c(Conference, Type)
  )

type.table <- as.data.frame(table(transfer.type$Type_2023, transfer.type$Type_2024))
colnames(type.table) <- c("SY23","SY24","Count")

# Compare snap counts and grade changes
transfer.improve <- transfer %>%
  select(player_id, year, snap_counts, grades) %>%
  pivot_wider(
    names_from = year,
    values_from = c(snap_counts, grades)
  )

transfer.full <- left_join(transfer.improve, transfer.type, by = "player_id")

# Now focus on MW players
mw.transfer <- transfer.full[transfer.full$Conference_2023 == "MWC" & !is.na(transfer.full$Conference_2023),]

mean_snaps_23 <- mean(transfer.full$snap_counts_2023) 
mw.transfer$Quadrant <- ifelse(mw.transfer$snap_counts_2023 > mean_snaps_23 & (mw.transfer$grades_2023 >= 60), "Q1",
                        ifelse(mw.transfer$snap_counts_2023 > mean_snaps_23 & (mw.transfer$grades_2023 < 60), "Q2",
                        ifelse(mw.transfer$snap_counts_2023 < mean_snaps_23 & (mw.transfer$grades_2023 < 60), "Q3","Q4")))

as.data.frame(table(mw.transfer$Quadrant))

aggregate(cbind(snap_counts_2023, grades_2023) ~ Type_2024, data = mw.transfer, FUN = mean)

# MW players for 2023 to see how many ended up in P4 schools
remain <- df[ave(df$player_id, df$player_id, FUN = length) == 2,]

remain.wide <- pivot_wider(remain, names_from = year, values_from =  
                             c("position","team_name","snap_counts","grades","Conference","Type"))

mw.remain <- remain.wide[remain.wide$Conference_2023 == "MWC" & !(is.na(remain.wide$Conference_2023)),]

mw.remain$QualityPlayer <- ifelse(mw.remain$snap_counts_2023 > 400 & mw.remain$grades_2023 >= 60, "Q","")
mw.remain$Poached <- ifelse(mw.remain$Type_2024 == "P4", "Left for P4",
                     ifelse(mw.remain$team_name_2023 != mw.remain$team_name_2024, "Left for Other G5",
                     "Stayed in MW"))

as.data.frame(table(mw.remain$QualityPlayer, mw.remain$Poached))
aggregate(cbind(snap_counts_2023, grades_2023) ~ QualityPlayer + Poached, data = mw.remain, FUN = mean)
