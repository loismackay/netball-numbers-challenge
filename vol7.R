# Load packages 
library(tidyverse)
library(devtools)
library(dplyr)
library(ggplot2)

library(readr)
library(here)

library(png)
library(cowplot)
library(patchwork)

library(ggridges)

library(grid)


TeamColours <- c("#fed22b", "#73D055FF", "#27AD81FF", 
                 "#7E4E90FF", "#CC6A70FF", "#2D708EFF", "#C0C0C0", "#FFA500") 
                 

names(TeamColours) <- c("Lightning", "Fever", "Vixens", 
                        "Firebirds", "Thunderbirds", "Swifts", "Magpies", "GIANTS")

# Read in logos

ssn_logo <- readPNG("logo_ssn.png")
anz_logo <- readPNG("logo_anz_champ.png")
super_shot <- readPNG("super_shot.png")

# Read in vol 7 data

netball_numbers_7 <- read_csv(here("datasets", "vol7", "inTheZone.csv"))

#add column to data set to count the number of misses & count the goals (not the points)
netball_numbers_7 <- netball_numbers_7 %>% 
  mutate(missedShots = ifelse(scoreValue == 0, "1", "0")) %>% 
  mutate(goal = case_when(
    shotResult == "unsuccessful" ~ "0", 
    shotResult == "successful" ~ "1")) 


#convert missed shots & goals to a numeric 
netball_numbers_7$missedShots <- as.numeric(netball_numbers_7$missedShots)
netball_numbers_7$goal <- as.numeric(netball_numbers_7$goal)

glimpse(netball_numbers_7)
      


####find which teams are most successful at what shots? Per year? -----------------


#sum total number of shots and misses for each team, each year, at each shot location
team_year_loc <- netball_numbers_7 %>% 
  group_by(year, squadName, shotLocation) %>% 
  summarise(totalGoals = sum(goal), 
            totalMiss = sum(missedShots)) %>% 
  mutate(totalShots = totalGoals + totalMiss) %>% #add column to sum goals & misses for % accuracy
  mutate(shotPer = (totalGoals/totalShots)*100) %>% #add column to add % accuracy 
  mutate(across(starts_with("shotPer"), round, 1))


#create dataset with only 'long' shots
team_year_loc_long <- 
  team_year_loc[grep("long", team_year_loc$shotLocation),] 

#sum all angles of shots (for long only)
team_year_loc_long <- team_year_loc_long %>% 
  group_by(year, squadName) %>% 
  summarise(totalGoalsL = sum(totalGoals), 
            totalMissL = sum(totalMiss), 
            totalShotsL = sum(totalShots))
  
#create dataset with only 'short' shots
team_year_loc_short <- 
  team_year_loc[grep("short", team_year_loc$shotLocation),] 

#sum all angles of shots (for short only)
team_year_loc_short <- team_year_loc_short %>% 
  group_by(year, squadName) %>% 
  summarise(totalGoalsS = sum(totalGoals), 
            totalMissS = sum(totalMiss), 
            totalShotsS = sum(totalShots))  
  

#combine the short and long datasets

short_long <- merge(team_year_loc_long, team_year_loc_short, by = c("year", "squadName"))

#add % accuracy 

short_long <- short_long %>% 
  mutate(totalShotsL = totalGoalsL + totalMissL) %>% 
  mutate(shotPerL = (totalGoalsL/totalShotsL)*100) %>% 
  mutate(totalShotsS = totalGoalsS + totalMissS) %>% 
  mutate(shotPerS = (totalGoalsS/totalShotsS)*100) %>% 
  mutate(across(starts_with("shotPer"), round, 1))



###calculate SSN AVERAGE, per year (not per team)

#ssn AVERAGE LONG
ssn_long <- team_year_loc_long %>% 
  group_by(year) %>% 
  summarise(totalGoalsL = sum(totalGoalsL), 
            totalMissL = sum(totalMissL), 
            totalShotsL = sum(totalShotsL))


#SSN AVERAGE SHORT 
ssn_short <- team_year_loc_short %>% 
  group_by(year) %>% 
  summarise(totalGoalsS = sum(totalGoalsS), 
            totalMissS = sum(totalMissS), 
            totalShotsS = sum(totalShotsS))  


#combine the SSN short and long datasets

ssn_short_long <- merge(ssn_long, ssn_short, by = c("year"))

#add % accuracy for SSN AVERAGE

ssn_short_long <- ssn_short_long %>% 
  mutate(totalShotsL = totalGoalsL + totalMissL) %>% 
  mutate(shotPerL = (totalGoalsL/totalShotsL)*100) %>% 
  mutate(totalShotsS = totalGoalsS + totalMissS) %>% 
  mutate(shotPerS = (totalGoalsS/totalShotsS)*100) %>% 
  mutate(across(starts_with("shotPer"), round, 1))


####PLOTS ------------------------

#plot long shots data over time
ggplot() +
  geom_path(data = short_long, aes(x = year, y = shotPerL, colour = squadName), size = 1) + 
  geom_point(data = short_long, aes(x = year, y = shotPerL, colour = squadName), size = 10) + 
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) +
  scale_y_continuous(limits = c(20, 100),breaks = seq(20, 100, by = 10))

  


#plot short shots data over time
ggplot() +
  geom_bump(data = short_long, aes(x = year, y = shotPerS, colour = squadName), size = 1) + 
  geom_point(data = short_long, aes(x = year, y = shotPerS, colour = squadName), size = 10) + 
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) +
  scale_y_continuous(limits = c(80, 100),breaks = seq(80, 100, by = 10))




#SSN AVERAGE + individual teams (LONG)
library(showtext)
font_add_google("Bayon", family = "CR")
showtext_auto()

ggplot() + 
  geom_point(data = short_long, aes(x = year, y = shotPerL, fill = squadName),
             shape = 23, size = ((short_long$totalShotsL)/25)) + 
  geom_point(data = ssn_short_long, aes(x = year, y = shotPerL), 
             colour = "black", fill = "white", size = 15, shape = 21, alpha = 0.75) + 
  geom_text(data = ssn_short_long, aes(x = year, y = shotPerL, label = shotPerL), size = 3.75, fontface = "bold", colour = "black", check_overlap = TRUE) + 
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) +
  scale_y_continuous(limits = c(20, 100),breaks = seq(20, 100, by = 10)) + 
  geom_vline(aes(xintercept = 2019.5), colour = "#ed215d", linetype = "dashed", size = .8, alpha = .7) +
  coord_cartesian(clip = "off") +
  
  labs(x = "Season", 
       y = "Average Goals Scored Per Game", 
       title = "Average Goals Scored Per Game.", 
       subtitle = "Circles represent average across all teams, per season.\nDiamonds represent average for each team, per season.", 
       caption = "data: @aaron_s_fox | by @loismackay") +
  
  
  theme(plot.background = element_rect(fill = "#1c1c1c"), panel.background = element_rect(fill = "#1c1c1c"), 
        plot.margin = margin(0.5,0.5,0.5,1, "cm"),
        legend.background = element_rect(fill = "#1c1c1c"), legend.key = element_rect(fill = "#1c1c1c"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20, colour = "white", face = "bold", family = "CR", margin = margin(15,0,0,0)),
        axis.text.y = element_text(size = 20, colour = "white", family = "CR"),
        axis.text.x = element_text(size = 14, colour = "white", family = "CR"), 
        axis.line.x = element_line(colour = "grey80", size = 0.3),
        axis.line.y = element_line(colour = "grey80", size = 0.3),
        plot.title = element_text(size = 28, colour = "white", hjust = 0.12, family = "CR"), 
        plot.subtitle = element_text(size = 18, colour = "white", hjust = 0.099, family = "CR",face = "italic", margin = margin(5,0,25,0)),
        plot.caption = element_text(colour = "grey80", size = 14, vjust = -0.75, hjust = 1, family = "CR"), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 12, family = "CR", colour = "grey90"), legend.margin = margin(0, 0, 0, 1, "cm"), 
        legend.position = "right", legend.direction = "vertical") 




geom_point(data = netball_numbers_1, aes(x = year, y = av_GF, colour = team, fill = team), size= 3.75, shape = 23) + 
  geom_point(data = goals_per_game, aes(x = year, y = mean_goals), colour = "black", fill = "white", size = 11.5, shape = 21, alpha = 0.75) + 
  
 
  
                    


#SSN AVERAGE + individual teams (SHORT)
  
font_add_google("Bayon", family = "CR")
showtext_auto()

ggplot() + 
  geom_point(data = ssn_short_long, aes(x = year, y = shotPerS), colour = "white", size = 15) + 
  geom_point(data = short_long, aes(x = year, y = shotPerS, fill = squadName), shape = 23, size = ((short_long$totalShotsS)/100)) +
  geom_text(data = ssn_short_long, aes(x = year, y = shotPerS, label = shotPerS), size = 3.75, fontface = "bold", colour = "black", check_overlap = TRUE) + 
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) +
  scale_y_continuous(limits = c(83, 100),breaks = seq(83, 100, by = 5)) +
  coord_cartesian(clip = "off") +
  
  labs(x = "Season", 
       y = "Average Goals Scored Per Game", 
       title = "Average Goals Scored Per Game.", 
       subtitle = "Circles represent average across all teams, per season.\nDiamonds represent average for each team, per season.", 
       caption = "data: @aaron_s_fox | by @loismackay") +
  
  theme(plot.background = element_rect(fill = "#1c1c1c"), panel.background = element_rect(fill = "#1c1c1c"), 
        plot.margin = margin(0.5,0.5,0.5,1, "cm"),
        legend.background = element_rect(fill = "#1c1c1c"), legend.key = element_rect(fill = "#1c1c1c"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20, colour = "white", face = "bold", family = "CR", margin = margin(15,0,0,0)),
        axis.text.y = element_text(size = 20, colour = "white", family = "CR"),
        axis.text.x = element_text(size = 14, colour = "white", family = "CR"), 
        axis.line.x = element_line(colour = "grey80", size = 0.3),
        axis.line.y = element_line(colour = "grey80", size = 0.3),
        plot.title = element_text(size = 28, colour = "white", hjust = 0.12, family = "CR"), 
        plot.subtitle = element_text(size = 18, colour = "white", hjust = 0.099, family = "CR",face = "italic", margin = margin(5,0,25,0)),
        plot.caption = element_text(colour = "grey80", size = 14, vjust = -0.75, hjust = 1, family = "CR"), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 12, family = "CR", colour = "grey90"), legend.margin = margin(0, 0, 0, 1, "cm"), 
        legend.position = "right", legend.direction = "vertical") 








#SSN plot number of goal scored at short and long distance 
ggplot() + ##short
  geom_point(data = short_long, aes(x = year, y = totalGoalsS, fill = squadName), shape = 21, size = 5)

ggplot() + ##long
  geom_point(data = short_long, aes(x = year, y = totalGoalsL, fill = squadName), shape = 21, size = 5)



##Breakdown per shot location, per year 
ggplot() +
  geom_point(data = team_year_loc, aes(x = year, y = shotPer, colour = squadName)) + 
  facet_wrap(~shotLocation)













################################################################################


##SD of clubs - Show deviation from their own average, how consistent are teams.... -----------------

#sum total number of shots and misses for each team, each game, at each shot location
pergame <- netball_numbers_7 %>% 
  group_by(year, matchId, roundNo, squadName, shotLocation) %>% 
  summarise(totalGoals = sum(goal), 
            totalMiss = sum(missedShots)) %>% 
  mutate(totalShots = totalGoals + totalMiss) %>% #add column to sum goals & misses for % accuracy
  mutate(shotPer = (totalGoals/totalShots)*100) %>% #add column to add % accuracy 
  mutate(across(starts_with("shotPer"), round, 1))


#create dataset with only 'long' shots
pergame_long <- 
  pergame[grep("long", pergame$shotLocation),] 

#sum all angles of shots (for long only)
pergame_long <- pergame_long %>% 
  group_by(year, matchId, roundNo, squadName) %>% 
  summarise(totalGoalsL = sum(totalGoals), 
            totalMissL = sum(totalMiss), 
            totalShotsL = sum(totalShots))


#create dataset with only 'short' shots
pergame_short <- 
  pergame[grep("short", pergame$shotLocation),] 

#sum all angles of shots (for long only)
pergame_short <- pergame_short %>% 
  group_by(year, matchId, roundNo, squadName) %>% 
  summarise(totalGoalsS = sum(totalGoals), 
            totalMissS = sum(totalMiss), 
            totalShotsS = sum(totalShots))


#combine the short and long datasets

pergame_short_long <- merge(pergame_long, pergame_short, by = c("year", "matchId", "roundNo", "squadName"))

#add % accuracy 

pergame_short_long <- pergame_short_long %>% 
  mutate(shotPerL = (totalGoalsL/totalShotsL)*100) %>% 
  mutate(shotPerS = (totalGoalsS/totalShotsS)*100) %>% 
  mutate(across(starts_with("shotPer"), round, 1))

#calculate the mean shots, mean goals and mean shooting % and SD for each (per year, per team?)

pergame_short_long_sd <- pergame_short_long %>% 
  group_by(year, squadName) %>% 
  summarise(mean_shotsL = mean(totalShotsL), 
            mean_shotsS = mean(totalShotsS), 
            mean_goalsL = mean(totalGoalsL), 
            mean_goalsS = mean(totalGoalsS), 
            mean_perL = mean(shotPerL), 
            mean_perS = mean(shotPerS), 
            sd_shotsL = sd(totalShotsL), 
            sd_shotsS = sd(totalShotsS), 
            sd_goalsL = sd(totalGoalsL), 
            sd_goalsS = sd(totalGoalsS), 
            sd_perL = sd(shotPerL), 
            sd_perS = sd(shotPerS))




 ########################################################            

##calculate shooting % per round (doesnt matter the year), per team


#sum total number of shots and misses for each team, each game
rounds <- netball_numbers_7 %>% 
  group_by(year, roundNo, squadName) %>% 
  summarise(totalGoals = sum(goal), 
            totalMiss = sum(missedShots)) %>% 
  mutate(totalShots = totalGoals + totalMiss) %>% #add column to sum goals & misses for % accuracy
  mutate(shotPer = (totalGoals/totalShots)*100) %>% #add column to add % accuracy 
  mutate(across(starts_with("shotPer"), round, 1))

#ignore the year, and work on rounds 

rounds2 <- netball_numbers_7 %>% 
  group_by(year, roundNo, squadName) %>% 
  summarise(totalGoals = sum(goal), 
            totalMiss = sum(missedShots)) %>% 
  mutate(totalShots = totalGoals + totalMiss) %>% #add column to sum goals & misses for % accuracy
  mutate(shotPer = (totalGoals/totalShots)*100) %>% #add column to add % accuracy
  mutate(across(starts_with("shotPer"), round, 1)) 

ssn_rounds <- netball_numbers_7 %>% 
  group_by(year, roundNo) %>% 
  summarise(totalGoals = sum(goal), 
            totalMiss = sum(missedShots)) %>% 
  mutate(totalShots = totalGoals + totalMiss) %>% #add column to sum goals & misses for % accuracy
  mutate(shotPer = (totalGoals/totalShots)*100) %>% #add column to add % accuracy 
  mutate(across(starts_with("shotPer"), round, 1))

ggplot() + 
  geom_point(data = ssn_rounds, aes(x = roundNo, y = shotPer), size = 10) + 
  geom_point(data = rounds2, aes(x = roundNo, y = shotPer, colour = squadName)) + 
  facet_wrap(~year) + 
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) +
  scale_x_continuous(limits = c(1, 10),breaks = seq(1, 10, by = 1))





##########################################################
######DENSITY PLOT 

#calculate total shots per game for density plot 
games <- netball_numbers_7 %>% 
  group_by(matchId, year, squadName) %>% 
  summarise(totalGoals = sum(goal), 
            totalMiss = sum(missedShots)) %>% 
  mutate(totalShots = totalGoals + totalMiss) %>% #add column to sum goals & misses for % accuracy
  mutate(shotPer = (totalGoals/totalShots)*100) %>% #add column to add % accuracy 
  mutate(across(starts_with("shotPer"), round, 1))


library(showtext)
font_add_google("Bayon", family = "CR")
showtext_auto()


order_sd <- c("Fever", "Lightning", "Vixens", "Magpies", "Firebirds", "Swifts", "Thunderbirds", "GIANTS")

(all_years <- ggplot(data = rounds2, aes(x = shotPer, y = squadName, fill = squadName)) + 
  geom_density_ridges(rel_min_height = 0.01, scale = 1.6, alpha = 1, colour = "white", lwd = 0.5,
                      quantile_lines = TRUE, quantile_fun = mean, vline_size = 1, vline_colour = "white",
                      jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0), 
                      point_shape = '|', point_size = 0, point_alpha = 1, point_colour = "white") + 
 # geom_label(data = mean_shotper, aes(x = meanshotper, y = squadName, label = paste0(meanshotper, "%")),
   #         size = 5, nudge_y = 0.4, nudge_x = 0, colour = "black", family = "CR") +
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) +
  scale_x_continuous(limits = c(65, 100),breaks = seq(65, 100, by = 5)) + 
  scale_y_discrete(limits = rev(order_sd)) +
  coord_cartesian(clip = "off") +
  
   labs(x = "Team Shooting Accuracy (%)", 
        y = "", 
        title = "", 
        subtitle = "Seasons 2018 - 2022 (R1 - R6)", 
        caption = "") +
   
   theme(plot.background = element_rect(fill = "#1c1c1c", colour = "#1c1c1c"), panel.background = element_rect(fill = "#1c1c1c"), 
         plot.margin = margin(0.5,5,0.5,1, "cm"),
         legend.background = element_rect(fill = "#1c1c1c"), legend.key = element_rect(fill = "#1c1c1c"), 
         panel.grid.major.x = element_blank(),
         panel.grid.minor = element_blank(),
         axis.ticks = element_blank(), 
         axis.title.y = element_blank(),
         axis.title.x = element_text(size = 20, colour = "white", face = "bold", family = "CR", margin = margin(15,0,0,0)),
         axis.text.y = element_text(size = 20, colour = "white", family = "CR"),
         axis.text.x = element_text(size = 14, colour = "white", family = "CR"), 
         axis.line.x = element_line(colour = "grey80", size = 0.3),
         axis.line.y = element_line(colour = "grey80", size = 0.3),
         plot.title = element_text(size = 28, colour = "white", hjust = 0, family = "CR"), 
         plot.subtitle = element_text(size = 24, colour = "white", hjust = 0, family = "CR",face = "italic", margin = margin(5,0,25,0)),
         plot.caption = element_blank(),
         legend.title = element_blank(), 
         legend.text = element_text(size = 12, family = "CR", colour = "grey90"), legend.margin = margin(0, 0, 0, 1, "cm"), 
         legend.position = "none", legend.direction = "vertical") 
)

all_years <- all_years +
  geom_text(label = "More \nConsistent", x = 110, y = 8, colour = "white", size = 6) 



#filter for only 2022 to show comparison to team average through all ssn season (from 2018)
twenty2 <- rounds2 %>% 
  filter(year == "2022")

twenty2av <- twenty2 %>% 
  group_by(squadName) %>% 
  summarise(meanper = mean(shotPer), 
            totalgoals = sum(totalGoals),
            totalshots = sum(totalShots), 
            shotPer_sd = sd(shotPer), 
            goals_sd = sd(totalGoals), 
            shots_sd = sd(totalShots)) %>%
  mutate(across(starts_with("meanper"), round, 1))

order_sd_22 <- c("Fever", "Vixens", "Firebirds", "Thunderbirds", "Swifts", "Lightning", "Magpies", "GIANTS")

(this_year <- ggplot(data = twenty2, aes(x = shotPer, y = squadName, fill = squadName)) + 
    geom_density_ridges(rel_min_height = 0.01, scale = 1.6, alpha = 1, colour = "white", lwd = 0.5,
                        quantile_lines = TRUE, quantile_fun = mean, vline_size = 1, vline_colour = "white",
                        jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0), 
                        point_shape = '|', point_size = 0, point_alpha = 1, point_colour = "white") + 
   # geom_label(data = twenty2av, aes(x = meanper, y = squadName, label = paste0(meanper, "%")),
              # size = 5, nudge_y = 0.4, nudge_x = 0, colour = "black", family = "CR") +
    scale_colour_manual(values = TeamColours) + 
    scale_fill_manual(values = TeamColours) +
    scale_x_continuous(limits = c(65, 100),breaks = seq(65, 100, by = 5)) + 
    scale_y_discrete(limits = rev(order_sd_22)) +
    coord_cartesian(clip = "off") +
    
    labs(x = "Team Shooting Accuracy (%)", 
         y = "", 
         title = "", 
         subtitle = "Season 2022 (R1 - R6)", 
         caption = "") +
    
    theme(plot.background = element_rect(fill = "#1c1c1c", colour = "#1c1c1c"), panel.background = element_rect(fill = "#1c1c1c"), 
          plot.margin = margin(0.5,3,0.5,1, "cm"),
          legend.background = element_rect(fill = "#1c1c1c"), legend.key = element_rect(fill = "#1c1c1c"), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(), 
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 20, colour = "white", face = "bold", family = "CR", margin = margin(15,0,0,0)),
          axis.text.y = element_text(size = 20, colour = "white", family = "CR"),
          axis.text.x = element_text(size = 14, colour = "white", family = "CR"), 
          axis.line.x = element_line(colour = "grey80", size = 0.3),
          axis.line.y = element_line(colour = "grey80", size = 0.3),
          plot.title = element_text(size = 28, colour = "white", hjust = 0, family = "CR"), 
          plot.subtitle = element_text(size = 24, colour = "white", hjust = 0, family = "CR",face = "italic", margin = margin(5,0,25,0)),
          plot.caption = element_text(colour = "grey80", size = 14, vjust = -0.75, hjust = 1, family = "CR"), 
          legend.title = element_blank(), 
          legend.text = element_text(size = 12, family = "CR", colour = "grey90"), legend.margin = margin(0, 0, 0, 1, "cm"), 
          legend.position = "none") 
)
  

##combine 2 plots together to show comparison

library(patchwork)


p <- all_years + this_year + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = 'SSN Scoring Accuracy Distribution, per Team',
    subtitle = 'How does 2022 compare, so far...', 
    caption = 'data: @aaron_s_fox | by @loismackay',
    theme = theme(plot.title = element_text(size = 32, colour = "white", hjust = 0.55, family = "CR", 
                                            margin = margin(0.5, 0, 0.1, 0, "cm")), 
                  plot.background = element_rect(fill = "#1c1c1c", colour = "#1c1c1c"), 
                  plot.subtitle = element_text(size = 24, colour = "white", hjust = 0.53, family = "CR",face = "italic", 
                                               margin = margin(5,0,25,0)),
                  plot.caption = element_text(colour = "grey80", size = 14, vjust = 0, hjust = 1.06, family = "CR"))) 



ggdraw(p) + 
  draw_label("Vertical white line = Team average shooting accuracy (%)", colour = "grey70", vjust = -30)


  







ggplot(data = rounds2, aes(x = shotPer, y = squadName, fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) + 
  scale_fill_viridis_c(name = "Probability", direction = -1)
 
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) 
  
  
  facet_wrap(~year)


adjust = 2.5, alpha = 0.4

#group shots to include shot and long 
shotsgrouped <- pergame_short_long %>% 
  group_by(squadName, matchId) %>%
  mutate(totalShots = totalShotsL + totalShotsS, 
         totalGoals = totalGoalsL + totalGoalsS, 
         totalMiss = totalMissL + totalMissS, 
         shotPer = (shotPerL + shotPerS)/2) %>%
  ungroup()

#Calculate SD for each team to order who is most consistent  
squad_sd <- rounds2 %>% 
  group_by(squadName) %>% 
  summarise(mean_shots = mean(totalShots), 
            mean_goals = mean(totalGoals), 
            mean_per = mean(shotPer),
            sd_shots = sd(totalShots), 
            sd_goals = sd(totalGoals), 
            sd_per = sd(shotPer))


mean_shotper <- rounds2 %>% 
  group_by(squadName) %>% 
  summarise(meanshotper = mean(shotPer))%>%
  mutate(across(starts_with("meanshotper"), round, 1))

