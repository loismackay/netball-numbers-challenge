# Load packages 
library(tidyverse)
library(devtools)
library(dplyr)
library(ggplot2)

library(readr)
library(here)

library(hrbrthemes)
library(ggtext)
library(png)
library(cowplot)
library(shape)
library(magick)


hrbrthemes::import_roboto_condensed()


# Read in team colours

TeamColours <- c("#fed22b", "#73D055FF", "#27AD81FF", 
                 "#7E4E90FF", "#CC6A70FF", "#2D708EFF", "#C0C0C0", "#FFA500")

names(TeamColours) <- c("Sunshine Coast Lightning", "West Coast Fever", "Melbourne Vixens", 
                        "Queensland Firebirds", "Adelaide Thunderbirds", "NSW Swifts", "Collingwood Magpies", "GIANTS Netball")



names(TeamColours) <- c("sc_lightning", "WestCoastFever", "MelbourneVixens", 
                        "FirebirdsQld", "AdelaideTBirds", "NSWSwifts", "collingwoodsn", "GIANTS_Netball")



#read in team logos 
lightning_logo <- readPNG("Logo_Lightning.png")
fever_logo <- readPNG("Logo_Fever.png")
vixens_logo <- readPNG("Logo_Vixens.png")
firebirds_logo <- readPNG("Logo_Firebirds.png")
swifts_logo <- readPNG("Logo_Swifts.png")
magpies_logo <- readPNG("Logo_Magpies.png")
giants_logo <- readPNG("Logo_Giants.png")
thunderbirds_logo <- readPNG("Logo_Thunderbirds.png")

ssn_logo <- readPNG("Logo_SSN.png")


# Read in data set 3 

netball_numbers_3 <- read_csv(here("datasets", "vol3", "somethingAboutRiddlesAndFruit.csv"))

netball_numbers_3_team <- read_csv(here("datasets", "vol3", "somethingAboutRiddlesAndFruit_teamTweets.csv"))



# Combine data sets together 

vol3_dataset <- rbind(netball_numbers_3, netball_numbers_3_team)




#average likes per tweet 

summary_vol3 <- vol3_dataset %>% 
  group_by(username) %>%
  summarise(TotalLikeCount = sum(likeCount), 
            TotalRetweetCount = sum(retweetCount), 
            TotalReplyCount = sum(replyCount), 
            TotalQuoteCount = sum(quoteCount)) %>%
  group_by(username) %>%
  mutate(TotalInteraction = sum(TotalLikeCount, TotalRetweetCount, TotalReplyCount, TotalQuoteCount))


#count - number of tweets per username 

total_tweet_count <- vol3_dataset %>% 
  count(username)



#combine tweet count column to summary vol 3 data set

vol3_dataset_merge <- merge(summary_vol3, total_tweet_count, by = "username")



#average number of 'likes' per tweet 

vol3_dataset_merge <- vol3_dataset_merge %>% 
  mutate(AvLikes = TotalLikeCount/n) %>%
  mutate_if(is.numeric, ~round(., 1)) %>% 
  arrange(-AvLikes) %>%
  filter(n > 1)


#filter for team official twitter profile only 

Official_Team <- vol3_dataset_merge %>% 
  filter(grepl("WestCoastFever|FirebirdsQld|GIANTS_Netball|sc_lightning|collingwoodsn|NSWSwifts|AdelaideTBirds|MelbourneVixens", username))




##Likes per team over time ---------------

#Separate date and time into 2 columns 

netball_numbers_3_team <- netball_numbers_3_team %>%
  separate(date, into = c("date", "time"), sep = " ", remove = FALSE)



#sum likes by date 

team_likes_by_date <- netball_numbers_3_team %>%
  group_by(username, date) %>% 
  summarise(TotalLikeCount = sum(likeCount), 
            TotalInteraction = sum(likeCount, quoteCount, retweetCount, replyCount))

#add another username column 
team_likes_by_date <- team_likes_by_date %>%
  mutate(name = username) %>% 
  mutate(name2 = username) %>%
  mutate(date2 = format(as.Date(date,"%Y-%m-%d"), format = "%d/%m"))

#rename usernames to official club titles

team_likes_by_date <- team_likes_by_date %>% 
  mutate(name = recode(name, "sc_lightning" = "Sunshine Coast Lightning", "WestCoastFever" = "West Coast Fever", "MelbourneVixens" = "Melbourne Vixens", 
"FirebirdsQld" = "Queensland Firebirds", "AdelaideTBirds" = "Adelaide Thunderbirds", "NSWSwifts" = "NSW Swifts", "collingwoodsn" = "Collingwood Magpies", "GIANTS_Netball" = "GIANTS Netball"))



#plot total likes per date for each team over time 

team_likes_by_date %>% 
  ggplot(aes(x = date, y = TotalInteraction, group = name, colour = name)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) + 
  coord_cartesian(clip = "off") +
  scale_x_date(date_breaks = "4 days", date_labels = "%d") +

theme(
  plot.background = element_rect(fill = "grey23"), 
  panel.background = element_rect(fill = "grey23"),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  plot.title = element_text(colour = "white", face = "bold", size = 18, margin = margin(10,0,0,0)), 
  plot.subtitle = element_text(colour = "white", size = 13, face = "italic", margin = margin(8, 0, 35, 0)),
  plot.caption = element_text(colour = "grey90", size = 12),
  
  axis.title.y = element_text(size = 15, colour = "white", face = "bold", margin = margin(0, 10, 0, 0)),
  axis.text.y = element_text(size = 10, colour = "white"),
  axis.title.x = element_text(size = 15, colour = "white", face = "bold", vjust = -2),
  axis.text.x = element_text(size = 10, colour = "white", angle = 30),
  axis.line.x = element_line(colour = "grey90", size = 0.5),
  axis.line.y = element_line(colour = "grey80", size = 0.5), 
  axis.line = element_line()) +
  
  labs(x = "Date (Days: 06/09/21 - 08/10/21)", 
       y = "Total Number of Interactions", 
       title = "Who 'won' the fan engagement over the SSN Signing Window", 
       subtitle = "Total interactions includes - Likes, retweets, quote tweets and replies on each tweet posted", 
       caption = "data: @aaron_s_fox | by @loismackay")





#facet plot, split by team 


team_likes_by_date %>% 
  ggplot(aes(x = date, y = TotalInteraction, group = name, colour = name)) +
  geom_line(aes(colour= name), size = 2) +
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) + 
  facet_wrap(~username)


#facet plot showing all team in each (total interaction)

p <- team_likes_by_date %>% 
  ggplot(aes(x = date, y = TotalInteraction)) + 
  geom_line(data = team_likes_by_date %>% dplyr::select(-name), aes(group = name2), 
            colour = "grey", size = 0.5, alpha = 0.4) +
  geom_line(aes(colour = username, group = username), size = 1.75) + 
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) + 
  facet_wrap(~name, nrow = 2, strip.position = "top", scales = "free") + 
  coord_cartesian(clip = "off") +
  
  theme(
    plot.background = element_rect(fill = "grey23"), 
    panel.background = element_rect(fill = "grey23"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none", 
    plot.title = element_text(colour = "white", face = "bold", size = 18, hjust = 0.09, margin = margin(10,0,0,0)), 
    plot.subtitle = element_text(colour = "white", size = 13, face = "italic", hjust = 0.1, margin = margin(8, 0, 35, 0)),
    plot.caption = element_text(colour = "grey90", size = 12), 
    strip.text = element_text(colour = "white", face = "bold", size = 14, hjust = 0.4),
    strip.background = element_blank(),
    axis.title.y = element_text(size = 15, colour = "white", face = "bold", margin = margin(0, 10, 0, 0)),
    axis.text.y = element_text(size = 10, colour = "white"),
    axis.title.x = element_text(size = 15, colour = "white", face = "bold", vjust = -2),
    axis.text.x = element_blank(), 
    axis.line.x = element_line(colour = "grey90", size = 0.5),
    axis.line.y = element_line(colour = "grey80", size = 0.5), 
    axis.line = element_line(),
    panel.spacing = unit(3, "lines")) + 

  
  labs(x = "Time (Days) 06/09/21 - 08/10/21", 
       y = "Total Number of Interactions", 
       title = "Who 'won' the fan engagement over the SSN Signing Window", 
       subtitle = "Total Twitter interactions (likes, retweets, quote tweets and replies) for each SSN team, shown across time (days)", 
       caption = "data: @aaron_s_fox | by @loismackay") 

  

p1 <- ggdraw(p) + 
  draw_text("Full Squad \nAnnouncement", x = 0.45, y = 0.8, size = 11, fontface = "bold.italic", colour = "#C0C0C0") +
  draw_text("Kiera-earthquake tweet,\n Watson captain &\nKate Eddy Signing", x = 0.95, y = 0.75, size = 11, fontface = "bold.italic", colour = "#27AD81FF") + 
  draw_text("Harry Styles\nAnnouncement", x = 0.2, y = 0.35, size = 11, fontface = "bold.italic", colour = "#2D708EFF") + 
  draw_text("Cute video &\n Vixens 'wind-up'", x = 0.45, y = 0.25, size = 11, fontface = "bold.italic", colour = "#7E4E90FF") + 
  draw_text("Harten signing &\n Austin leaving", x = 0.7, y = 0.65, size = 11, fontface = "bold.italic", colour = "#FFA500") + 
  
  
  draw_image(lightning_logo, x = 0.08, y = -0.12, scale = 0.065) + 
  draw_image(vixens_logo, x = 0.32, y = 0.35, scale = 0.07) + 
  draw_image(giants_logo, x = 0.07, y = 0.34, scale = 0.08) + 
  draw_image(magpies_logo, x = -0.185, y = 0.35, scale = 0.07) + 
  draw_image(thunderbirds_logo, x = -0.44, y = 0.345, scale = 0.055) + 
  draw_image(swifts_logo, x = -0.43, y = -0.10, scale = 0.05) + 
  draw_image(firebirds_logo, x = -0.18, y = -0.1, scale = 0.065) + 
  draw_image(fever_logo, x = 0.32, y = -0.1, scale = 0.07) + 
  draw_image(ssn_logo, x = -0.45, y = 0.46, scale = 0.075)

p1 + 
  geom_segment(aes(x = 0.665, y = 0.65, xend = 0.645, yend = 0.65), 
             arrow = arrow(length = unit(0.01, "npc"), type = "open"),
             colour = "#FFA500") + 
  geom_segment(aes(x = 0.42, y = 0.8, xend = 0.37, yend = 0.83), 
               arrow = arrow(length = unit(0.01, "npc"), type = "open"),
               colour = "#C0C0C0") + 
  geom_segment(aes(x = 0.915, y = 0.74, xend = 0.9, yend = 0.74), 
               arrow = arrow(length = unit(0.01, "npc"), type = "open"),
               colour = "#27AD81FF") + 
  geom_segment(aes(x = 0.175, y = 0.35, xend = 0.15, yend = 0.37), 
               arrow = arrow(length = unit(0.01, "npc"), type = "open"),
               colour = "#2D708EFF") + 
  geom_segment(aes(x = 0.415, y = 0.25, xend = 0.365, yend = 0.255), 
               arrow = arrow(length = unit(0.01, "npc"), type = "open"),
               colour = "#7E4E90FF") + 
  geom_segment(aes(x = 0.595, y = 0.025, xend = 0.65, yend = 0.025), 
               arrow = arrow(length = unit(0.015, "npc"), type = "open"),
               colour = "white", size = 0.8)

  
  
  
                                                                          



  
#find max interaction for each team

max <- team_likes_by_date %>% 
  group_by(username) %>% 
  summarise(max = max(TotalInteraction), 
            date = date)

  
  

#facet plot showing all team in each (likes per day only)

team_likes_by_date %>% 
  ggplot(aes(x = date, y = TotalLikeCount)) + 
  geom_line(data = team_likes_by_date %>% dplyr::select(-name), aes(group = name2), 
            colour = "grey", size = 0.5, alpha = 0.4) +
  geom_line(aes(colour = username, group = username), size = 1.75) + 
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) + 
  facet_wrap(~name, nrow = 2, strip.position = "top") + 
  coord_cartesian(clip = "off") +
  
  theme(
    plot.background = element_rect(fill = "grey23"), 
    panel.background = element_rect(fill = "grey23"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none", 
    plot.title = element_text(colour = "white", face = "bold", size = 18, margin = margin(10,0,0,0)), 
    plot.subtitle = element_text(colour = "white", size = 13, face = "italic", margin = margin(8, 0, 35, 0)),
    plot.caption = element_text(colour = "grey90", size = 12), 
    strip.text = element_text(colour = "white", face = "bold", size = 14, hjust = 0.4),
    strip.background = element_blank(),
    axis.ticks = element_blank(), 
    axis.title.y = element_text(size = 13, colour = "white", face = "bold", margin = margin(0, 10, 0, 0)),
    axis.text.y = element_text(size = 11, colour = "white", face = "bold"),
    axis.title.x = element_text(size = 13, colour = "white", face = "bold", vjust = -1.5),
    axis.text.x = element_blank(), 
    axis.line.x = element_line(colour = "grey90", size = 0.5),
    axis.line.y = element_line(colour = "grey80", size = 0.5), 
    panel.spacing = unit(2, "lines")) + 
  
  
  labs(x = "Date Range: 06/09/21 - 08/10/21", 
       y = "Number of 'Likes'", 
       title = "Who 'won' the fan engagement over the SSN Signing Window", 
       subtitle = "Total number of 'likes' per day, from each SSN team", 
       caption = "data: @aaron_s_fox | by @loismackay") 




##facet grid for top 4 teams with fan engagement -----------------

#find top 4 teams for fan interaction 

top4 <- team_likes_by_date %>% 
  group_by(username) %>% 
  summarise(TotalEngagementOverall = sum(TotalInteraction), 
            TotalLikeOverall = sum(TotalLikeCount)) %>% 
  arrange(-TotalEngagementOverall)




#filter data to top 4 and plot (likes)


team_likes_by_date %>% 
  filter(username == c("NSW Swifts", "Melbourne Vixens", "Queensland Firebirds", "Sunshine Coast Lightning")) %>%
  ggplot(aes(x = date, y = TotalLikeCount)) + 
  geom_line(data = team_likes_by_date %>% dplyr::select(-name), aes(group = name2), 
            colour = "grey", size = 0.5, alpha = 0.4) +
  geom_line(aes(colour = username, group = username), size = 1.75) + 
  scale_colour_manual(values = TeamColours) + 
  scale_fill_manual(values = TeamColours) + 
  facet_grid(name ~ .,) + 
  coord_cartesian(clip = "off") +
  
  theme(
    plot.background = element_rect(fill = "grey23"), 
    panel.background = element_rect(fill = "grey23"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none", 
    plot.title = element_text(colour = "white", face = "bold", size = 18, margin = margin(10,0,0,0)), 
    plot.subtitle = element_text(colour = "white", size = 13, face = "italic", margin = margin(8, 0, 35, 0)),
    plot.caption = element_text(colour = "grey90", size = 12), 
    strip.text = element_text(colour = "white", face = "bold", size = 14, hjust = 0.4),
    strip.background = element_blank(),
    axis.ticks = element_blank(), 
    axis.title.y = element_text(size = 13, colour = "white", face = "bold", margin = margin(0, 10, 0, 0)),
    axis.text.y = element_text(size = 11, colour = "white", face = "bold"),
    axis.title.x = element_text(size = 13, colour = "white", face = "bold", vjust = -1.5),
    axis.text.x = element_blank(), 
    axis.line.x = element_line(colour = "grey90", size = 0.5),
    axis.line.y = element_line(colour = "grey80", size = 0.5), 
    panel.spacing = unit(2, "lines")) + 
  
  
  labs(x = "Date Range: 06/09/21 - 08/10/21", 
       y = "Total Number of Interactions", 
       title = "Who 'won' the fan engagement over the SSN Signing Window", 
       subtitle = "Total interactions includes - Likes, retweets, quote tweets and replies on each tweet posted", 
       caption = "data: @aaron_s_fox | by @loismackay")


