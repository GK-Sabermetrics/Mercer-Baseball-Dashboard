# Random Side Work
# Packages ----
library(tidyverse)
library(ggforce)

data = read.csv("data/Fall Scrimmage Data.csv")
game = 
  data %>% 
  mutate(Count = paste(Balls, Strikes, sep = "-"),.after = Strikes)

p = unique(data$Pitcher[data$Date == "10/11/24"])

# Pitcher Rankings Updates ----
## Table ----
ps =
game %>% 
  #filter(Date == "10/11/24") %>% 
  #filter(Pitcher %in% p) %>% 
  filter(PitcherTeam == "MER_BEA") %>% 
  group_by(Pitcher) %>% 
  summarise(
    #Velo = max(RelSpeed, na.rm=T)
    'Innings' = length(Inning),
    #'BF' = length(which(Balls == 0 & Strikes == 0)),
    #'Strikes' = length(which(PitchCall != 'BallCalled')),
    #'Pitches' = n(),
    #FPS = length(which(PitchCall %in% c('StrikeCalled','StrikeSwinging', 'FoulBallNotFieldable') & Count == "0-0")),
    #'BB' = length(which(KorBB == 'Walk')),
    #'FBk' = length(which(TaggedPitchType %in% c("Fastball",'Cutter','Sinker','TwoSeamFastBall') & 
    #                       PitchCall != 'BallCalled')),
    #'FB' = length(which(TaggedPitchType %in% c("Fastball",'Cutter','Sinker','TwoSeamFastBall'))),
    #'BBk' = length(which(TaggedPitchType %in% c('Slider', 'Curveball') & 
    #                       PitchCall != 'BallCalled')),
    #'BBs' = length(which(TaggedPitchType %in% c('Slider', 'Curveball'))),
    #'CHk' = length(which(TaggedPitchType == 'ChangeUp' & 
    #                       PitchCall != 'BallCalled')),
    #'CH' = length(which(TaggedPitchType == 'ChangeUp'))
  )

## 3PWAR ----
first_two_strikes_before_two_balls <- data %>%
  group_by(Pitcher) %>%  # Group by pitcher and plate appearance
  filter(Strikes == 2 & Balls < 2) %>%        # Filter for counts with 2 strikes and fewer than 2 balls
  slice_min(PitchNo, n = 1, with_ties = FALSE) %>%  # Select the first occurrence based on Pitch UID
  ungroup() %>%
  group_by(Pitcher) %>%                       # Group by pitcher for final summarization
  summarise(total_occurrences = n())  


## Random Progress Table ----
table = 
game %>% 
  group_by(Count) %>% 
  summarise(
    K_Called = length(which(PitchCall == "StrikeCalled")),
    K_Swing = length(which(PitchCall == "StrikeSwinging")),
    Foul = length(which(PitchCall %in% c("FoulBallFieldable", "FoulBallNotFieldable"))),
    Ball = length(which(PitchCall %in% c("BallCalled", "BallinDirt"))),
  ) 

## Calculate Pither Progress ----
pd = 
data %>% 
  group_by(Pitcher) %>% 
  summarise(
    Hits = length(which(PitchCall == 'InPlay' & PlayResult != 'Out')),
    Runs = sum(RunsScored),
    HR = length(which(PlayResult == "HomeRun")),
    BB = length(which(KorBB == "Walk")),
    HBP = length(which(PitchCall == 'HitByPitch')),
    K = length(which(KorBB == 'Strikeout')),
  )

## Calculate Innings ----
innings_count <- data %>%
  filter(PitcherTeam=="MER_BEA") %>% 
  group_by(Pitcher, Date) %>%
  summarise(innings_pitched = n_distinct(Inning)) %>%
  ungroup() %>%
  group_by(Pitcher) %>%
  summarise(IP = sum(innings_pitched))

Progress = merge(innings_count, pd, by="Pitcher")

# PM Viewing ----

# *Data Upload ----

p = unique(data$Pitcher[data$PitcherTeam == "MER_BEA"]) %>% as.data.frame()

cols = c('Fastball' = '#d22d49', 'TwoSeamFastBall' = '#93afd4', 'ChangeUp' = '#1dbe3a', 
         'Slider' = '#c3bd0e', 'Curveball' = '#00d1ed', 'Cutter' = '#933f2c', 
         'Sinker' = '#de6a04', 'Splitter' = '#DDB33A', 'Slurve' = '#3BACAC', 'Sweeper' = "#6236CD",
         'Slutter' = '#c3870e')
bks = c('Fastball','TwoSeamFastBall','Sinker','Cutter','Splitter','ChangeUp','Curveball','Slider',
        'Slurve', 'Sweeper', 'Slutter')
lbs = c('FB','2SFB','SI','CU','SP','CH','CB','SL','SLV','SWP','SLT')

ggplot() +
  labs(x = "Horizontal Movement (in)", y = "Vertical Movement (in)", color = " ", title = "Pitch Movement") + 
  xlim(-32, 32) + ylim(-30, 30) +
  geom_segment(aes(x = 0, y = -28, xend = 0, yend = 28), size = 1, color = "grey55") + 
  geom_segment(aes(x = -28, y = 0, xend = 28, yend = 0), size = 1, color = "grey55") +
  geom_point(aes(x=12,y=20), size=5, color = "#d22d49") +
  geom_ellipse(aes(x0 = 12, y0 = 20, a = 8, b = 3, angle = 0), fill = "#d22d49", alpha = .3) +
  geom_point(aes(x=18,y=13), size=5, color = "#93afd4") +
  geom_ellipse(aes(x0 = 18, y0 = 14, a = 12, b = 3, angle = 0), fill = "#93afd4", alpha = .3) +
  geom_point(aes(x=18,y=10), size=5, color = "#de6a04") +
  geom_ellipse(aes(x0 = 18, y0 = 8, a = 10, b = 3, angle = 0), fill = "#de6a04", alpha = .3) +
  geom_point(aes(x=0,y=8), size=5, color = "#933f2c") +
  geom_ellipse(aes(x0 = 0, y0 = 6, a = 8, b = 3, angle = 0), fill = "#933f2c", alpha = .3) +
  geom_point(aes(x=-1,y=-1), size=5, color = "#c3bd0e") +
  geom_point(aes(x=-2,y=-5), size=5, color = "#c3bd0e") +
  geom_point(aes(x=-8,y=-2), size=5, color = "#c3bd0e") +
  geom_ellipse(aes(x0 = -12, y0 = -13, a = 10, b = 5, angle = -180), fill = "#c3bd0e", alpha = .3) +
  geom_point(aes(x=-10,y=-16), size=5, color = "#00d1ed") +
  geom_point(aes(x=-12,y=-13), size=5, color = "#00d1ed") +
  geom_point(aes(x=-16,y=-10), size=5, color = "#00d1ed") +
  geom_ellipse(aes(x0 = -12, y0 = -13, a = 10, b = 5, angle = -180), fill = "#00d1ed", alpha = .3) +
  geom_point(aes(x=15,y=10), size=5, color = "#1dbe3a") +
  geom_point(aes(x=10,y=15), size=5, color = "#1dbe3a") +
  geom_point(aes(x=20,y=0), size=5, color = "#1dbe3a") +
  geom_ellipse(aes(x0 = 15, y0 = 8, a = 10, b = 3, angle = -180), fill = "#1dbe3a", alpha = .3) +
  geom_point(aes(x=2,y=10), size=5, color = "#DDB33A") +
  #geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = 25), fill = "#d22d49", alpha = .5) +
  #geom_point(size = 2, na.rm = TRUE) +
  #scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8), axis.title = element_text(size = 14),
        legend.background = element_blank(), legend.key = element_blank())

## Pitcher ----
P = filter(data, Pitcher == "Ewaldsen, Collin")
subset = P[, c("RelSpeed")]
P = P[complete.cases(subset),]
# Pitch Movement
ggplot(data = P, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
  labs(x = "Horizontal Movement (in)", y = "Vertical Movement (in)", color = " ", title = "Pitch Movement") + 
  xlim(-32, 32) + ylim(-30, 30) +
  geom_segment(aes(x = 0, y = -28, xend = 0, yend = 28), size = 1, color = "grey55") + 
  geom_segment(aes(x = -28, y = 0, xend = 28, yend = 0), size = 1, color = "grey55") +
  geom_point(size = 2, na.rm = TRUE) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8), axis.title = element_text(size = 14),
        legend.background = element_blank(), legend.key = element_blank())


# Pitch Labeler

library(plotly)

data = read.csv("~/Downloads/20241030-MercerUniversity-Private-1_unverified.csv")

p = unique(data$Pitcher)

test = data %>% 
  group_by(Pitcher) %>% 
  mutate(avgVelo = mean(RelSpeed, na.rm = T),
         avgSpin = mean(SpinRate, na.rm = T), .after = Notes) %>% 
  ungroup() %>% 
  mutate(
    PitchType = case_when(
      InducedVertBreak > 14 & HorzBreak > 4 ~ "Fastball",
      between(InducedVertBreak, 10,14) & HorzBreak > 10 ~ "TwoSeamFastBall",
      between(HorzBreak, -5,5) & InducedVertBreak > 0 & SpinRate >= avgSpin-100 ~ "Cutter",
      RelSpeed < avgVelo & SpinRate < avgSpin ~ "ChangeUp"
    )
  )

test %>% select(Pitcher, TaggedPitchType, PitchType, RelSpeed, SpinRate, InducedVertBreak, HorzBreak)  

P = 
ggplot(data = test %>% filter(Pitcher == "Soliday, Cody"), aes(x = HorzBreak, y = InducedVertBreak, color = PitchType)) +
  labs(x = "Horizontal Movement (in)", y = "Vertical Movement (in)", color = " ", title = "Pitch Movement") + 
  xlim(-32, 32) + ylim(-30, 30) +
  geom_segment(aes(x = 0, y = -28, xend = 0, yend = 28), size = 1, color = "grey55") + 
  geom_segment(aes(x = -28, y = 0, xend = 28, yend = 0), size = 1, color = "grey55") +
  geom_point(size = 2, na.rm = TRUE) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8), axis.title = element_text(size = 14),
        legend.background = element_blank(), legend.key = element_blank())
ggplotly(P, tooltip = c("HorzBreak","InducedVertBreak","TaggedPitchType"))




test =
game %>% 
  filter(PitcherTeam == "MER_BEA") %>% 
  group_by(Pitcher) %>% 
  mutate(avgVelo = mean(RelSpeed, na.rm = T),
         avgSpin = mean(SpinRate, na.rm = T), .after = Notes) %>% 
  ungroup()






# this part is crap
testA = 
  test %>% 
  mutate(
    PitchType = case_when(
      RelSpeed > avgVelo & InducedVertBreak > 15 ~ "Fastball",
      RelSpeed > avgVelo-3 & between(InducedVertBreak, 11,15) & HorzBreak > 6 ~ "TwoSeamFastBall",
      RelSpeed > avgVelo & between(InducedVertBreak, 5,11) & HorzBreak > 6 ~ "Sinker",
      RelSpeed > avgVelo & SpinRate > avgSpin & between(InducedVertBreak, 2,14) & HorzBreak < 6 ~ "Cutter",
      RelSpeed < avgVelo & between(InducedVertBreak, -5, 0) ~ "Slider",
      RelSpeed < avgVelo & between(InducedVertBreak, 0, 10) & HorzBreak < 0 ~ "Slutter",
      RelSpeed < avgVelo & between(InducedVertBreak, -20, -5) & HorzBreak > -10 ~ "Curveball",
      RelSpeed < avgVelo & between(InducedVertBreak, -20, -5) & between(HorzBreak, -16, -10) ~ "Slurve",
      RelSpeed < avgVelo & between(InducedVertBreak, -20, -5) & HorzBreak < -16 ~ "Sweeper"
    ), .after = TaggedPitchType
    )

testA %>% filter()

testA %>% select(Pitcher, TaggedPitchType, PitchType, RelSpeed, avgVelo, SpinRate, avgSpin, InducedVertBreak, HorzBreak) %>% view()  

ggplot(data = testA %>% filter(Pitcher == "Mummert, Wyatt"), aes(x = HorzBreak, y = InducedVertBreak, color = PitchType)) +
  labs(x = "Horizontal Movement (in)", y = "Vertical Movement (in)", color = " ", title = "Pitch Movement") + 
  xlim(-32, 32) + ylim(-30, 30) +
  geom_segment(aes(x = 0, y = -28, xend = 0, yend = 28), size = 1, color = "grey55") + 
  geom_segment(aes(x = -28, y = 0, xend = 28, yend = 0), size = 1, color = "grey55") +
  geom_point(size = 2, na.rm = TRUE) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8), axis.title = element_text(size = 14),
        legend.background = element_blank(), legend.key = element_blank())

