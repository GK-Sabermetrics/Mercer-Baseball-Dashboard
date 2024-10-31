#
# 2023 Shiny APP THIS ONE WORKS and is the Updated One
# Make sure to use shiny run in background jobs to make it auto update
# Do this by opening shiny-run and app.R then go to background jobs, start background
# job, do the shiny-run, copy IP address, then in console do rstudioapi::viewer() or
# rstudioapi::translateLocalUrl(<URL>, absolute = TRUE)

# Notes: ADD average Exit velo, add XBHs on pitches, limiting factors
# also add HBP for batters and Runs if possible

library(shiny)
library(tidyverse)
library(DT)
library(scales)
library(shinythemes)
library(data.table)

data = fread('data/Fall Scrimmage Data.csv') %>% 
  mutate(RelSpeed = floor(RelSpeed),
         SpinRate = round(SpinRate),
         ExitSpeed = round(ExitSpeed),
         Angle = round(Angle,2),
         Distance = round(Distance,2),
         HorzBreak = round(HorzBreak,2),
         InducedVertBreak = round(InducedVertBreak,2)
         ) %>% 
  select(1:6,8:10,12,13,15:37,39:50,54:57,69,70,76,77,83:85)

PitchingData = filter(data, PitcherTeam == "MER_BEA")

BattingData = filter(data, BatterTeam == "MER_BEA")

d = data 

#data = read_csv("data/Fall Season 2023 - Full Fall Season (1).csv")

PitchCallChoices = list(
  'Ball' = 'BallCalled',
  'Int. Ball' = 'BallIntentional',
  'KC' = 'StrikeCalled',
  'KS' = 'StrikeSwinging',
  'Foul' = 'FoulBall',
  'Foul Fieldable ' = 'FoulBallFieldable',
  'Foul Not Fieldable' = 'FoulBallNotFieldable',
  'Ball Dirt' = 'BallinDirt',
  'In Play' = 'InPlay',
  'HBP' = 'HitByPitch',
  'Wild' = 'WildPitch'
)

HitTypeChoices = list(
  'Bunt' = 'Bunt',
  'Ground Ball' = 'GroundBall',
  'Line Drive' = 'LineDrive',
  'Fly Ball' = 'FlyBall',
  'Popup' = 'Popup'
)

PlayResultChoices = list(
  'Out' = 'Out',
  'Error' = 'Error',
  'Sac' = 'Sacrifice',
  'FC' = 'FieldersChoice',
  '1B' = 'Single',
  '2B' = 'Double',
  '3B' = 'Triple',
  'HR' = 'HomeRun'
)

PAOutChoices = list('BB' = 'Walk',
                    'K' = 'Strikeout')

PitchChoices = list(
  'FB' = 'Fastball',
  '2SFB' = 'TwoSeamFastBall',
  'CU' = 'Cutter',
  'SI' = 'Sinker',
  'SP' = 'Splitter',
  'CH' = 'ChangeUp',
  'CB' = 'Curveball',
  'SL' = 'Slider'
)



cols = c('Fastball' = '#d22d49', 'TwoSeamFastBall' = '#93afd4', 'ChangeUp' = '#1dbe3a', 
         'Slider' = '#c3bd0e', 'Curveball' = '#00d1ed', 'Cutter' = '#933f2c', 
         'Sinker' = '#de6a04', 'Splitter' = '#DDB33A', 'Four-Seam' = '#d22d49')
bks = c('Four-Seam','Fastball','TwoSeamFastBall','Sinker','Cutter','Splitter','ChangeUp','Curveball','Slider')
lbs = c('FB','FB','2SFB','SI','CU','SP','CH','CB','SL')

sz.top = 3.5
sz.bottom = 1.5
sz.right = 0.708
sz.left = -0.708

#PitchLocPlot Code ----
PitchLocPlot = 
ggplot() +
  ylim(0,5) + 
  labs(color = "") +
  scale_x_continuous(breaks = c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3),limits = c(-3,3)) +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15),linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15),linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5),linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3),linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15),linewidth = 1, color = "black") +
  geom_segment(aes(x = -1.208, y = 0, xend = -1.208, yend = 0.5), linewidth = 1, color = 'black') +
  geom_segment(aes(x = -1.208, y = 0, xend = -3, yend = 0), linewidth = 1, color = 'black') +
  geom_segment(aes(x = 1.208, y = 0, xend = 1.208, yend = 0.5), linewidth = 1, color = 'black') +
  geom_segment(aes(x = 1.208, y = 0, xend = 3, yend = 0), linewidth = 1, color = 'black') +
  geom_segment(aes(x = -0.83, y = 2.167, xend = 0.83, yend = 2.167), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.83, y = 2.833, xend = 0.83, yend = 2.833), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), linetype = 2, color = 'dark grey', linewidth = 1) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() + 
  theme(
        legend.position = 'bottom',
        legend.text = element_text(size = 10),
        axis.title = element_blank()
        ) +
  guides(color = guide_legend(override.aes = list(size=4)))

# Pitch Mov Code ----
MovPlot = 
ggplot() +
  labs(x = "Horizontal Movement (in)", y = "Vertical Movement (in)", color = " ", title = "Pitch Movement") + 
  xlim(-28, 28) + ylim(-28, 28) +
  geom_segment(aes(x = 0, y = -28, xend = 0, yend = 28), size = 1, color = "grey55") + 
  geom_segment(aes(x = -28, y = 0, xend = 28, yend = 0), size = 1, color = "grey55") +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() +
  theme(
        legend.position = "bottom", 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 10), 
        axis.title = element_text(size = 14)
        ) +
  guides(color = guide_legend(override.aes = list(size=4)))

# Rel Plot Code ----
RelPlot = 
ggplot() +
  labs(x = "Release Side (ft)", y = "Release Height (ft)", color = " ", title = "Release Points") + 
  xlim(-5, 5) + ylim(3, 7) +
  geom_segment(aes(x = 0, y = 3, xend = 0, yend = 7), size = 1, color = "grey55") + 
  geom_segment(aes(x = -5, y = 5, xend = 5, yend = 5), size = 1, color = "grey55") +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() + 
  theme(
        legend.position = 'bottom', 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 10), 
        axis.title = element_text(size = 14)
        ) +
  guides(color = guide_legend(override.aes = list(size=4)))

# ESV Plot Code ----
ESVPlot = 
  #this is the Extension Side View
  ggplot() +
  labs(x = "Extension (in)", y = "Release Height (in)", color = " ", title = "Extension Side View") +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  coord_cartesian(xlim = c(0,90), ylim = c(0,80)) +
  geom_segment(aes(x = 0, y = 0, xend = 48, yend = 0), size = 2, color = 'black') +
  geom_curve(aes(x = 48, y = 0, xend = 85, yend = -4), size = 2, curvature = -.05, color = 'black') +
  theme_bw() + 
  theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = 'bottom',
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14)
        ) +
  guides(color = guide_legend(override.aes = list(size=4)))

# ETV Plot Code ----

ETVPlot = 
  #this is the Extension Top View
  ggplot() +
  labs(x = "Extension (in)", y = "Release Side (in)", color = " ", title = "Extension Top View") +
  #geom_point(size = 3) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs, drop = FALSE) +
  coord_cartesian(xlim = c(0,96), ylim = c(-55,55)) +
  geom_segment(aes(x = -6, y = 10, xend = 0, yend = 10), color = 'black', size = 1) +
  geom_segment(aes(x = -6, y = -10, xend = 0, yend = -10), color = 'black', size = 1) +
  geom_segment(aes(x = 0, y = -10, xend = 0, yend = 10), color = 'black', size = 1) +
  theme_bw() + 
  theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 14)
        ) +
  guides(color = guide_legend(override.aes = list(size=4)))


# Spray Plot ----
SprayPlot = 
  ggplot() +
  xlim(-270,270) +
  ylim(-50,400) +
  geom_segment(aes(x=0,y=0,xend=226,yend=226), linewidth = 1) +
  geom_segment(aes(x=0,y=0,xend=-234,yend=234), linewidth = 1) +
  geom_segment(aes(x=60,y=60,xend=0,yend=120), linewidth = 1) +
  geom_segment(aes(x=-60,y=60,xend=0,yend=120), linewidth = 1) +
  #1B
  geom_point(aes(x = 60, y = 60), shape = 18, size =7) +
  #2B
  geom_point(aes(x = 0, y = 120), shape = 15, size =5) +
  #3B
  geom_point(aes(x = -60, y = 60), shape = 18, size =7) +
  geom_curve(aes(x=-234,y=234,xend=226,yend=226), curvature = -.64, ncp = 4, linewidth = 1) +
  geom_curve(aes(x=-90,y=90,xend=90,yend=90), curvature = -.75, linewidth = 1, ncp = 7) +
  theme_void()

# Pitcher Standings Table ----
PStandings = data %>%
  filter(PitcherTeam == "MER_BEA") %>%
  group_by('Pitcher' = Pitcher) %>%
  summarise(
            'IP' = length(unique(Inning)),
            'P' = n(),
            #'G' = length(unique(Date)),
            'BF' = length(which(Balls == 0 & Strikes == 0)),
            #'SO' = length(which(KorBB == "Strikeout")),
            'K' = length(which(KorBB == 'Strikeout')),
            'Strikes' = length(which(PitchCall != 'BallCalled')),
            #'KS' = length(which(PitchCall == 'StrikeSwinging' & KorBB == 'Strikeout')),
            #'KC' = length(which(PitchCall == 'StrikeCalled' & KorBB == 'Strikeout')),
            'BB' = length(which(KorBB == "Walk")),
            'HBP' = length(which(PitchCall == 'HitByPitch')),
            'H' = length(which(PitchCall == 'InPlay' & PlayResult != 'Out')),
            'R' = sum(RunsScored),
            #'2B' = length(which(PlayResult == 'Double')),
            #'3B' = length(which(PlayResult == 'Triple')),
            'HR' = length(which(PlayResult == "HomeRun")),
            
  )

# Pitcher Totals Table ----
PTotals = data %>%
  filter(PitcherTeam == "MER_BEA") %>%
  group_by(PitcherTeam) %>%
  summarise('P' = n(),
            'G' = length(unique(Date)),
            'BF' = length(which(Balls == 0 & Strikes == 0)),
            #'K' = length(which(KorBB == "Strikeout")),
            'KS' = length(which(PitchCall == 'StrikeSwinging' & KorBB == 'Strikeout')),
            'KC' = length(which(PitchCall == 'StrikeCalled' & KorBB == 'Strikeout')),
            'BB' = length(which(KorBB == "Walk")),
            'H' = length(which(PitchCall == 'InPlay' & PlayResult != 'Out')),
            'R' = sum(RunsScored),
            '2B' = length(which(PlayResult == 'Double')),
            '3B' = length(which(PlayResult == 'Triple')),
            'HR' = length(which(PlayResult == "HomeRun")),
  )%>%.[, c(2,3,4,5,6,7,8,9,10,11,12)]

# Batter Standings Table ----
BStand = data %>%
  filter(BatterTeam == "MER_BEA") %>%
  group_by(Batter) %>%
  summarise(
    PA = length(which(Balls == 0 & Strikes == 0)),
    AB = length(which(!KorBB %in% c('Undefined','Walk') | !PlayResult %in% c('Undefined', 'Sacrifice'))),
    H = length(which(!PlayResult %in% c('Undefined', 'Out', 'Sacrifice'))),
    AVG = sprintf((H/AB), fmt = '%#.3f'),
    K = length(which(KorBB == 'Strikeout')),
    BB = length(which(KorBB == 'Walk')),
    OBP = sprintf((H+BB)/(AB+BB), fmt = '%#.3f'),
    `1B` = length(which(PlayResult == 'Single')),
    `2B` = length(which(PlayResult == 'Double')),
    `3B` = length(which(PlayResult == 'Triple')),
    `HR` = length(which(PlayResult == 'HomeRun')),
    TB = sum(length(which(PlayResult == 'Single'))*1, length(which(PlayResult == 'Double'))*2,
             length(which(PlayResult == 'Triple'))*3, length(which(PlayResult == 'HomeRun'))*3),
    SLG = sprintf(TB/AB, fmt = '%#.3f'),
    OPS = sprintf(((H+BB)/(AB+BB))+(TB/AB), fmt = '%#.3f'),
    RBI = sum(RunsScored[PitchCall=='InPlay']),
    SF = length(which(TaggedHitType %in% c("FlyBall", "Popup") & PlayResult == "Sacrifice")),
    SH = length(which(TaggedHitType == "Bunt" & PlayResult == "Sacrifice"))
  )%>%.[, c(1,5,8,14,15,2,3,4,6,7,9,10,11,12,16,13,17,18)]

# Batter Totals Table ----
BTotals = data%>%
  filter(BatterTeam == "MER_BEA") %>%
  group_by(BatterTeam)%>%
  summarise(
    PA = length(which(Balls == 0 & Strikes == 0)),
    AB = length(which(!KorBB %in% c('Undefined','Walk') | !PlayResult %in% c('Undefined', 'Sacrifice'))),
    H = length(which(!PlayResult %in% c('Undefined', 'Out', 'Sacrifice'))),
    AVG = sprintf((H/AB), fmt = '%#.3f'),
    K = length(which(KorBB == 'Strikeout')),
    BB = length(which(KorBB == 'Walk')),
    OBP = sprintf((H+BB)/(AB+BB), fmt = '%#.3f'),
    `1B` = length(which(PlayResult == 'Single')),
    `2B` = length(which(PlayResult == 'Double')),
    `3B` = length(which(PlayResult == 'Triple')),
    `HR` = length(which(PlayResult == 'HomeRun')),
    TB = sum(length(which(PlayResult == 'Single'))*1, length(which(PlayResult == 'Double'))*2,
             length(which(PlayResult == 'Triple'))*3, length(which(PlayResult == 'HomeRun'))*3)%>%round(digits = 0),
    SLG = sprintf(TB/AB, fmt = '%#.3f'),
    OPS = sprintf(((H+BB)/(AB+BB))+(TB/AB), fmt = '%#.3f'),
    RBI = sum(RunsScored[PitchCall=='InPlay']),
    SF = length(which(TaggedHitType %in% c("FlyBall", "Popup") & PlayResult == "Sacrifice")),
    SH = length(which(TaggedHitType == "Bunt" & PlayResult == "Sacrifice"))
  )%>%.[, c(5,8,14,15,2,3,4,6,7,9,10,11,12,13,16,17,18)]



# Batter Loc Plot ----
BatLocPlot = 
  ggplot() +
  ylim(0,5) + 
  labs(color = "") +
  scale_x_continuous(breaks = c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3),limits = c(-3,3)) +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
  geom_segment(aes(x = 0.708, y = 0.45, xend = 0.708, yend = 0.3), linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.45, xend = -0.708, yend = 0.3), linewidth = 1, color = "black") +
  geom_segment(aes(x = 0, y = 0, xend = 0.708, yend = 0.3), linewidth = 1, color = "black") +
  geom_segment(aes(x = 0, y = 0, xend = -0.708, yend = 0.3), linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.45, xend = 0.708, yend = 0.45), linewidth = 1, color = "black") +
  geom_segment(aes(x = -1.208, y = 0, xend = -1.208, yend = 0.5), linewidth = 1, color = 'black') +
  geom_segment(aes(x = -1.208, y = 0.5, xend = -3, yend = 0.5), linewidth = 1, color = 'black') +
  geom_segment(aes(x = 1.208, y = 0, xend = 1.208, yend = 0.5), linewidth = 1, color = 'black') +
  geom_segment(aes(x = 1.208, y = 0.5, xend = 3, yend = 0.5), linewidth = 1, color = 'black') +
  geom_segment(aes(x = -0.83, y = 2.167, xend = 0.83, yend = 2.167), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.83, y = 2.833, xend = 0.83, yend = 2.833), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), linetype = 2, color = 'dark grey', linewidth = 1) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_bw() + 
  theme(
    legend.position = 'bottom',
    legend.text = element_text(size = 10),
    axis.title = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size=4)))

# Batter Contact Plot ----
BatContPlot = 
  ggplot() +
  xlim(-2,2) + ylim(-1,5) +
  geom_segment(aes(x = 0, y = 0, xend = .708, yend = .61), size = 1, color = 'black') +
  geom_segment(aes(x = 0, y = 0, xend = -0.708, yend = .61), size = 1, color = 'black') +
  geom_segment(aes(x = -0.708, y = .61, xend = -0.708, yend = 1.40), size = 1, color = 'black') +
  geom_segment(aes(x = 0.708, y = .61, xend = 0.708, yend = 1.40), size = 1, color = 'black') +
  geom_segment(aes(x = -0.708, y = 1.40, xend = 0.708, yend = 1.40), size = 1, color = 'black') +
  geom_segment(aes(x = -1.2, y = -1, xend = -1.2, yend = 3.60), size = 1, color = 'black') +
  geom_segment(aes(x = 1.2, y = -1, xend = 1.2, yend = 3.60), size = 1, color = 'black') +
  geom_segment(aes(x = -1.2, y = 3.6, xend = -2, yend = 3.60), size = 1, color = 'black') +
  geom_segment(aes(x = 1.2, y = 3.6, xend = 2, yend = 3.60), size = 1, color = 'black') +
  labs(color = "") +
  theme_bw() +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme(legend.position = 'bottom',
        axis.title = element_blank()) +
  geom_point(size = 2, na.rm = TRUE)

# fluidPage(theme = shinytheme("united"),

# UI ----
ui <- fluidPage(theme = shinytheme("united"),
  tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "DashStyle.css")
  ),
  # Navbar ----
  navbarPage("MU Baseball",
    #tags$img(src = 'SailorBear.png', width = 48, height = 48),
              # Home ----
              tabPanel("Home",
                       tabsetPanel(
                         tabPanel('Please Read',
                                  tags$h1('PLEASE READ BEFORE CONTINUING'),
                                  tags$h4('This application operates on a time usage basis,
                                  so every minute you spend on the application counts 
                                  toward our usage limit. The usage limit that we have
                                  is 500 hours/month. If we hit the limit we are charged extra 
                                  money, so please use this app deliberately. Also make sure
                                  you close the tab you are using for this app when you are done
                                  to ensure we are not waisting time.'),
                                  fluidRow(
                                    column(6,
                                           tags$h1('Home'),
                                           tags$h2('Team Pitching Tab'),
                                           tags$h4('Page with individual pitching stats
                                                   as well as overall team stats.'),
                                           tags$h2('Team Batting Tab'),
                                           tags$h4('Page with individual batting stats
                                                   as well as overall team stats.')
                                           ),
                                    column(6,
                                           tags$h1('Pitching Tab'),
                                           tags$h4('Selecters and filters are located on the 
                                                   left hand side of the page. Tabs with tables 
                                                   and graphs are located on the right. Select your name
                                                   from the dropdown to see overall stats/graphs. 
                                                   Select a date to see stats/graphs from a specific
                                                   game.'),
                                           tags$h4('Use filters to see information 
                                                   such as specific pitches, counts, or batted balls.
                                                   Filters are applied to all tables and 
                                                   graphs. Tabs on the right hand side have their respective
                                                   graphs/info.')
                                    ),
                                  ),
                                    fluidRow(
                                    column(6,
                                           tags$h1('Batting Tab'),
                                           tags$h4('Selecters and filters are located on the 
                                                   left hand side of the page. Tabs with tables 
                                                   and graphs are located on the right. Select your name
                                                   from the dropdown to see overall stats/graphs. 
                                                   Select a date to see stats/graphs from a specific
                                                   game. You also have a table below your stats that will show
                                                   specific pitches/info of each AB in order.'),
                                           tags$h4('Use filters to see information 
                                                   such as specific arm sides, counts, or pitch calls.
                                                   Filters are applied to all tables and 
                                                   graphs. Tabs on the right hand side have their respective
                                                   graphs/info.'),
                                           tags$h4('FYI WHEN USING FILTERS: the stats for PAs and ABs
                                                   will be wrong when certain filters are applied. To see correct 
                                                   PA/AB info, only use the name and date selectors, not filters.'),
                                           tags$h4('Also, the spray chart shows where the ball initially landed,
                                                   so most of the dots near home plate are where balls first
                                                   hit the ground. Pay more attention to tendency.')
                                           ),
                                    column(6,
                                    tags$h1('General Info'),
                                    tags$h4('Sometimes if you are going back and forth changing names and 
                                                   dates the graphs and tables will get messed up. If things look
                                                   messed up, just reset the name and date selectors to all and 
                                                   uncheck all filters. The same happens with filters as well, 
                                                   especially if you select a filter that does not apply to you
                                                   (i.e. selecting a pitch filter for a pitch you do not throw).
                                                   When in doubt, reset the whole page and uncheck filters.')
                                    ),
                                  )
                                  ), #Tab Panel for Home
                         tabPanel('Team Pitching',
                                  h1('Team Pitching Totals'),
                                  tableOutput('PitcherStandingsTotals'),
                                  h1('Pitcher Standings'),
                                  dataTableOutput('PitcherStandings')
                                  ),
                         tabPanel('Team Batting',
                                  h1("Team Batting Totals"),
                                  tableOutput('BatterStandingsTotals'),
                                  h1('Batter Standings'),
                                  dataTableOutput('BatterStandings')
                                  )
                       ),
                       ),
              # Pitching ----
              tabPanel("Pitching",
                       sidebarLayout(
                         sidebarPanel(
                           #> Filters ----
                           fluidRow(
                             column(5, selectInput("pitcher", "Pitcher", 
                                                   choices = c("all",unique(PitchingData$Pitcher)))),
                             column(4, selectInput("date", "Date", choices = c("all", unique(data$Date)))),
                             #column(3, selectInput("team", "Team", choices = c("all", unique(data$PitcherTeam))))
                           ),
                           fluidRow(
                             column(2, checkboxGroupInput("strike", "Strikes", c('0','1','2'))),
                             column(2, checkboxGroupInput('ball','Balls',c('0','1','2','3'))),
                             column(2, checkboxGroupInput('out','Outs',c('0','1','2'))),
                             column(2, checkboxGroupInput('BH','BH',c('Left','Right'))),
                             column(3, checkboxGroupInput('PAOut', 'PA Outcome', PAOutChoices))
                           ),
                           fluidRow(
                             column(3, checkboxGroupInput("pcall", "Pitch Call", PitchCallChoices)),
                             column(3, checkboxGroupInput("ht", "Hit Type", HitTypeChoices)),
                             column(3, checkboxGroupInput('pr', "Play Result", PlayResultChoices)),
                             column(3, checkboxGroupInput("pitch", "Pitch", PitchChoices))
                           ),
                           fluidRow(
                             #column(5, sliderInput('blls', 'Limit to Last:', min = 10, max = 100, value = 50 ))
                           ),
                         ), #sidebar panel
                         # Main Panel ----
                         mainPanel(
                           tabsetPanel(
                             #> Pitcher Table ----
                             tabPanel("Pitch Table",
                                      tableOutput("PitchMetrics")
                             ),
                             #> Pitch Locations ----
                             tabPanel("Pitch Locations",
                                      plotOutput(outputId = "PitchLoc",
                                                 width = "60%",height = "500px"),
                                      dataTableOutput('PitcherAppears', height = 'auto', width = '50%')
                             ),
                             #> PM, Rel, ESV, ETV ----
                             tabPanel("Pitch Movement",
                                      plotOutput("PitchMov", 
                                                 click = "MovClick",
                                                 width = "50%", height = "400px"),
                                      tableOutput('MovData')
                             ),
                             tabPanel('Release Points',
                                      plotOutput("PitchRel", width = "50%", height = "400px")
                                      ),
                             tabPanel('Extension Side',
                                      plotOutput("ExtSide", width = "50%", height = "400px")
                                      ),
                             tabPanel('Extension Top',
                                      plotOutput("ExtTop", width = "50%", height = "400px")
                                      )
                           ), #tabset panel
                         ) #main panel
                       ) #sidebar layout
              ), #tabpanel Pitching
               # Batting ----
              tabPanel("Batting",
                       sidebarLayout(
                         sidebarPanel(
                           fluidRow(
                             #> Filters ----
                             column(5, selectInput("batter", "Batter", choices = c("all",unique(BattingData$Batter)))),
                             column(4, selectInput("dateB", "Date", choices = c("all", unique(data$Date)))),
                             #column(3, selectInput("teamB", "Team", choices = c("all", unique(data$PitcherTeam))))
                           ),
                           fluidRow(
                             column(2, checkboxGroupInput("strikeB", "Strikes", c('0','1','2'))),
                             column(2, checkboxGroupInput('ballB','Balls',c('0','1','2','3'))),
                             column(2, checkboxGroupInput('outB','Outs',c('0','1','2'))),
                             column(2, checkboxGroupInput('arm', 'Arm Side', choices = c('Left', 'Right'))),
                             column(2, checkboxGroupInput('zone', 'Zone', choices = c('Inside', 'Outside')))
                           ),
                           fluidRow(
                             column(3, checkboxGroupInput('pcallB', 'Pitch Call', PitchCallChoices)),
                             column(3, checkboxGroupInput("htB", "Hit Type", HitTypeChoices)),
                             column(3, checkboxGroupInput('prB', "Play Result", PlayResultChoices)),
                             column(3, checkboxGroupInput("pitchB", "Pitch", PitchChoices))
                           )
                         ), #Sidebar Panel Batting
                         mainPanel(
                           tabsetPanel(
                             #> Batter Table ----
                             tabPanel('Batter Table',
                                      fluidRow(
                                        column(12, tableOutput('BattingMetrics')),
                                      ),
                                      ),
                             #> Batter Loc ----
                             tabPanel('Locations',
                                      plotOutput("BatLoc", width = "60%", height = "500px"),
                                      dataTableOutput('BatterAppears', height = 'auto', width = '50%')
                                      ),
                             #> Batter Contact ----
                             tabPanel('Contact Chart',
                                      plotOutput("BatContact", width = "60%", height = "500px")
                             ),
                             #> Spray Chart ----
                             tabPanel('Spray Chart',
                                      plotOutput('SprayChart', 
                                                 click = 'SprayClick',
                                                 width = '100%', height = '800px'),
                                      tableOutput('SprayData')
                                      ),
                           ),
                         ),
                       )#sidebarLayout
                       ), 
  ) #navbar page
) #fluid page

server <- function(input, output, session) {

  # Data Frames ----
  PitchingDF = reactive({
    data %>%
      filter(PitcherTeam == "MER_BEA") %>% 
      filter(if (input$pitcher != "all") Pitcher == input$pitcher else TRUE,
             if (input$date != "all") Date == input$date else TRUE,
             #if (input$team != "all") BatterTeam == input$team else TRUE,
             if (is.null(input$strike)) TRUE else Strikes %in% input$strike,
             if (is.null(input$ball)) TRUE else Balls %in% input$ball,
             if (is.null(input$out)) TRUE else Outs %in% input$out,
             if (is.null(input$BH)) TRUE else BatterSide %in% input$BH,
             if (is.null(input$pcall)) TRUE else PitchCall %in% input$pcall,
             if (is.null(input$ht)) TRUE else TaggedHitType %in% input$ht,
             if (is.null(input$pr)) TRUE else PlayResult %in% input$pr,
             if (is.null(input$PAOut)) TRUE else KorBB %in% input$PAOut,
             if (is.null(input$pitch)) TRUE else TaggedPitchType %in% input$pitch)
    
  })
  
  PitcherDF = reactive({
    data %>%
      filter(if (input$pitcher != "all") Pitcher == input$pitcher else TRUE)
  })
  # BattingDF ----
  BattingDF = reactive({
    data %>% 
      mutate(
        brg = Bearing * (pi/180),
        BallY = Distance*cos(brg),
        BallX = Distance*sin(brg)
             ) %>% 
      filter(BatterTeam == "MER_BEA") %>% 
      filter(if (input$batter != 'all') Batter == input$batter else TRUE,
             if (input$dateB != "all") Date == input$dateB else TRUE,
             if (is.null(input$strikeB)) TRUE else Strikes %in% input$strikeB,
             if (is.null(input$ballB)) TRUE else Balls %in% input$ballB,
             if (is.null(input$outB)) TRUE else Outs %in% input$outB,
             if (is.null(input$arm)) TRUE else PitcherThrows %in% input$arm,
             if (is.null(input$pcallB)) TRUE else PitchCall %in% input$pcallB,
             if (is.null(input$htB)) TRUE else TaggedHitType %in% input$htB,
             if (is.null(input$prB)) TRUE else PlayResult %in% input$prB,
             if (is.null(input$pitchB)) TRUE else TaggedPitchType %in% input$pitchB,
             if (is.null(input$zone)) TRUE else if (input$zone == 'Inside') {
               PlateLocSide >= -0.708 & PlateLocSide <= 0.708 &
               PlateLocHeight <= 3.5 & PlateLocHeight >= 1.5
               
             } else if (input$zone == 'Outside') {
               PlateLocSide <= -0.958 | PlateLocSide >= 0.958 |
               PlateLocHeight >= 3.75 | PlateLocHeight <= 1.25
             }
             )
  })
  
  BatterDF = reactive({
    data %>% 
      filter(if (input$batter != 'all') Batter == input$batter else TRUE)
  })
  
  # Observers ----
  observeEvent(input$pitcher, {
    choices = if (input$pitcher == "all") {
      c("all", unique(data$Date))
    } else {
      c("all", unique(data$Date[data$Pitcher == input$pitcher]))
    }
    updateSelectInput(session, "date", choices = choices, selected = input$date) 
  })
  
  # Date Batter Observer
  observeEvent(input$batter, {
    choices = if (input$batter == "all") {
      c("all", unique(data$Date))
    } else {
      c("all", unique(data$Date[data$Batter == input$batter]))
    }
    updateSelectInput(session, "dateB", choices = choices, selected = input$dateB) 
  })
  
  
  observeEvent(input$date, {
    choices = if (input$date == "all") {
      c("all", unique(PitchingData$Pitcher))
    } else {
      c("all", unique(PitchingData$Pitcher[PitchingData$Date == input$date]))
    }
    updateSelectInput(session, "pitcher", choices = choices, selected = input$pitcher)  
  })
  
  # Batter Date Observer
  observeEvent(input$dateB, {
    choices = if (input$dateB == "all") {
      c("all", unique(BattingData$Batter))
    } else {
      c("all", unique(BattingData$Batter[BattingData$Date == input$dateB]))
    }
    updateSelectInput(session, "batter", choices = choices, selected = input$batter)  
  })
  
  
  observeEvent(input$team, {
    choices = if (input$team == "all") {
      c("all", unique(data$Pitcher))
    } else {
      c("all", unique(data$Pitcher[data$PitcherTeam == input$team]))
    }
    updateSelectInput(session, "pitcher", choices = choices, selected = input$pitcher)  
  })
  
  observeEvent(input$pitcher, {
    updateCheckboxGroupInput(session, "pcall", choices = PitchCallChoices[PitchCallChoices %in% unique(PitcherDF()$PitchCall)])
    updateCheckboxGroupInput(session, "ht", choices = HitTypeChoices[HitTypeChoices %in% unique(PitcherDF()$TaggedHitType)])
    updateCheckboxGroupInput(session, "pr", choices = PlayResultChoices[PlayResultChoices %in% unique(PitcherDF()$PlayResult)])
    updateCheckboxGroupInput(session, "PAout", choices = PAOutChoices[PAOutChoices %in% unique(PitcherDF()$KorBB)])
    updateCheckboxGroupInput(session, "pitch", choices = PitchChoices[PitchChoices %in% unique(PitcherDF()$TaggedPitchType)])
  })
  
  observeEvent(input$batter, {
    updateCheckboxGroupInput(session, "pcallB", choices = PitchCallChoices[PitchCallChoices %in% unique(BatterDF()$PitchCall)])
    updateCheckboxGroupInput(session, "htB", choices = HitTypeChoices[HitTypeChoices %in% unique(BatterDF()$TaggedHitType)])
    updateCheckboxGroupInput(session, "prB", choices = PlayResultChoices[PlayResultChoices %in% unique(BatterDF()$PlayResult)])
    updateCheckboxGroupInput(session, "pitchB", choices = PitchChoices[PitchChoices %in% unique(BatterDF()$TaggedPitchType)])
  })
  
  
  # Pitcher Table ----
  output$PitchMetrics = renderTable({
    table = PitchingDF() %>%
      filter(PitcherTeam == "MER_BEA") %>% 
      drop_na(RelSpeed) %>% 
      group_by('Pitch' = TaggedPitchType) %>%
      summarise('Total' = n(),
                'Usage' = percent(n()/length(.$TaggedPitchType)),
                'Max' = max(RelSpeed, na.rm = TRUE),
                'Avg' = floor(mean(RelSpeed, na.rm = TRUE)),
                'Spin' = floor(mean(SpinRate, na.rm = TRUE)),
                'Tilt' = Tilt %>% as.POSIXct(format = '%H:%M', tz = 'UTC') %>%
                  as.numeric() %>% mean() %>%
                  as.POSIXct(origin = '1970-01-01', tz = 'UTC') %>%
                  format(format = "%k:%M", tz = 'UTC'),
                'HB' = round(mean(HorzBreak, na.rm = TRUE), 2),
                'IVB' = round(mean(InducedVertBreak, na.rm = TRUE), 2),
                'Strikes' = length(which(PitchCall != 'BallCalled')),
                'Strike%' = percent(length(which(PitchCall == "StrikeCalled"))/n()),
                #'Whiff' = length(which(PitchCall == "StrikeSwinging")),
                'Whiff' = percent(length(which(PitchCall == "StrikeSwinging"))/n()),
                'LHB' = percent(length(which(BatterSide == "Left"))/n()),
                'RHB' = percent(length(which(BatterSide == "Right"))/n()),
                'FP' = length(which(Balls == 0 & Strikes == 0)),
                'PC' = length(which(Strikes == 2 & Balls != 3)),
      )
  }, striped = TRUE, bordered = TRUE)
  
  # Pitcher Standings Table ----
  output$PitcherStandings = renderDataTable({
    PStandings
  })
  
  # Pitcher Totals ----
  output$PitcherStandingsTotals = renderTable({
    PTotals
  })
  
  # Pitch Loc Graph ----
  output$PitchLoc = renderPlot({
    PitchLocPlot + 
      geom_point(aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType), 
                 PitchingDF(), size = 9) + 
      geom_text(data = PitchingDF(), 
                aes(label = PitchNo, x = PlateLocSide, y = PlateLocHeight), color = "white", size = 4)
  })
  
  # Pitch Appears Table ----
  output$PitcherAppears = renderDataTable({
    
    table = PitchingDF() %>% 
      select(PitchNo, Inning, Strikes, Balls, Outs, TaggedPitchType, PitchCall, PlayResult, 
             RelSpeed, SpinRate, Tilt, InducedVertBreak, HorzBreak)
    
    colnames(table) = c('#','Inn','S','B','O','Pitch','Call', 'Result', 'Velo','Spin','Tilt','IVB','HB')
    
    table
  })
  
  
  # Pitch Movement graph ----
  output$PitchMov = renderPlot({
    MovPlot + geom_point(aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType), PitchingDF(), size = 3)
  })
  
  # Release Plot ----
  output$PitchRel = renderPlot({
    RelPlot + geom_point(aes(x = RelSide, y = RelHeight, color = TaggedPitchType), PitchingDF(), size = 3)
  })
  
  # Extension Side View Plot ----
  output$ExtSide = renderPlot({
    ESVPlot + geom_point(aes(x = Extension*12, y = RelHeight*12, color = TaggedPitchType), PitchingDF(), size = 3)
  })
  
  # Extension Top View Plot ----
  output$ExtTop = renderPlot({
    ETVPlot + geom_point(aes(x = Extension*12, y = RelSide*12, color = TaggedPitchType), PitchingDF(), size = 3)
  })
  
  # Click Actions ----
 output$MovData = renderTable({
   req(input$MovClick)
   nearPoints(PitchingDF(), input$MovClick, xvar = "HorzBreak", yvar = "InducedVertBreak")%>%
     select('BatterSide', 'Outs','Balls', 'Strikes', 'TaggedPitchType', 'PitchCall','RelSpeed',
            'SpinRate', 'Tilt', 'InducedVertBreak', 'HorzBreak')
 })
  
 output$SprayData = renderTable({
   req(input$SprayClick)
   nearPoints(BattingDF(), input$SprayClick, xvar = "BallX", yvar = "BallY") %>% 
     select('Batter', 'Outs', 'Balls', 'Strikes', 'TaggedHitType', 'PitchCall', 'PlayResult', 'Distance')
 })
  
  # Batter Standings Table ----
  output$BatterStandings = renderDataTable({
    BStand
    })
 
  # Batter Standings Totals ----
  output$BatterStandingsTotals = renderTable({
    BTotals
  })  
  

  # Batter Table ----
  output$BattingMetrics = renderTable({
    table = BattingDF() %>%
      filter(BatterTeam == 'MER_BEA') %>% 
      group_by('BT' = BatterTeam) %>%
      summarise(
        Pitches = length(TaggedPitchType),
        PA = length(which(Balls == 0 & Strikes == 0)),
        AB = length(which(!KorBB %in% c('Undefined','Walk') | !PlayResult %in% c('Undefined', 'Sacrifice'))),
        H = length(which(!PlayResult %in% c('Undefined', 'Out', 'Sacrifice'))),
        RBI = sum(RunsScored[PitchCall=='InPlay']),
        K = length(which(KorBB == 'Strikeout')),
        BB = length(which(KorBB == 'Walk')),
        "2B" = length(which(PlayResult == 'Double')),
        "3B" = length(which(PlayResult == 'Triple')),
        HR = length(which(PlayResult == 'HomeRun'))
      )%>%.[, c(2,3,4,5,6,7,8,9,10,11)]
  }, striped = TRUE, bordered = TRUE)
  
 
  # Batter Appears Table ----
  output$BatterAppears = renderDataTable({
    
    table = BattingDF() %>% 
      select(PitchNo, Inning, Balls, Strikes, Outs, Batter, TaggedPitchType, PitchCall, KorBB,
             TaggedHitType, PlayResult, ExitSpeed, Angle, Distance)
    
    colnames(table) = c('No','Inn','B','S','O', 'Batter','Pitch','Call','PA Result',
                        'Type','Play Result', 'EV', 'LA', 'Dist')
    
    table
  })
  
  # Field Spray ----
  output$SprayChart = renderPlot({
    brg = BattingDF()$Bearing * (pi/180)
    y = BattingDF()$Distance*cos(brg)
    x = BattingDF()$Distance*sin(brg)
    
    PlayResult = BattingDF()$PlayResult
    
    SprayPlot + geom_point(aes(x=x, y=y, color = PlayResult), BattingDF(), size = 2) +
      ##1B
      #geom_point(aes(x = 69, y = 88), shape = 15, size =5) +
      ##2B
      #geom_point(aes(x = 41, y = 145), shape = 15, size =5) +
      ##3B
      #geom_point(aes(x = -67, y = 87), shape = 15, size =5) +
      ##SS
      #geom_point(aes(x = -41, y = 141), shape = 15, size =5) +
      ##LF
      #geom_point(aes(x = -130, y = 243), shape = 15, size =5) +
      ##CF
      #geom_point(aes(x = -24, y = 296), shape = 15, size =5) +
      ##RF
      #geom_point(aes(x = 123, y = 264), shape = 15, size =5) +
      theme(legend.text = element_text(size = 12), legend.title = element_text(size=12)) +
      guides(color = guide_legend(override.aes = list(size=4)))
  })
 
  # Batter Loc Plot ----
  output$BatLoc = renderPlot({
    # PlateLocSide is negative because original data is from Pitcher POV so this makes it Hitter POV
    BatLocPlot + 
      geom_point(aes(x = -PlateLocSide, y = PlateLocHeight, color = TaggedPitchType), 
                            BattingDF(), size = 8) +
      geom_text(data = BattingDF(), 
                aes(label = PitchNo, x = -PlateLocSide, y = PlateLocHeight), 
                color = 'white', size = 4)
  })
  
  
  
  # Batter Contact Plot ----
  output$BatContact = renderPlot({
    
    BatContPlot + geom_point(aes( x = ContactPositionZ, y = ContactPositionX, 
                                 color = TaggedPitchType), BattingDF(), size = 4, na.rm = T)
    
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)

#profvis(runApp(shinyApp(ui = ui, server = server)))


