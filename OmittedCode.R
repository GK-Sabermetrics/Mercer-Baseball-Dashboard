# Omitted Code that is helpful

library(tidyverse)

library(hms)

library(data.table)

library(pacman)

p_unload("all")

data = read.csv('data/2024 Full Season Data - Sheet1.csv')

nd = select(data, 1:6,8:10,12,13,15:37,39:50,54:57,69,70,76,77,83:85)




# Partly taken out Pitcher Standings Table ----
table = data%>%
  filter(PitcherTeam == "MER_BEA") %>%
  group_by('Pitcher' = Pitcher) %>%
  summarise('P' = n(),
            'G' = length(unique(Date)),
            'BF' = length(which(Balls == 0 & Strikes == 0)),
            #'SO' = length(which(KorBB == "Strikeout")),
            'KS' = length(which(PitchCall == 'StrikeSwinging' & KorBB == 'Strikeout')),
            'KC' = length(which(PitchCall == 'StrikeCalled' & KorBB == 'Strikeout')),
            'BB' = length(which(KorBB == "Walk")),
            'H' = length(which(PitchCall == 'InPlay' & PlayResult != 'Out')),
            'R' = sum(RunsScored),
            '1B' = length(which(PlayResult == 'Single')),
            '2B' = length(which(PlayResult == 'Double')),
            '3B' = length(which(PlayResult == 'Triple')),
            'HR' = length(which(PlayResult == "HomeRun")),
  )

#add a pipe %>% below the closing bracket to do add row normally
#   add_row(
#     Pitcher = "Total",
#     'P' = sum(.$P),
#     'G' = NULL,
#     'BF' = sum(.$BF),
#     #'SO' = sum(.$SO),
#     'KS' = sum(.$KS),
#     'KC' = sum(.$KC),
#     'BB' = sum(.$BB),
#     'H' = sum(.$H),
#     'R' = sum(.$R),
#     '1B' = sum(.$`1B`),
#     '2B' = sum(.$`2B`),
#     '3B' = sum(.$`3B`),
#     'HR' = sum(.$HR)
#    )

# LEFT OUT Pitcher Eval Table ----
#  output$PitcherEval = renderTable({
#    table = PitchingDF() %>%
#      filter(PitcherTeam == "MER_BEA") %>%
#      group_by('Pitcher' = Pitcher) %>%
#      summarise('P' = n(),
#                'FB' = length(which(TaggedPitchType %in% c('Fastball',
#                                                           'TwoSeamFastBall',
#                                                           'Cutter',
#                                                           'Sinker',
#                                                           'Splitter'))),
#                'CH' = length(which(TaggedPitchType == "ChangeUp")),
#                'CB' = length(which(TaggedPitchType == "Curveball")),
#                'SL' = length(which(TaggedPitchType == "Slider")),
#                'K' = length(which(PitchCall %in% c('StrikeCalled', 'StrikeSwinging'))),
#                'B' = length(which(PitchCall == 'BallCalled')),
#                '3PW%' = length(which(Strikes == 2 & Balls < 2))
#      )
#  }, striped = TRUE, bordered = TRUE)

# Old Pitch Location Plot
PitchLocPlot = 
  ggplot() +
  xlim(-3,3) + ylim(0,5) + 
  labs(color = "") +
  geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.83, y = 2.167, xend = 0.83, yend = 2.167), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.83, y = 2.833, xend = 0.83, yend = 2.833), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), linetype = 2, color = 'dark grey', linewidth = 1) +
  scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  theme_void() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 10), axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.background = element_blank(), 
        legend.key = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=4)))

# Old Heatmap Code ----

#tabPanel("Heat Map",
#         plotOutput(outputId = 'Heat',width = "80%",height = "600px")
#         ),

# Heat Map ----
#  output$Heat = renderPlot({
#    ggplot(data = PitchingDF(), aes(x = PlateLocSide, y = PlateLocHeight)) +
#      stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = F) +
#      scale_fill_gradientn(colours = c("blue", "white", "red")) +
#      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
#      geom_segment(aes(x = -0.83, y = 2.167, xend = 0.83, yend = 2.167), linetype = 2, color = 'dark grey', size = 1) +
#      geom_segment(aes(x = -0.83, y = 2.833, xend = 0.83, yend = 2.833), linetype = 2, color = 'dark grey', size = 1) +
#      geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), linetype = 2, color = 'dark grey', size = 1) +
#      geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), linetype = 2, color = 'dark grey', size = 1) +
#      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
#      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
#      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
#      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
#      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
#      ylim(0, 5) + xlim(-3, 3) +
#      theme(panel.background = element_blank()) +
#      theme(legend.position = "none", legend.text = element_text(size = 8), axis.title = element_blank()) +
#      theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.background = element_blank(), 
#            legend.key = element_blank()) +
#      guides(fill = FALSE) 
#   
#  })

# Plot Testing Space ----

data = read.csv("data/2024 Full Season Data - Sheet1.csv") %>% 
  filter(Pitcher == "Lambert, Garrett") %>% 
  filter(PitchCall == "InPlay")

ggplot(data, aes(x = HorzBreak, y = InducedVertBreak, color = ExitSpeed)) +
  labs(x = "Horizontal Movement (in)", y = "Vertical Movement (in)", color = " ", title = "Pitch Movement") + 
  xlim(-28, 28) + ylim(-28, 28) +
  geom_segment(aes(x = 0, y = -28, xend = 0, yend = 28), size = 1, color = "grey55") + 
  geom_segment(aes(x = -28, y = 0, xend = 28, yend = 0), size = 1, color = "grey55") +
  #scale_color_manual(values = cols, breaks = bks, labels = lbs) +
  geom_point() +
  theme_bw() +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10), 
    axis.title = element_text(size = 14)
  ) +
  guides(color = guide_legend(override.aes = list(size=4)))

# Ratio Work ----
library(MASS)
library(tidyverse)

data = read.csv("data/2024 Full Season Data - Sheet1.csv")

data = data %>% 
  filter(!is.na(InducedVertBreak),!is.na(InducedVertBreak)) %>% 
  mutate(
  ratio = abs(fractions(round(InducedVertBreak)/round(HorzBreak))) %>% as.character()
  , .after = HorzBreak
)

td =
filter(data, 
       !is.na(InducedVertBreak),
       !is.na(InducedVertBreak))

round(data$InducedVertBreak)/round(data$HorzBreak) %>% fractions(max.denominator = 4)

round(data$InducedVertBreak, 1)

# Working on Stuff ----

head(data,100) %>% filter(PitchCall %in% c("BallCalled", "StrikeCalled")) %>% 
  ggplot(aes(x=PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
  ylim(0,5) +
  scale_x_continuous(breaks = c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3),limits = c(-3,3)) +
  labs(color = "", title = "Overall") +
  geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), 
               linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 2.167, xend = 0.708, yend = 2.167), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.708, y = 2.833, xend = 0.708, yend = 2.833), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -1.2, y = 0, xend = -1.2, yend = 0.5), linewidth = 1, color = 'black') +
  geom_segment(aes(x = -1.2, y = 0, xend = -3, yend = 0), linewidth = 1, color = 'black') +
  geom_segment(aes(x = 1.2, y = 0, xend = 1.2, yend = 0.5), linewidth = 1, color = 'black') +
  geom_segment(aes(x = 1.2, y = 0, xend = 3, yend = 0), linewidth = 1, color = 'black') +
  geom_point(size = 2.86) +
  scale_color_manual(values = list("StrikeCalled" = "red3", "BallCalled" = "blue3")) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 10), axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), legend.background = element_blank(), 
        legend.key = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=4)))

# From Pitcher Perspective ----
head(data) %>% filter(PitchCall %in% c("BallCalled", "StrikeCalled")) %>% 
  ggplot(aes(x=PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
  ylim(0,5) +
  scale_x_continuous(breaks = c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3),limits = c(-3,3)) +
  labs(color = "", title = "Overall") +
  geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), 
               linewidth = 1, color = "black") + 
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), 
               linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 2.167, xend = 0.708, yend = 2.167), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.708, y = 2.833, xend = 0.708, yend = 2.833), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), 
               linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_rect(aes(xmin = -3, xmax = -1.2, ymin = 0, ymax = 0.5), linewidth = 1, color = "black") +
  geom_rect(aes(xmin = 1.2, xmax = 3, ymin = 0, ymax = 0.5), linewidth = 1, color = "black") +
  geom_point(size = 2.86) +
  scale_color_manual(values = list("StrikeCalled" = "red3", "BallCalled" = "blue3")) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 10), axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), legend.background = element_blank(), 
        legend.key = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=4)))

# From Hitter Perspective
head(data) %>% filter(PitchCall %in% c("BallCalled", "StrikeCalled")) %>% 
  ggplot(aes(x=PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
  ylim(0,5) +
  scale_x_continuous(breaks = c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3),limits = c(-3,3)) +
  labs(color = "", title = "Overall") +
  geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
  #geom_segment(aes(x = 0.708, y = 0.45, xend = 0.708, yend = 0.3), linewidth = 1, color = "black") +
  #geom_segment(aes(x = -0.708, y = 0.45, xend = -0.708, yend = 0.3), linewidth = 1, color = "black") +
  #geom_segment(aes(x = 0, y = 0, xend = 0.708, yend = 0.3), linewidth = 1, color = "black") +
  #geom_segment(aes(x = 0, y = 0, xend = -0.708, yend = 0.3), linewidth = 1, color = "black") +
  #geom_segment(aes(x = -0.708, y = 0.45, xend = 0.708, yend = 0.45), linewidth = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 2.167, xend = 0.708, yend = 2.167), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.708, y = 2.833, xend = 0.708, yend = 2.833), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = -0.277, y = 1.5, xend = -0.277, yend = 3.5), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_segment(aes(x = 0.276, y = 1.5, xend = 0.276, yend = 3.5), linetype = 2, color = 'dark grey', linewidth = 1) +
  geom_rect(aes(xmin = -3, xmax = -1.2, ymin = 0, ymax = 0.5), linewidth = 1, color = "black") +
  geom_rect(aes(xmin = 1.2, xmax = 3, ymin = 0, ymax = 0.5), linewidth = 1, color = "black") +
  geom_point(size = 2.86) +
  scale_color_manual(values = list("StrikeCalled" = "red3", "BallCalled" = "blue3")) +
  theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  theme(legend.position = 'bottom', legend.text = element_text(size = 10), axis.title = element_blank()) +
  theme(axis.ticks = element_blank(), legend.background = element_blank(), 
        legend.key = element_blank()) +
  guides(color = guide_legend(override.aes = list(size=4)))

data = read.csv("data/Fall Scrimmage Data.csv")

ps =
data %>% 
  #filter(Date == "9/28/24") %>% 
  group_by(Pitcher) %>% 
  summarise(
    #'Innings' = length(unique(Inning)),
    #'BF' = length(which(Balls == 0 & Strikes == 0)),
    'Strikes' = length(which(PitchCall != 'BallCalled')),
    'Pitches' = n(),
    #'FPS' = length(which(PitchCall == 'StrikeCalled' | PitchCall == "StrikeSwinging" | PitchCall == "FoulBallNotFieldable" & Balls == 0 & Strikes == 0)),
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

library(tidyverse)


data %>% filter(!TaggedPitchType %in% c('FourSeamFastBall', 'Undefined')) %>% 
ggplot(aes(x = RelSpeed, y = SpinRate, color = TaggedPitchType)) +
  geom_point() +
  labs(title = "Spin Rate vs Velocity of Pitch Types", 
       x = "Velocity", 
       y = "Spin Rate (RPM)") +
  theme_minimal()

data %>% filter(!TaggedPitchType %in% c('FourSeamFastBall', 'Undefined')) %>% 
  ggplot(aes(x = TaggedPitchType, y = SpinRate)) +
  geom_boxplot() +
  labs(title = "Spin Rate per Pitch Type", 
       x = "Pitch Type", 
       y = "Spin Rate (RPM)") +
  theme_minimal()


