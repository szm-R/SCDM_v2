#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots LGRD-Days time series. The input comes from 
#              running the code Run_Experiment.R for all four design points 
#              for different setup numbers.


library(readxl)
library(xlsx)
library(ggplot2)
library(ggpubr)

set.seed(10)
PS_Idx = 32

DP_PaRWithPeI = 1
DP_PaRWithoutPeI = 3

##########################################################################
##########################################################################
##########################################################################
##########################################################################

SetupName = paste("TS_Setup", PS_Idx, sep = "")
FileName1 = paste(paste(SetupName, "DP", sep = "_"), 
                  DP_PaRWithoutPeI, sep = "=")
FileName2 = paste(FileName1, "csv", sep = ".")
FilePath = paste("Output/SimulationData", FileName2, sep = "/")

TimeSeries = read.csv(FilePath, header = TRUE)

# Choose randomly from the "subjects"
SubID = sample(1:100, 6, replace = FALSE)

RandomTS = subset(TimeSeries, Subject %in% SubID)

TS1 = ggplot(RandomTS, aes(x = Day, y = LGRD)) +
      geom_point(size = 0.5, aes(color = as.factor(Subject))) +
      geom_smooth(aes(color = as.factor(Subject)), 
                  method = lm, se = F, linewidth = 1) +
      scale_color_manual(values = c("#FFC107", "#E2B40D", "#A8991A",
                                    "#6D7E28", "#3E6932", "#004D40")) +
      labs(y = "LGR difference", 
           x = "Days") + 
      theme_light() +
      theme(legend.position = "none") +
      theme(text = element_text(size = 8,family = "Times"))


##########################################################################
##########################################################################
##########################################################################
##########################################################################

SetupName = paste("TS_Setup", PS_Idx, sep = "")
FileName1 = paste(paste(SetupName, "DP", sep = "_"), 
                  DP_PaRWithPeI, sep = "=")
FileName2 = paste(FileName1, "csv", sep = ".")
FilePath = paste("Output/SimulationData", FileName2, sep = "/")

TimeSeries = read.csv(FilePath, header = TRUE)

# Choose randomly from the "subjects"
SubID = sample(1:100, 6, replace = FALSE)

RandomTS = subset(TimeSeries, Subject %in% SubID)

TS2 = ggplot(RandomTS, aes(x = Day, y = LGRD)) +
      geom_point(size = 0.5, aes(color = as.factor(Subject))) +
      geom_smooth(aes(color = as.factor(Subject)), 
                  method = lm, se = F, linewidth = 1) +
      scale_color_manual(values = c("#FFC107", "#E2B40D", "#A8991A",
                                    "#6D7E28", "#3E6932", "#004D40")) +
      labs(y = "LGR difference", 
           x = "Days") + 
      theme_light() +
      theme(legend.position = "none") +
      theme(text = element_text(size = 8,family = "Times"))


##########################################################################
##########################################################################
##########################################################################
##########################################################################

MultiPlot = ggarrange(TS1, TS2, labels = c("", ""), 
                      ncol = 1, nrow = 2)

print(MultiPlot)

FileName = paste("fig-TS_WithoutWithPeI", PS_Idx, sep = "_")
FigurePath = paste("Output/Figures", FileName, sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width = 5, height = 5, dpi = 1000)

ggsave(file = paste(FigurePath, "jpeg", sep = "."), 
       width = 5, height = 5, dpi = 1000)













