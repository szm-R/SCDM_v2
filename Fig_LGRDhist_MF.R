#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots a histogram of LGRDs of cases with and without
#              peer influence. The input comes from running the code 
#              Run_Experiment.R for all four design points for different 
#              setup numbers.


library(readxl)
library(xlsx)
library(ggplot2)
library(ggpubr)

DP_PaRWithPeI = 1
DP_PaRWithoutPeI = 3


PS_Idx_L = 32
PS_Idx_H = 17

FilePath = "Output/SimulationData/LGRDs_MultipleFU.xlsx"

##########################################################################
##########################################################################
##########################################################################
##########################################################################

SheetName = paste("Setup", PS_Idx_L, sep = "")
Final_LGRDs = read_excel(FilePath, sheet = SheetName, 
                         col_names = FALSE,
                         .name_repair = "unique_quiet")

Input = as.data.frame(matrix(0, nrow = 200, ncol = 2))
colnames(Input) = c("LGRD", "PeI")

Input$LGRD[1:100] = Final_LGRDs[DP_PaRWithPeI,]
Input$PeI[1:100] = "On"
Input$LGRD[101:200] = Final_LGRDs[DP_PaRWithoutPeI,]
Input$PeI[101:200] = "Off"

Input$LGRD = as.numeric(Input$LGRD)

windowsFonts(Times = windowsFont("Times New Roman"))

LGRD_hist1 = ggplot(Input, aes(x=LGRD, color = PeI, fill=PeI)) +
             geom_histogram(alpha=0.65, position="identity", binwidth=0.1) +
             scale_color_manual(values=c("#004D40", "#FFC107")) + 
             scale_fill_manual(values=c("#004D40", "#FFC107")) +
             labs(y = "Count", 
                  x = "LGR difference",
                  fill="Peer influence", 
                  color="Peer influence") +
             theme_light() +
             theme(text=element_text(size=8,family="Times"))


##########################################################################
##########################################################################
##########################################################################
##########################################################################

SheetName = paste("Setup", PS_Idx_H, sep = "")
Final_LGRDs = read_excel(FilePath, sheet = SheetName, 
                         col_names = FALSE,
                         .name_repair = "unique_quiet")

Input = as.data.frame(matrix(0, nrow = 200, ncol = 2))
colnames(Input) = c("LGRD", "PeI")

Input$LGRD[1:100] = Final_LGRDs[DP_PaRWithPeI,]
Input$PeI[1:100] = "On"
Input$LGRD[101:200] = Final_LGRDs[DP_PaRWithoutPeI,]
Input$PeI[101:200] = "Off"

Input$LGRD = as.numeric(Input$LGRD)

windowsFonts(Times = windowsFont("Times New Roman"))

LGRD_hist2 = ggplot(Input, aes(x=LGRD, color = PeI, fill=PeI)) +
             geom_histogram(alpha=0.65, position="identity", binwidth=0.1) +
             scale_color_manual(values=c("#004D40", "#FFC107")) + 
             scale_fill_manual(values=c("#004D40", "#FFC107")) +
             labs(y = "Count", 
                  x = "LGR difference",
                  fill="Peer influence", 
                  color="Peer influence") +
             theme_light() +
             theme(text=element_text(size=8,family="Times"))


##########################################################################
##########################################################################
##########################################################################
##########################################################################

MultiPlot = ggarrange(LGRD_hist1, LGRD_hist2, 
                      labels = c("", ""), 
                      ncol = 2, nrow = 1, 
                      common.legend = TRUE, 
                      legend="right")

print(MultiPlot)

FileName = paste("fig-LGRDhist", PS_Idx_L, PS_Idx_H, sep = "_")
FigurePath = paste("Output/Figures", FileName, sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width = 7.5, height = 4.5, dpi = 1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), 
       width = 7.5, height = 4.5, dpi = 1000)
















