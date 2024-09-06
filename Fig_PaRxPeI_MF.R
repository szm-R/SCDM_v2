#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots the interaction effect of parental rules and
#              peer influence on the target agent's LGRD. The input comes from 
#              running the code Run_Experiment.R for all four design points 
#              for different setup numbers.

library(readxl)
library(xlsx)
library(ggplot2)
library(ggpubr)


Num_Runs = 100
PS_Idx_L = 32
PS_Idx_H = 17

FilePath = "Output/SimulationData/LGRDs_MultipleFU.xlsx"

# We aim to analyze the PaR x PeI interaction effect
i = 1
j = 2

#########################################################################
#########################################################################
######  Interaction effect for the case of peers with lower ILGPP  ######   
#########################################################################
#########################################################################

SheetName = paste("Setup", PS_Idx_L, sep = "")
Final_LGRDs = read_excel(FilePath, sheet = SheetName, 
                         col_names = FALSE,
                         .name_repair = "unique_quiet")

# Store the average response of each design point
AverageResponse = rowMeans(Final_LGRDs)


# Initializing the responses

# For j at negative level:
# i at negative level 
R_in_jn = 0
# i at positive level
R_ip_jn = 0

# For j at positive level:
# i at negative level
R_in_jp = 0
# i at positive level
R_ip_jp = 0


# Factor level representation
f1 = c(1,-1)
f2 = c(1,-1)
DM_Rep = expand.grid(f1, f2)

# Loop over design points
for (DP in 1:nrow(DM_Rep))
{
    if (DM_Rep[DP,j] == -1)
    {
        # Compute average responses when factor j is at its 
        # negative level
        
        if (DM_Rep[DP,i] == -1)
            R_in_jn = R_in_jn + AverageResponse[DP]
        else
            R_ip_jn = R_ip_jn + AverageResponse[DP]
    }
    else
    {
        # Compute average responses when factor j is at its 
        # positive level
        
        if (DM_Rep[DP,i] == -1) 
            R_in_jp = R_in_jp + AverageResponse[DP]
        else
            R_ip_jp = R_ip_jp + AverageResponse[DP]
    }
}

# Factor level values
f1 = c("Off", "On")
f2 = c("Off", "On")
FactorLevels = rbind(f1, f2)

Int_DF = as.data.frame(matrix(0, nrow = 4, ncol = 3))
colnames(Int_DF) = c("PaR", "LGRD", "PeI")

Int_DF$PaR[1] = FactorLevels[i,1]
Int_DF$PaR[2] = FactorLevels[i,2]
Int_DF$PaR[3] = FactorLevels[i,1]
Int_DF$PaR[4] = FactorLevels[i,2]

Int_DF$LGRD[1] = R_in_jn
Int_DF$LGRD[2] = R_ip_jn
Int_DF$LGRD[3] = R_in_jp
Int_DF$LGRD[4] = R_ip_jp

Int_DF$PeI[1] = FactorLevels[j,1]
Int_DF$PeI[2] = FactorLevels[j,1]
Int_DF$PeI[3] = FactorLevels[j,2]
Int_DF$PeI[4] = FactorLevels[j,2]


windowsFonts(Times = windowsFont("Times New Roman"))

Int1 = ggplot(Int_DF, aes(x = PaR, y = LGRD, group = PeI)) +
       geom_line(aes(color = PeI), linewidth = 1) +
       geom_point(aes(color = PeI)) +
       scale_x_discrete(limits = c("Off","On")) +
       scale_color_manual(values=c("#004D40", "#FFC107")) + 
       labs(y = "LGR difference", 
            x = "Parental rule",
            color = "Peer influence") +
       theme_light() +
       theme(text = element_text(size = 8,family = "Times"))


#########################################################################
#########################################################################
######  Interaction effect for the case of peers with higher ILGPP  #####   
#########################################################################
#########################################################################

SheetName = paste("Setup", PS_Idx_H, sep = "")
Final_LGRDs = read_excel(FilePath, sheet = SheetName, 
                         col_names = FALSE,
                         .name_repair = "unique_quiet")

# Store the average response of each design point
AverageResponse = rowMeans(Final_LGRDs)


# Initializing the responses

# For j at negative level:
# i at negative level 
R_in_jn = 0
# i at positive level
R_ip_jn = 0

# For j at positive level:
# i at negative level
R_in_jp = 0
# i at positive level
R_ip_jp = 0


# Factor level representation
f1 = c(1,-1)
f2 = c(1,-1)
DM_Rep = expand.grid(f1, f2)

# Loop over design points
for (DP in 1:nrow(DM_Rep))
{
    if (DM_Rep[DP,j] == -1)
    {
        # Compute average responses when factor j is at its 
        # negative level
        
        if (DM_Rep[DP,i] == -1)
            R_in_jn = R_in_jn + AverageResponse[DP]
        else
            R_ip_jn = R_ip_jn + AverageResponse[DP]
    }
    else
    {
        # Compute average responses when factor j is at its 
        # positive level
        
        if (DM_Rep[DP,i] == -1) 
            R_in_jp = R_in_jp + AverageResponse[DP]
        else
            R_ip_jp = R_ip_jp + AverageResponse[DP]
    }
}

# Factor level values
f1 = c("Off", "On")
f2 = c("Off", "On")
FactorLevels = rbind(f1, f2)

Int_DF = as.data.frame(matrix(0, nrow = 4, ncol = 3))
colnames(Int_DF) = c("PaR", "LGRD", "PeI")

Int_DF$PaR[1] = FactorLevels[i,1]
Int_DF$PaR[2] = FactorLevels[i,2]
Int_DF$PaR[3] = FactorLevels[i,1]
Int_DF$PaR[4] = FactorLevels[i,2]

Int_DF$LGRD[1] = R_in_jn
Int_DF$LGRD[2] = R_ip_jn
Int_DF$LGRD[3] = R_in_jp
Int_DF$LGRD[4] = R_ip_jp

Int_DF$PeI[1] = FactorLevels[j,1]
Int_DF$PeI[2] = FactorLevels[j,1]
Int_DF$PeI[3] = FactorLevels[j,2]
Int_DF$PeI[4] = FactorLevels[j,2]


windowsFonts(Times = windowsFont("Times New Roman"))

Int2 = ggplot(Int_DF, aes(x = PaR, y = LGRD, group = PeI)) +
       geom_line(aes(color = PeI), linewidth = 1) +
       geom_point(aes(color = PeI)) +
       scale_x_discrete(limits = c("Off","On")) +
       scale_color_manual(values=c("#004D40", "#FFC107")) + 
       labs(y = "LGR difference", 
            x = "Parental rule",
            color = "Peer influence") +
       theme_light() +
       theme(text = element_text(size = 8,family = "Times"))


#########################################################################
#########################################################################
################     Plot the interaction effects      ##################   
#########################################################################
#########################################################################

MultiPlot = ggarrange(Int1, Int2, 
                      labels = c("", ""), 
                      ncol = 2, nrow = 1, 
                      common.legend = TRUE, 
                      legend="right")

print(MultiPlot)

FileName = paste("fig-Int_ParPeI", PS_Idx_L, PS_Idx_H, sep = "_")
FigurePath = paste("Output/Figures", FileName, sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width = 7, height = 3.5, dpi = 1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), 
       width = 7, height = 3.5, dpi = 1000)

print(MultiPlot)









