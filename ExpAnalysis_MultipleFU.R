#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code analyzes the results of running the code 
#              Run_Experiment.R for all four design points for different 
#              setup numbers.

library(readxl)
library(xlsx)

OutputDir = "Output/SimulationData"
OutInSuffix = "MultipleFU.xlsx"

Num_Runs = 100
Setup_Idx = c(42,48,54,5,11,22,28,34)

Parental_Rules = c(TRUE, FALSE)
Peer_Influence = c(TRUE, FALSE)

# Building the design matrix
DM_Value = expand.grid(Parental_Rules, Peer_Influence)
colnames(DM_Value) = c("PaR", "PeI")

for (PS_Idx in Setup_Idx)
{
    
    SheetName = paste("Setup", PS_Idx, sep = "")
    FileName = paste("LGRDs", OutInSuffix, sep = "_")
    FilePath = paste(OutputDir, FileName, sep = "/")
    Final_LGRDs = read_excel(FilePath, sheet = SheetName, 
                             col_names = FALSE, 
                             .name_repair = "unique_quiet")
    
    ##########################################################################
    #############                                                 ############   
    #############  Calculating and Saving the Response Summery    ############
    #############                                                 ############   
    ##########################################################################
    
    Summary_Variables = c("PaR", "PeI", "Mean", "StD", "LB", 
                          "UB", "min", "max", "L0", "L0.1", "L0.2", "L0.3", 
                          "L0.4", "L0.5", "G0.5")
    RS = as.data.frame(matrix(0, nrow = nrow(DM_Value), 
                              ncol = length(Summary_Variables)))
    colnames(RS) = Summary_Variables
    
    
    alpha = 0.05
    degrees_of_freedom = Num_Runs
    t_score = qt(p = alpha/2, df = degrees_of_freedom,lower.tail = F)
    
    for (DP in 1:nrow(DM_Value))
    {
        Mean = mean(as.numeric(Final_LGRDs[DP,]))
        SD = sd(as.numeric(Final_LGRDs[DP,]))
        SE = SD/sqrt(Num_Runs)
        
        Margin_of_Error = t_score*SE
        Lower_Bound = Mean - Margin_of_Error
        Upper_Bound = Mean + Margin_of_Error
        
        RS$PaR[DP] = DM_Value$PaR[DP]
        RS$PeI[DP] = DM_Value$PeI[DP]
        RS$Mean[DP] = Mean
        RS$StD[DP] = SD
        RS$LB[DP] = Lower_Bound
        RS$UB[DP] = Upper_Bound
        RS$min[DP] = min(Final_LGRDs[DP,])
        RS$max[DP] = max(Final_LGRDs[DP,])
        
        bin = c(-0.5, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
        LGRD_hist = hist(as.numeric(Final_LGRDs[DP,]), 
                         breaks = c(bin,Inf), plot = FALSE)
        RS[DP,9:15] = LGRD_hist$counts
    }
    
    SheetName = paste("Setup", PS_Idx, sep = "")
    FileName = paste("RS", OutInSuffix, sep = "_")
    FilePath = paste(OutputDir, FileName, sep = "/")
    write.xlsx(RS, file = FilePath, sheetName = SheetName,
               append = TRUE, row.names = FALSE)
    
    
    ##########################################################################
    #############                                                 ############   
    #############    Calculating and Saving the Effect Matrix     ############
    #############                                                 ############   
    ##########################################################################
    
    # Factor level representation
    f1 = c(1,-1)
    f2 = c(1,-1)
    DM_Rep = expand.grid(f1, f2)
    
    Effect_Names = c("PaR", "PeI", "PaRxPeI")
    
    Run_EffectMatrix = matrix(0, nrow = length(Effect_Names), 
                              ncol = Num_Runs)
    
    # Calculate the main effect of each factor for all runs, for the details
    # of these equations refer to the Systematic_Experiment.html file
    for (Run_Idx in 1:Num_Runs)
    {
        for (F_Idx in 1:ncol(DM_Rep))
        {
            Effect = 0
            for (DP in 1:nrow(DM_Rep))
            {
                ResponseSign = DM_Rep[DP, F_Idx]
                ResponseValue = as.numeric(Final_LGRDs[DP, Run_Idx]) 
                Effect = Effect + (ResponseSign*ResponseValue)
            }
            Run_EffectMatrix[F_Idx, Run_Idx] = Effect/2
        }
    }
    
    
    # Calculate the interaction effect of factors for all runs, for the details
    # of these equations refer to the Systematic_Experiment.html file
    for (Run_Idx in 1:Num_Runs)
    {
        Counter = ncol(DM_Rep)
        for (i in 1:ncol(DM_Rep))
        {
            for (j in 2:ncol(DM_Rep))
            {
                if (j <= i)
                    next
                
                Effect = 0
                for (DP in 1:nrow(DM_Rep))
                {
                    ResponseSign = DM_Rep[DP, i]*DM_Rep[DP, j]
                    ResponseValue = as.numeric(Final_LGRDs[DP, Run_Idx]) 
                    Effect = Effect + (ResponseSign*ResponseValue)
                }
                Counter = Counter + 1
                Run_EffectMatrix[Counter, Run_Idx] = 0.5*(Effect)
            }
        }
    }
    
    # We have 3 effect in total (2 main effects and 1 interactions), therefore, 
    # to have at least an overall 90% confidence level, we need to consider a 
    # confidence level of  96.67% for each of our 3 effects. In other words, 
    # we would be using alpha = 0.033
    alpha = 0.033
    degrees_of_freedom = Num_Runs
    t_score = qt(p = alpha/2, df = degrees_of_freedom,lower.tail = F)
    
    Overall_EM = as.data.frame(matrix(0, nrow = length(Effect_Names), 
                                      ncol = 4))
    colnames(Overall_EM) = c("Factor", "Mean", "LB", "UB")
    
    for (i in 1:length(Effect_Names))
    {
        Mean = mean(Run_EffectMatrix[i,])
        SD = sd(Run_EffectMatrix[i,])
        SE = SD/sqrt(Num_Runs)
        
        Margin_of_Error = t_score*SE
        Lower_Bound = Mean - Margin_of_Error
        Upper_Bound = Mean + Margin_of_Error
        
        Overall_EM[i,1] = Effect_Names[i]
        Overall_EM[i,2] = Mean
        Overall_EM[i,3] = Lower_Bound
        Overall_EM[i,4] = Upper_Bound
    }
    
    SheetName = paste("Setup", PS_Idx, sep = "")
    FileName = paste("EM", OutInSuffix, sep = "_")
    FilePath = paste(OutputDir, FileName, sep = "/")
    write.xlsx(Overall_EM, file = FilePath, sheetName = SheetName,
               append = TRUE, row.names = FALSE)
}















