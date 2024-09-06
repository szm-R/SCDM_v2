#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code analyzes the results of running the Run_Experiment.R
#              code for design point 3 (the parent only case) for setup numbers 
#              1, 19, and 37, indicating Strict, Moderate, and lax parenting 
#              styles for the target family unit.

library(readxl)
library(xlsx)

OutputDir = "Output/SimulationData"

Num_Runs = 100
PS = c("Strict", "Moderate", "Lax")

Summary_Variables = c("PS", "ILGPP", "Mean", "StD", "LB", 
                      "UB", "min", "max", "L0", "L0.1", "L0.2", "L0.3", 
                      "L0.4", "L0.5", "G0.5")
RS = as.data.frame(matrix(0, nrow = 12, 
                          ncol = length(Summary_Variables)))
colnames(RS) = Summary_Variables

RC = 1
for (LGPP in c(0.1, 0.2, 0.3, 0.4))
{
    
    SheetName = paste("LGPP", LGPP, sep = "=")
    FilePath = paste(OutputDir, "LGRDs_SingleFU.xlsx", sep = "/")
    Final_LGRDs = read_excel(FilePath, sheet = SheetName, 
                             col_names = FALSE, 
                             .name_repair = "unique_quiet")
    

    alpha = 0.05
    degrees_of_freedom = Num_Runs
    t_score = qt(p = alpha/2, df = degrees_of_freedom,lower.tail = F)
    
    for (DP in 1:3)
    {
        Mean = mean(as.numeric(Final_LGRDs[DP,]))
        SD = sd(as.numeric(Final_LGRDs[DP,]))
        SE = SD/sqrt(Num_Runs)
        
        Margin_of_Error = t_score*SE
        Lower_Bound = Mean - Margin_of_Error
        Upper_Bound = Mean + Margin_of_Error
        
        RS$PS[RC] = PS[DP]
        RS$ILGPP[RC] = LGPP
        RS$Mean[RC] = Mean
        RS$StD[RC] = SD
        RS$LB[RC] = Lower_Bound
        RS$UB[RC] = Upper_Bound
        RS$min[RC] = min(Final_LGRDs[DP,])
        RS$max[RC] = max(Final_LGRDs[DP,])
        
        bin = c(-0.5, 0, 0.1, 0.2, 0.3, 0.4, 0.5)
        LGRD_hist = hist(as.numeric(Final_LGRDs[DP,]), 
                         breaks = c(bin,Inf), plot = FALSE)
        RS[RC,9:15] = LGRD_hist$counts
        
        RC = RC + 1
    }
    
}

FilePath = paste(OutputDir, "RS_SingleFU.xlsx", sep = "/")
write.xlsx(RS, file = FilePath, append = TRUE, 
           row.names = FALSE)















