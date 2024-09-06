#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function records the results.

Record_Response <- function()
{
    Init_LGPP = Model_Config$ILGPP[1]
    Goal_History = Agent_Goal_Hist[[1]]
    
    
    ## Save the final outcome
    
    # The difference between the LG ratio of the Response Steps and the 
    # initial LGPP serves as the final outcome of the model, denoting the 
    # change in the agent's self-control score
    
    # Number of executed LG-directed actions after lifting the parental rule
    EG_List = Goal_History$EG[SimDuration+1:Response_Steps]
    Final_LGR = length(which(EG_List == LG_Idx))/Response_Steps
    
    Final_LGRDs_TA[1,Run_Idx] <<- Final_LGR - Init_LGPP
    
    
    # Save the time series of executed goals
    
    if (Save_TS)
    {
        StepsPerDay = Day_Steps
        NumDays = SimDuration/StepsPerDay
        
        ## Save the time series of this subject, i.e., run
        
        CN = c("Subject", "Day", "LGRD")
        LDT <<- as.data.frame(matrix(0, nrow = NumDays, 
                                     ncol = length(CN)))
        colnames(LDT) <<- CN
        
        for (i in 1:NumDays)
        {
            LDT$Subject <<- Run_Idx
            LDT$Day[i] <<- i
            
            SI = (i-1)*StepsPerDay + 1
            EI = i*StepsPerDay
            
            SG_List = Goal_History$SG[SI:EI]
            LGR = length(which(SG_List == LG_Idx))/StepsPerDay
            LDT$LGRD[i] <<- LGR - Init_LGPP
        }
        
        SetupNum = paste("Setup", PS_Idx, sep = "")    
        DP_Num = paste("DP", DP, sep = "=")
        FileName1 = paste("TS", SetupNum, DP_Num, sep = "_")
        FileName2 = paste(FileName1, "csv", sep = ".")
        FilePath = paste(OutputDir, FileName2, sep = "/")
        write.table(LDT, file = FilePath, col.names=!file.exists(FilePath),
                    sep = ",", append = TRUE, row.names = FALSE)
    }
}








