#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function records snapshots of the agent's state space

Record_StateSpace <- function(step)
{
    SS_Snapshot = HPF[[1]] 
    
    SheetName = paste("Step", step, sep = "")
    FileName1 = paste("SS_Snapshots", PS_Idx, SubID, sep = "_")
    FileName2 = paste(FileName1, "xlsx", sep = ".")
    FilePath = paste(OutputDir, FileName2, sep = "/")
    write.xlsx(SS_Snapshot, file = FilePath, sheetName = SheetName,
               append = TRUE, row.names = FALSE, col.names = FALSE)
}