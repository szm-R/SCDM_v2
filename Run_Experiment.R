#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code runs experiments with varying sets of parameter setups
#              for multiple family units.


# Clear the environment
rm(list=ls())

library(readxl)
library(xlsx)

debugSource("Initialize.R")
debugSource("Run_Simulation.R")
debugSource("Record_Response.R")
debugSource("Experiment_Setup.R")
debugSource("Record_StateSpace.R")


debugSource("Select_Goal.R")
debugSource("Execute_Goal.R")
debugSource("Choose_Next_SSP.R")
debugSource("Update_PeerGoals.R")
debugSource("Update_GoalHistory.R")
debugSource("Update_StateVariables.R")
debugSource("Determine_Peer_Preference.R")
debugSource("Determine_Parental_Expectation.R")

OutputDir = "Output/SimulationData"

Save_TS = FALSE
Save_SS = FALSE
SaveOutputs = TRUE

# Specify the setups based on the file Setup.xlsx
# The code retrieves the row indices specified here and
# sets the model configuration accordingly
Setup_Idx = c(22,28,34)
OutputName = "LGRDs_MultipleFU.xlsx"
# OutputName = "LGRDs_SingleFU.xlsx"

# We save state space snapshots at the beginning, in the middle, 
# and at the end of the simulation steps
Snapshot_Steps = c(96, 840, 1680)

TA_ILGPP = 0.2 # Target agent's initial LGPP
PA_ILGPP = c(0.1, 0.5) # Peer agents' lower and higher initial LGPP


# Numbers to run each model configuration
Num_Runs = 100

# Set the factor levels
Parental_Rules = c(TRUE, FALSE)
Peer_Influence = c(TRUE, FALSE)

# Build the design matrix
DM_Value = expand.grid(Parental_Rules, Peer_Influence)
colnames(DM_Value) = c("PaR", "PeI")

# Loop over setup indices
for (PS_Idx in Setup_Idx)
{
    print(paste("Running for setup number", PS_Idx))
    
    # Data frame for storing final LGRD values of 
    # the target agent
    FL_TA = data.frame()
    
    # Loop over design points
    for (DP in 1:4)
    {
        print(paste("   Design point:", DP))
        
        # Read the specified row from the file Setup.xlsx
        # and set the model config matrix
        Experiment_Setup()
        
        # Temporary matrix for storing final LGRD values of 
        # the target agent
        Final_LGRDs_TA = matrix(0, nrow = 1, ncol = Num_Runs)

        # Repeat each design point for Num_Runs
        for (Run_Idx in 1:Num_Runs)
        {
            # print(paste("Run idx:", Run_Idx))
            set.seed(Run_Idx)
            Initialize()
            Run_Simulation()
            Record_Response()
        }

        FL_TA = rbind(FL_TA, Final_LGRDs_TA)
    }

    if (SaveOutputs)
    {
        SheetName = paste("Setup", PS_Idx, sep = "")
        FilePath = paste(OutputDir, OutputName, sep = "/")
        write.xlsx(FL_TA, file = FilePath, sheetName = SheetName,
                   append = TRUE, row.names = FALSE, col.names = FALSE)
    }
}










