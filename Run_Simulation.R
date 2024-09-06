#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function runs the main body of the simulation and the 
#              the response steps.


Run_Simulation <- function()
{
    # The main body of the simulation
    for (step in 1:SimDuration) 
    {
        # print(paste("step:", step))
        
        if (step == 1680)
        {
            Seriously = 1
            stop_here = 1
        }
        
        # Repeat the model processes for each adolescent agent
        for (Agent_Idx in 1:Num_Agents)
        {
            # print(paste("Agent idx:", Agent_Idx))
            
            
            Selected_Goal = Select_Goal(Agent_Idx)
            
            Peer_Preference = Determine_Peer_Preference(Agent_Idx)
            
            Parental_Expectation = Determine_Parental_Expectation(step,
                                                                  Agent_Idx)
            
            # print("EG function")
            Executed_Goal = Execute_Goal(Agent_Idx,  
                                         Selected_Goal,
                                         Peer_Preference,
                                         Parental_Expectation)
            
            Update_StateVariables(step, Agent_Idx,
                                  Selected_Goal,
                                  Executed_Goal)
            
            Choose_Next_SSP(Agent_Idx)
            
            Update_GoalHistory(Agent_Idx,  
                               Selected_Goal,
                               Executed_Goal,
                               Peer_Preference,
                               Parental_Expectation)
        }
        
        Update_PeerGoals()
        
        if (Save_SS && (step %in% Snapshot_Steps))
            Record_StateSpace(step)
    }
    
    # Loop over the response steps
    RS = Response_Steps
    for (Agent_Idx in 1:Num_Agents)
    {
        # Update LGPP to the overall ratio achieved during the simulation
        SG_List = Agent_Goal_Hist[[Agent_Idx]]$SG[1:SimDuration]
        LGPP[Agent_Idx] <<- length(which(SG_List == LG_Idx))/SimDuration

        for (step in SimDuration+1:RS)
        {
            Selected_Goal = Select_Goal(Agent_Idx)

            # There are no social expectations in the response steps
            Executed_Goal = Selected_Goal

            Choose_Next_SSP(Agent_Idx)

            Peer_Preference = 0

            Parental_Expectation = "None"

            Update_GoalHistory(Agent_Idx,
                               Selected_Goal,
                               Executed_Goal,
                               Peer_Preference,
                               Parental_Expectation)
        }
    }
}






