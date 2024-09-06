#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel updates the agent's state space and LGPP.

Update_StateVariables <- function(step, Agent_Idx, 
                                  Selected_Goal, 
                                  Executed_Goal)
{
    ## Update the state space
    
    SSP_x = SS_Position[Agent_Idx,1]
    SSP_y = SS_Position[Agent_Idx,2]
    
    # The depth of the new potential well equals the 
    # perceived autonomy
    PW_Depth = Perceived_Autonomy[Agent_Idx]
    
    # Form a 3D parabola centered at the current SSP, with only negative values, 
    # a depth equal to the perceived autonomy, and an approximate diameter 
    # of 15 units at the surface (the number is arbitrary)
    x = seq(0, State_Range, length=State_Range)
    y = seq(0, State_Range, length=State_Range)
    New_PW = outer(x, y, function(x, y) ((x-SSP_x)^2 + (y-SSP_y)^2)/36 - PW_Depth)
    New_PW[New_PW > 0] = 0
    
    # Add the new potential well to the Hypothetical Potential Function (HPF) 
    # of the agent's state space
    HPF[[Agent_Idx]] <<- HPF[[Agent_Idx]] + New_PW
    
    
    ## Update the Long-term Goal Preference Probability (LGPP)
    
    # We update the LGPP daily, i.e., every 24 simulation steps.
    SG_List = Agent_Goal_Hist[[Agent_Idx]]$SG[1:step]
    if (step %% Day_Steps == 1 && step > 1)
    {
        New_LGPP = length(which(SG_List == LG_Idx))/step
        if (New_LGPP > 0)
            LGPP[Agent_Idx] <<- New_LGPP
    }
}












