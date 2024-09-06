#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel finalizes the goal selection process based
#              on social expectations.

Execute_Goal <- function(Agent_Idx,  
                         Selected_Goal,
                         Peer_Preference,
                         Parental_Expectation)
{
    # Set the Compliance Likelihood (CL) to the
    # parental Rule (R)
    CL = Model_Config$R[Agent_Idx]
    
    # Set the Rule-based Perceived Autonomy (RPA) to
    # the parental Rule (R)
    RPA = Model_Config$R[Agent_Idx]
    
    # The PoC (Probability of Change) indicates how likely
    # the adolescent agent is to change its initially selected
    # goal due to social expectations
    PoC <<- 0 
    
    # By default, the agent executes the selected goal. In such a
    # case the perceived autonomy is at its maximum and equals to 1
    Executed_Goal = Selected_Goal
    Perceived_Autonomy[Agent_Idx] <<- 1
    
    
    PP = Peer_Preference
    PE = Parental_Expectation
    
    # The IP (Imitation Probability) denotes the agent's 
    # susceptibility to peer influence
    IP = Model_Config$IP[Agent_Idx] 
    
    SSP_x = SS_Position[Agent_Idx,1]
    SSP_y = SS_Position[Agent_Idx,2]
    
    # Minimum and maximum Jump Length (JL) or the distance
    # the agent can move through the state space
    Max_JL = State_Range  - 9
    Min_JL = 1
    
    PA = 1 # perceived autonomy
    if (Selected_Goal == SG_Idx)
    {
        # In case of a change, the agent should move vertically downward 
        # to the lower half of the state space, just below the x = y line
        Jump_Length <<- SSP_y - SSP_x + 1
        
        # Normalize the jump length for use in the PoC equation
        JL = (Jump_Length - Min_JL)/(Max_JL - Min_JL)
        if (JL > 1)
            JL = 1
        if (JL < 0)
            JL = 0
        
        if (PP == LG_Idx && PE == "None")
        {
            PoC <<- IP*(1 - JL) 
            PA = 1 - IP
        }
        else if (PP == LG_Idx && PE == "LG")
        {
            PoC <<- (CL + IP - CL*IP)*(1 - JL)
            PA = RPA*(1 - IP)
        }
        else if (PP == SG_Idx && PE == "LG")
        {
            PoC <<- CL*(1 - IP)*(1 - JL)
            PA = RPA
        }
        else if (PE == "LG")
        {
            PoC <<- CL*(1 - JL)
            PA = RPA
        }
        
        Executed_Goal = sample(c(SG_Idx, LG_Idx), size = 1,  
                                 prob = c((1-PoC), PoC))
        
        # Alter the state space position and perceived autonomy
        # in case of change in the executed goal
        if (Executed_Goal == LG_Idx)
        {
            SS_Position[Agent_Idx,2] <<- SSP_x - 1
            Perceived_Autonomy[Agent_Idx] <<- PA
        }
    }
    else
    {
        # In case of a change, the agent should move vertically upward 
        # to the upper half of the state space, just above the x = y line
        Jump_Length <<- SSP_x - SSP_y + 1
        
        # Normalize the jump length for use in the PoC equation
        JL = (Jump_Length - Min_JL)/(Max_JL - Min_JL)
        if (JL > 1)
            JL = 1
        if (JL < 0)
            JL = 0

        if (PP == SG_Idx && PE == "None")
        {
            PoC <<- IP*(1 - JL)
            PA = 1 - IP
        }
        else if (PP == SG_Idx && PE == "LG")
        {
            PoC <<- IP*(1 - CL)*(1 - JL)
            PA = 1 - IP
        }

        Executed_Goal = sample(c(LG_Idx, SG_Idx), size = 1,
                                 prob = c((1-PoC), PoC))

        # Alter the state space position and perceived autonomy
        # in case of change in the executed goal
        if (Executed_Goal == SG_Idx)
        {
            SS_Position[Agent_Idx,2] <<- SSP_x + 1
            Perceived_Autonomy[Agent_Idx] <<- PA
        }
    }
  
    return(Executed_Goal)
}






