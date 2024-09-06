#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel outputs a parental expectation of either
#              None or LG. 

Determine_Parental_Expectation <- function(step, Agent_Idx)
{
    # First, we reset the SG_Counter if Day_Steps has passed
    if (step %% Day_Steps == 1)
        SG_Counter[Agent_Idx] <<- 0
    
    # Next, we check the SG_Counter: 
    #
    #    - If it has not reached SG_Limit, there is no expectation,
    #    - Otherwise, the parents expect the adolescent to execute 
    #      an LG instead.
    
    # The parental rule is converted to a limit on
    # the number of SG actions the adolescent can 
    # execute in every day
    SG_Limit = floor(Day_Steps*Model_Config$R[Agent_Idx])
    if (SG_Counter[Agent_Idx] < SG_Limit)
    {
        SG_Counter[Agent_Idx] <<- SG_Counter[Agent_Idx] + 1
        Parental_Expectation = "None"
    }
    else
        Parental_Expectation = "LG"
    
    return(Parental_Expectation)
}





