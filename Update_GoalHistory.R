#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel updates the Goal history of the adolescent agents.


Update_GoalHistory <- function(Agent_Idx,  
                               Selected_Goal,
                               Executed_Goal,
                               Peer_Preference,
                               Parental_Expectation)
{
    
    # We store the history of the executed Goals in a data frame with
    # 5 columns:
    #    - SG: The index of the selected goal
    #    - EG: The index of the executed goal  
    #    - PP: The overall peer preference in this step (LG or SG)
    #    - PE: The parent agents' expectation of the adolescent (LG or None)
    #    - PoC: The probability of changing the originally selected goal

    NewRow = data.frame(Selected_Goal, Executed_Goal,
                        Peer_Preference, Parental_Expectation, PoC)
    names(NewRow) = c("SG", "EG", "PP", "PE", "PoC")
    Agent_Goal_Hist[[Agent_Idx]] <<- rbind(Agent_Goal_Hist[[Agent_Idx]], NewRow)
}





