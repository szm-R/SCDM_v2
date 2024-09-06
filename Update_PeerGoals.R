#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel updates the matrix containing the goals peers
#              have executed in the previous simulation step.


Update_PeerGoals <- function()
{
    for (i in 1:Num_Agents)
    {
        for (j in 1:Num_Agents)
        {
            Goal_History = Agent_Goal_Hist[[j]]
            Executed_Goal = Goal_History$EG[nrow(Goal_History)]
            
            if (i == j)
                Peer_Previous_Goal[i,j] <<- 0
            else
                Peer_Previous_Goal[i,j] <<- Executed_Goal
        }
    }
}





