#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function initializes the model for each run.

Initialize <- function()
{
    # The adolescent agent counts the number of
    # SG Goals using this variable
    SG_Counter <<- rep(0, Num_Agents)
    
    # Choose a random point as the initial position of the agent
    # on the state spaces
    SS_Position <<- matrix(floor(runif(Num_Agents*2, 
                                       min = 5, max = State_Range - 5)),
                           nrow = Num_Agents)
    
    # The LG preference probability
    LGPP <<- Model_Config$ILGPP
    
    Perceived_Autonomy <<- rep(0, Num_Agents)
    
    ############################################
    ########      Adolescent States     ########    
    ############################################
    
    # The values of the hypothetical potential field (HPF) 
    # of the agent's state space
    HPF <<- replicate(n = Num_Agents,
                      expr = matrix(0, 
                                    State_Range, 
                                    State_Range),
                      simplify = FALSE)
    
    # Define a matrix to store the peers' executed Goals
    # from the previous step
    Peer_Previous_Goal <<- matrix(0, Num_Agents, Num_Agents)
    
    ############################################
    ########       Observer state       ########    
    ############################################
    
    # Define a data frame for each agent to store a history of 
    # Goals
    Goal_History = data.frame(matrix(ncol = 5, nrow = 0))
    Column_Names = c("SG", "EG", "PP", "PE", "PoC")
    colnames(Goal_History) = Column_Names
    Agent_Goal_Hist <<- replicate(n = Num_Agents,
                                    expr = Goal_History,
                                    simplify = FALSE)
}








