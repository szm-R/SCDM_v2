#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function sets the model parameters.

Experiment_Setup <- function()
{
    ParamSetup = read_excel("Setup.xlsx", col_names = TRUE, 
                            sheet = "ParamSetups", 
                            .name_repair = "unique_quiet")
    
    if (DM_Value$PeI[DP])
        Num_Peers <<- 5
    else
        Num_Peers <<- 0
    
    Num_Agents <<- Num_Peers + 1
    
    # We build the model configuration data frame with the 
    # following parameters:
    #    R    : parental rule
    #    IP   : adolescent's imitation probability
    #    ILGPP: adolescent's initial LG preference probability
    # The first row of model config contains the target agent's
    # setting, and the following rows represent that of peers.
    Parameter_List <<- c("R", "IP", "ILGPP") 
    Model_Config <<- as.data.frame(matrix(0, 
                                          nrow = Num_Agents, 
                                          ncol = length(Parameter_List)))
    colnames(Model_Config) <<- Parameter_List
    
    
    ############################################
    ####  Auxiliary Variables and Constants ####    
    ############################################
    
    # LG and SG indices
    LG_Idx <<- 1
    SG_Idx <<- 2
    
    # Number of steps in the simulation's main body
    SimDuration <<- 1680
    
    # Number of steps where we calculate the model 
    # response (the LGRD)
    Response_Steps <<- 312
    
    # Number of steps representing a working day
    Day_Steps <<- 24
    
    # The range of the state of space
    State_Range <<- 100
    
    
    # Setting the imitation probability (IP) based on
    # the agents' peer influence susceptibility
    PI_susceptibility = ParamSetup$PIS[PS_Idx]
    if (PI_susceptibility == "Low")
        Model_Config$IP <<- rep(0.2, Num_Agents)
    else if (PI_susceptibility == "Medium")
        Model_Config$IP <<- rep(0.5, Num_Agents)
    else # high peer influence susceptibility
        Model_Config$IP <<- rep(0.8, Num_Agents)
    
    # The initial LG preference probability
    #    The target agent's ILGPP is either lower or
    #    or higher than that of its peers
    if (ParamSetup$PLGPP[PS_Idx] == "Lower")
        Peer_Status = 1
    else
        Peer_Status = 2
    
    Model_Config$ILGPP <<- c(TA_ILGPP, 
                             rep(PA_ILGPP[Peer_Status], 
                                 Num_Peers))
    
    # Setting target rule ratios based on target parenting  
    # style levels, strict, moderate, and lax
    Parenting_Style = ParamSetup$TPS[PS_Idx]
    if (Parenting_Style == "Strict")
        TA_PR = 0.125
    else if (Parenting_Style == "Moderate")
        TA_PR = 0.5
    else
        TA_PR = 0.875
    
    # When the PaR is FALSE, there are no parenting 
    # restrictions, so, the ratio would be 1.0
    if (DM_Value$PaR[DP])
        Model_Config$R[1] <<- TA_PR
    else
        Model_Config$R[1]  <<- 1.0
  
      
    if (Num_Peers)
    {
        # Setting peer rule ratios based on peer parenting  
        # style levels, strict, moderate, and lax
        Parenting_Style = ParamSetup$PPS[PS_Idx]
        if (Parenting_Style == "Strict")
            PA_PR = 0.125
        else if (Parenting_Style == "Moderate")
            PA_PR = 0.5
        else
            PA_PR = 0.875
        
        # When the PaR is FALSE, there are no parenting 
        # restrictions, so, the ratio would be 1.0
        if (DM_Value$PaR[DP])
            Model_Config$R[2:Num_Agents] <<- rep(PA_PR, Num_Peers)
        else
            Model_Config$R[2:Num_Agents]  <<- rep(1.0, Num_Peers)
    }
}








