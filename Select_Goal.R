#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel selects a goal to be pursued in the current 
#              simulation step.


Select_Goal <- function(Agent_Idx)
{
    ##########################################################################
    # The agent selects goals based on its position within the state space.  #
    # we define this state space by two axes: the horizontal axis            #
    # representing the tendency toward the long-term goal (LG), and the      #
    # vertical axis representing the tendency toward the short-term goal     #
    # (SG). Therefore, a position in the lower half of the state space       #
    # indicates a stronger preference for the long-term goal, while a        #
    # position in the upper half reflects a greater inclination towards the  #
    # short-term goal.                                                       #
    ##########################################################################
    
    if (SS_Position[Agent_Idx,1] > SS_Position[Agent_Idx,2])
        Selected_Goal = LG_Idx
    else
        Selected_Goal = SG_Idx
    
    return(Selected_Goal)
}