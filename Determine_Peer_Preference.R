#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel outputs the goal index of the peer preference.

Determine_Peer_Preference <- function(Agent_Idx)
{
    LG_Count = sum(Peer_Previous_Goal[Agent_Idx,] == LG_Idx)
    SG_Count = sum(Peer_Previous_Goal[Agent_Idx,] == SG_Idx)
    
    # Choose the more common goal as the peer preference
    Peer_Preference = 0
    if (SG_Count > LG_Count)
        Peer_Preference = SG_Idx
    else if (LG_Count > SG_Count)
        Peer_Preference = LG_Idx
    
    return(Peer_Preference)
}





