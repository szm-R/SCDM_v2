#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel chooses the next state space position.

debugSource("Check_LocalMin.R")

Choose_Next_SSP <- function(Agent_Idx)
{
    ##########################################################################
    #############                                                #############   
    #############         Determining the jump direction         #############
    #############                                                #############   
    ##########################################################################
    
    # print("CNS function")
    
    # Movement in the state space can occur in 8 directions, four directions
    # parallel to the x and y axes, and the other four in between (which
    # would be parallel and perpendicular to the y = x line)
    Direction_List = rbind(c(0, 1), c(-1, 1), c(-1, 0), c(-1, -1),
                           c(0, -1), c(1, -1), c(1, 0), c(1, 1))

    # LG direction preference
    LG_DP = LGPP[Agent_Idx]

    # SG direction preference
    SG_DP = 1 - LGPP[Agent_Idx]

    # Moving toward directions 1 to 3 increases the SG to LG ratio, while
    # moving in the opposite direction of 5 to 7 decreases this relative
    # ratio. Therefore, we set the probability of moving toward the first
    # three to SG_DP and the probability of the second set to LG_DP.
    # Directions 2 and 8  are neutral therefore, we have divided them
    # equally between the two goals
    Direction_Prob = c(SG_DP, SG_DP, SG_DP, SG_DP,
                       LG_DP, LG_DP, LG_DP, LG_DP)

    SSP_x = SS_Position[Agent_Idx,1]
    SSP_y = SS_Position[Agent_Idx,2]

    # Determine the Jump Direction (JD) based on the agent's current position
    # within the state space (being in corners and edges constrains movement)
    if (SSP_x < 10 && SSP_y > 90) # top left
        JD = sample(c(5,6,7), size = 1, prob = Direction_Prob[c(5,6,7)])
    else if (SSP_x > 90 && SSP_y > 90) # top right
        JD = sample(c(3,4,5), size = 1, prob = Direction_Prob[c(3,4,5)])
    else if (SSP_x > 90 && SSP_y < 10) # bottom right
        JD = sample(c(1,2,3), size = 1, prob = Direction_Prob[c(1,2,3)])
    else if (SSP_x < 10 && SSP_y < 10) # bottom left
        JD = sample(c(1,7,8), size = 1, prob = Direction_Prob[c(1,7,8)])
    else if (SSP_y < 10) # bottom middle
        JD = sample(c(1,2,3,7,8), size = 1, prob = Direction_Prob[c(1,2,3,7,8)])
    else if (SSP_y > 90) # top middle
        JD = sample(c(3,4,5,6,7), size = 1, prob = Direction_Prob[c(3,4,5,6,7)])
    else if (SSP_x > 90) # middle right
        JD = sample(c(1,2,3,4,5), size = 1, prob = Direction_Prob[c(1,2,3,4,5)])
    else if (SSP_x < 10) # middle left
        JD = sample(c(1,5,6,7,8), size = 1, prob = Direction_Prob[c(1,5,6,7,8)])
    else
        JD = sample(c(1:8), size = 1, prob = Direction_Prob)

    # if (SSP_x < 10 && SSP_y > 90) # top left
    # {
    #     print("top left")
    #     JD = sample(c(5,6,7), size = 1, prob = Direction_Prob[c(5,6,7)])
    # }
    # else if (SSP_x > 90 && SSP_y > 90) # top right
    # {
    #     print("top right")
    #     JD = sample(c(3,4,5), size = 1, prob = Direction_Prob[c(3,4,5)])
    # }
    # else if (SSP_x > 90 && SSP_y < 10) # bottom right
    # {
    #     print("bottom right")
    #     JD = sample(c(1,2,3), size = 1, prob = Direction_Prob[c(1,2,3)])
    # }
    # else if (SSP_x < 10 && SSP_y < 10) # bottom left
    # {
    #     print("bottom left")
    #     JD = sample(c(1,7,8), size = 1, prob = Direction_Prob[c(1,7,8)])
    # }
    # else if (SSP_y < 10) # bottom middle
    # {
    #     print("bottom middle")
    #     JD = sample(c(1,2,3,7,8), size = 1, prob = Direction_Prob[c(1,2,3,7,8)])
    # }
    # else if (SSP_y > 90) # top middle
    # {
    #     print("top middle")
    #     JD = sample(c(3,4,5,6,7), size = 1, prob = Direction_Prob[c(3,4,5,6,7)])
    # }
    # else if (SSP_x > 90) # middle right
    # {
    #     print("middle right")
    #     JD = sample(c(1,2,3,4,5), size = 1, prob = Direction_Prob[c(1,2,3,4,5)])
    # }
    # else if (SSP_x < 10) # middle left
    # {
    #     print("middle left")
    #     JD = sample(c(1,5,6,7,8), size = 1, prob = Direction_Prob[c(1,5,6,7,8)])
    # }
    # else
    # {
    #     print("middle")
    #     JD = sample(c(1:8), size = 1, prob = Direction_Prob)
    # }


    ##########################################################################
    #############                                                #############
    #############         Determining the jump magnitude         #############
    #############                                                #############
    ##########################################################################

    TD = c(0,0)


    ## Determine the jump length along the x axis

    if (Direction_List[JD,1] != 0)
    {
        if (Direction_List[JD,1] < 0)
            x_range = SSP_x - 5
        else
            x_range = (State_Range - 5) - SSP_x

        # Repeatedly draw from a normal distribution to get a final
        # destination point that is within the state space's range
        JL_x = rnorm(1, mean = 0, sd = x_range)
        TD[1] = floor(SSP_x + Direction_List[JD,1]*JL_x)
        while((TD[1] < 5 || TD[1] > (State_Range - 5)) || JL_x < 0)
        {
            JL_x = rnorm(1, mean = 0, sd = x_range)
            TD[1] = floor(SSP_x + Direction_List[JD,1]*JL_x)
        }
    }
    else
        TD[1] = SSP_x


    ## Determine the jump length along the y axis

    if (Direction_List[JD,2] != 0)
    {
        if (Direction_List[JD,2] < 0)
            y_range = SSP_y - 5
        else
            y_range = (State_Range - 5) - SSP_y

        # Repeatedly draw from a normal distribution to get a final
        # destination point that is within the state space's range
        JL_y = rnorm(1, mean = 0, sd = y_range)
        TD[2] = floor(SSP_y + Direction_List[JD,2]*JL_y)
        while((TD[2] < 5 || TD[2] > (State_Range - 5)) || JL_y < 0)
        {
            JL_y = rnorm(1, mean = 0, sd = y_range)
            TD[2] = floor(SSP_y + Direction_List[JD,2]*JL_y)
        }
    }
    else
        TD[2] = SSP_y


    ##########################################################################
    #############                                                #############
    #############          Finalizing the next position          #############
    #############                                                #############
    ##########################################################################

    # Relocate the selected position to deepest local minimum, i.e., the
    # deepest attractor
    Dest_Point = Check_LocalMin(TD[1], TD[2], HPF[[Agent_Idx]])

    SS_Position[Agent_Idx,1] <<- Dest_Point[[1]]
    SS_Position[Agent_Idx,2] <<- Dest_Point[[2]]
}











