#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code implements the Check_LocalMin function under
#              the Choose_Next_SSP submodel.

Check_LocalMin <- function(In_r, In_c, Source)
{
    ###########################################################################
    # This function looks for the local minimum neighboring the selected      #
    # destination position.                                                   #
    #                                                                         #
    # When a local minimum is found in the (normally) 3x3 window              #
    # surrounding the destination position, the function calls itself         #
    # recursively and the search shifts to the window surrounding the new     #
    # local minimum. The process continues until we can find no further       #
    # minimum, i. e. the point resides in the deepest neighboring valley.     #
    ###########################################################################
    
    # By default, the focal node (the coordinates of the destination position
    # within the target search window) resides in the center of a 3x3 window.
    # One column above and below this node and one row above and below 
    # this node defines the boundaries of the target search window
    Focal_r = 2
    Focal_c = 2
    
    # If the upper boundaries exceed the state space, they are set to the 
    # the state space's range
    # In these cases the target window becomes a 3x2 or a 2x3 matrix
    
    # The cases where the upper row (left) and the upper column (right) 
    # exceed the state space boundary (SR), the focal node is shown by #:
    #
    #              1             SR        1             SR
    #            1 . . . . . . . .       1 . . . . . . . .
    #              . . . . . . . .         . . . . . . . .
    #              . . . . . . . .         . . . . . . x x
    #              . . . . . . . .         . . . . . . x #
    #              . . . . . . . .         . . . . . . x x
    #              . . . . . . . .         . . . . . . . .
    #              . . x x x . . .         . . . . . . . .
    #           SR . . x # x . . .      SR . . . . . . . .
    
    # Upper row
    Upper_r = In_r + 1
    if (Upper_r > State_Range)
        Upper_r = State_Range
       
    # Upper column 
    Upper_c = In_c + 1
    if (Upper_c > State_Range)
    {
        Upper_c = State_Range
    }
        
    # If the lower boundaries exceed the state space, they are set to the 
    # the state space's range.
    # In these cases the target window becomes a 3x2 or a 2x3 matrix. 
    # Moreover, here the focal node's coordinate also changes. 
    
    # The cases where the lower row (left) and the lower column (right) 
    # exceed the state space boundary (SR), the focal node is shown by #:
    #
    #              1             SR        1             SR
    #            1 . . x # x . . .       1 . . . . . . . .
    #              . . x x x . . .         . . . . . . . .
    #              . . . . . . . .         x x . . . . . .
    #              . . . . . . . .         # x . . . . . .
    #              . . . . . . . .         x x . . . . . .
    #              . . . . . . . .         . . . . . . . .
    #              . . . . . . . .         . . . . . . . .
    #           SR . . . . . . . .      SR . . . . . . . .
    
    # Lower row
    Lower_r = In_r - 1
    if (Lower_r < 1)
    {
        Lower_r = 1
        Focal_r = 1
    }
        
    # Lower column
    Lower_c = In_c - 1
    if (Lower_c < 1)
    {
        Lower_c = 1
        Focal_c = 1
    }
        
    # Separate the target search window from the state space.
    Target_Win = Source[Lower_r:Upper_r, Lower_c:Upper_c]
    
    # Find the indices of all local minimums within the target window
    Min_Idx = which(Target_Win == min(Target_Win), arr.ind = TRUE)
    
    # Check if the input node (Focal_r,Focal_c) is among the minimums
    In_Rows = grep(Focal_r, Min_Idx[,1], value = FALSE)
    In_Cols = grep(Focal_c, Min_Idx[,2], value = FALSE)
    InputMin = intersect(In_Rows,In_Cols)
    
    # Return the input node as the output if it is among the minimums
    if (length(InputMin) != 0)
        return(list(In_r, In_c))
    
    # Return the input node as the output if all surrounding nodes have 
    # similar values, in such a case the destination position resides  
    # in a plateau. 
    NumberOfNodes = nrow(Target_Win)*ncol(Target_Win)
    if (length(Min_Idx) == NumberOfNodes)
        return(list(In_r, In_c))
    
    # If there are more than one minimum, choose the next path randomly
    Row_Idx = 1
    if (length(Min_Idx) > 2)
        Row_Idx = sample.int(length(Min_Idx)/2, 1)
    
    # Adjust the coordinates of the local minimum to the original 
    # state space
    Corrected_r = In_r + (Min_Idx[Row_Idx,1] - Focal_r)
    Corrected_c = In_c + (Min_Idx[Row_Idx,2] - Focal_c)
    
    # Adjust the destination to the state space boundaries
    if (Corrected_r > State_Range)
        Corrected_r = State_Range
    
    if (Corrected_c > State_Range)
        Corrected_c = State_Range 
    
    if (Corrected_r < 1)
        Corrected_r = 1
    
    if (Corrected_c < 1)
        Corrected_c = 1 
    
    # Stop the search if the new point resides in either of the four corners 
    # of the state space
    if ((Corrected_r == State_Range & Corrected_c == State_Range) |
        (Corrected_r == 1 & Corrected_c == 1) |
        (Corrected_r == 1 & Corrected_c == State_Range) |
        (Corrected_r == State_Range & Corrected_c == 1))
    {
        return(list(Corrected_r, Corrected_c))
    }
    else
    {
        return(Check_LocalMin(Corrected_r, Corrected_c, Source))
    }
        
}








