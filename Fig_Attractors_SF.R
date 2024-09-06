#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code depicts the attractors as colored blobs with the 
#              agent's goal state space. The input comes from running the code
#              Run_Experiment.R for design point 3 (the parent only case) for 
#              setup numbers 19, indicating a Moderate parenting style for 
#              the target family unit. 


library(readxl)
library(ggplot2)
library(ggpubr)
library(igraph)
library(raster)

PS_Idx = 19

# The subject IDs are equivalent to the variable Run_Idx in the inner
# loop of the code Run_Experiment.R. Here we use IDs 72 and 1 with a 
# final LGRD of -0.067 and 0.294, respectively.
SubIDs = c(72, 1)
steps = c(96, 840, 1680)

windowsFonts(Times = windowsFont("Times New Roman"))

##########################################################################
##########################################################################
#############              Read the state spaces             #############    
##########################################################################
##########################################################################

Base_SS = data.frame(matrix(nrow = 100, ncol = 100))
SS_List = replicate(n = 6, expr = Base_SS, simplify = FALSE)

for (i in 1:2)
{
    FileName1 = paste("SS_Snapshots", PS_Idx, SubIDs[i], sep = "_")
    FileName2 = paste(FileName1, "xlsx", sep = ".")
    FilePath = paste("Output/SimulationData", FileName2, sep = "/")
    
    for (j in 1:3)
    {
        SheetName = paste("Step", steps[j], sep = "")
        HPF = read_excel(FilePath, sheet = SheetName,
                         col_names = FALSE,
                         .name_repair = "unique_quiet")
        
        SS_List[[(i-1)*3+j]] = -1*as.matrix(HPF)
    }
}

Max_Att_Depth = max(max(SS_List[[3]]), max(SS_List[[6]]))


##########################################################################
##########################################################################
#############          Extract the attractor blobs           #############    
##########################################################################
##########################################################################

Max_Disp = c(3, 20, 20, 3, 20, 20)

AttP = data.frame(matrix(0, nrow = 0, ncol = 4))
colnames(AttP) = c("AttractorID", "LG.tendecy", 
                    "SG.tendecy", "AverageDepth")
AttPatches = replicate(n = 6, expr = AttP, simplify = FALSE)

Attmeans_List = vector("list", 6)
for (i in 1:6)
{
    NSS = SS_List[[i]]/Max_Att_Depth
    
    NSS[NSS < 0.01] = 0
    
    # Detect patches of connected cells within the state space matrix
    # by converting it to a raster object
    Rmat = raster(NSS)
    Clumps = as.matrix(clump(Rmat, directions = 4))
    
    # Turn the clumps into a list
    Total = max(Clumps, na.rm = TRUE)
    Blob_List = vector("list", Total)
    
    Attmeans = NULL
    
    for (t in 1:Total)
    {
        Blob_List[t] = list(which(Clumps == t, arr.ind = TRUE))
        Attmeans[t] = mean(NSS[Blob_List[[t]]])
        
        NewAttractor = as.data.frame(Blob_List[[t]])
        
        NewAttractor = cbind(a = t, NewAttractor)
        NewAttractor = cbind(NewAttractor, d = mean(NSS[Blob_List[[t]]]))
        colnames(NewAttractor) = c("AttractorID", "LG.tendecy", 
                                   "SG.tendecy", "AverageDepth")
        
        AttPatches[[i]] = rbind(AttPatches[[i]], NewAttractor)
    }
    Attmeans_List[[i]] = Attmeans
}

Attmeans_List = lapply(Attmeans_List ,sort, decreasing = TRUE)

for (i in 1:6)
{
    if (length(Attmeans_List[[i]]) < 11)
        Cut_Threshold = 0
    else
        Cut_Threshold = Attmeans_List[[i]][11]

    AttPatches[[i]] = subset(AttPatches[[i]], AverageDepth > Cut_Threshold)
}

max_Attmean = max(unlist(Attmeans_List))
min_Attmean = min(unlist(Attmeans_List))


##########################################################################
##########################################################################
#############               Plot the attractors              #############    
##########################################################################
##########################################################################

SS_Plots = NULL

for (i in 1:6)
{
    p = ggplot(data = AttPatches[[i]], 
               aes(LG.tendecy, SG.tendecy, color = AverageDepth)) +
        geom_point() + 
        geom_abline(intercept = 0, slope = 1, 
                    linewidth = 1, linetype = "dashed") +
        scale_x_continuous(limits = c(0, 100)) + 
        scale_y_continuous(limits = c(0, 100)) +
        labs(y = "SG tendency", 
             x = "LG tendency", 
             color="Attractor depth") +
        scale_color_gradient(low="#FFC107", high="#004D40", 
                             limits = range(min_Attmean, max_Attmean)) +
        theme_light() +
        theme(text = element_text(size = 8,family = "Times"))
    
    SS_Plots[[i]] = p
}

MultiPlot = ggarrange(plotlist = SS_Plots,
                      labels = c("", ""), 
                      ncol = 3, nrow = 2, 
                      common.legend = TRUE,
                      legend="right")

print(MultiPlot)

Subject = paste("Subjects", SubIDs[1], SubIDs[2], sep = "_")
FileName = paste("fig-AttractorPlot", Subject, sep  =  "_")
FigurePath = paste("Output/Figures", FileName, sep  =  "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width = 10, height = 5, dpi = 1000)

ggsave(file = paste(FigurePath, "jpeg", sep = "."), 
       width = 10, height = 5, dpi = 1000)

















