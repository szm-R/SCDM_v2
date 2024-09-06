#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots the LGRD vs. initial LGPP values for  
#              different parenting styles in the Parent-Only (Single  
#              Family Unit) case. The input comes from running the code 
#              ExpAnalysis_SingleFU.R.


library(readxl)
library(ggplot2)

FilePath = "Output/SimulationData/RS_SingleFU.xlsx"
RS = read_excel(FilePath, col_names  =  TRUE)

RS$ILGPP = as.factor(RS$ILGPP)

windowsFonts(Times = windowsFont("Times New Roman"))

PS_ILGPP = ggplot(RS, aes(x = ILGPP, y = Mean, color = PS, fill = PS)) + 
           geom_bar(alpha = 0.75, stat = "identity", position = position_dodge()) +
           scale_color_manual(values = c("#2FAAB9", "#FFB000", "#004D40")) +
           scale_fill_manual(values = c("#2FAAB9", "#FFB000", "#004D40")) +
           geom_errorbar(aes(ymin = LB, ymax = UB), width = .2,
                         position = position_dodge(.9), color  =  "black") +
           labs(y = "LGR difference", 
                x = "Initial LGPP",
                fill="Parenting style", 
                color="Parenting style") +
           theme_light() +
           theme(text = element_text(size = 8,family = "Times"))

FigurePath = paste("Output/Figures", "fig-ILGPP_SF", sep  =  "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width = 5, height = 4, dpi = 1000)

ggsave(file = paste(FigurePath, "jpeg", sep = "."), 
       width = 5, height = 4, dpi = 1000)

print(PS_ILGPP)









