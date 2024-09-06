#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots a histogram of LGRDs for a single initial LGPP 
#              for different parenting styles in the Parent-Only (Single  
#              Family Unit) case. The input comes from running the code
#              Run_Experiment.R for design point 3 (the parent only case) for 
#              setup numbers 1, 19, and 37, indicating Strict, Moderate, and 
#              lax parenting styles for the target family unit.


library(readxl)
library(xlsx)
library(ggplot2)
library(ggpubr)


LGPP = 0.2

SheetName = paste("LGPP", LGPP, sep = "=")
FilePath = "Output/SimulationData/LGRDs_SingleFU.xlsx"
Final_LGRDs = read_excel(FilePath, sheet = SheetName,
                         .name_repair = "unique_quiet", 
                         col_names = FALSE )


Input = as.data.frame(matrix(0, nrow = 300, ncol = 2))
colnames(Input) = c("LGRD", "PS")

Input$LGRD[1:100] = Final_LGRDs[1,]
Input$PS[1:100] = "Strict"

Input$LGRD[101:200] = Final_LGRDs[2,]
Input$PS[101:200] = "Moderate"

Input$LGRD[201:300] = Final_LGRDs[3,]
Input$PS[201:300] = "Lax"

Input$LGRD = as.numeric(Input$LGRD)


windowsFonts(Times = windowsFont("Times New Roman"))

LGRD_hist = ggplot(Input, aes(x=LGRD, color = PS, fill=PS)) +
            geom_histogram(alpha=0.75, position = position_dodge(), 
                           binwidth=0.1) +
            scale_color_manual(values=c("#2FAAB9", "#FFB000", "#004D40")) + 
            scale_fill_manual(values=c("#2FAAB9", "#FFB000", "#004D40")) +
            labs(y = "Count", 
                 x = "LGR difference",
                 fill="Parenting style", 
                 color="Parenting style") +
            theme_light() +
            theme(text=element_text(size=8,family="Times"))


FileName = paste("fig-LGRDhist_SF", LGPP, sep = "_")
FigurePath = paste("Output/Figures", FileName, sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width = 5, height = 4, dpi = 1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), 
       width = 5, height = 4, dpi = 1000)

print(LGRD_hist)







