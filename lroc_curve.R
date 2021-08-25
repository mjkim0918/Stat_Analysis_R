
library(tidyverse)
library(RJafroc)



lroc_curve <- function(data, plotT, plotR){
    LCM_lroc <- PlotEmpiricalOperatingCharacteristics(dataset = data, trts = plotT,rdrs = plotR, opChType = "LROC", 
                                                       legend.position = c(0.9,0.4))
    
    
    data <- LCM_lroc$Points %>% 
      mutate(reader2 = as.numeric(str_sub(Reader, 4))) %>% 
      mutate(Reader = if_else(reader2 < 10, paste0("Reader0", reader2), paste0("Reader", reader2))) %>% 
      mutate(Modality = str_replace(Modality, "M: ", "Test")) %>% 
      rename(FPF = genAbscissa, TPF = genOrdinate)
    
    
    plot <- ggplot(data) + geom_line(mapping = aes(x=FPF, y = TPF, color = Reader, linetype = Modality))
    return(plot)
}


###############
##example
###############

Sys.setlocale(category = "LC_ALL", locale = "us")
dataset = DfReadDataFile("/Users/lunit/Documents/lroc_curve/LCM(LROC).xlsx", 
                         format = "JAFROC", delimiter = ",")

lrocDataset <- DfFroc2Lroc(dataset)
#각 테스트별 reader 별 그래프 -> plotT와 plotR을 vector 의 형태로 넣어줘야 함
lroc_curve(lrocDataset, c(1:2), c(1:12))
