library("readxl");library(dplyr);library(tidyverse);library(RJafroc);library(WriteXLS);library(glue)
library("e1071");library(epiR);library(pROC);library(caret);

auc_rrrc_result<-function(data){ 
  #data:StSignificanceTesting ftn result w/ option ="RRRC"
  #######################################
  ##AUC result
  #######################################
  ##test result(AUC, CI)
  test_res <- data$ciAvgRdrEachTrtRRRC %>% 
    rename(AUC=Area) %>% 
    dplyr::select(Treatment, AUC, CILower, CIUpper)
  
  index<-unlist(lapply(test_res,is.numeric))
  test_res[,index]<-round(test_res[,index],digit=3)
  
  ##difference test result(AUC difference result, CI, p value)
  diff_res <- data$ciDiffTrtRRRC %>% 
    mutate(P_value=if_else(PrGTt < .001, "<0.001", as.character(round(PrGTt,3)))) %>% 
    mutate(Treatment="Trt1-Trt2") %>% 
    rename(AUC=Estimate) %>% 
    dplyr::select(Treatment, AUC, CILower, CIUpper, P_value)
  
  index <- unlist(lapply(diff_res, is.numeric))
  diff_res[,index] <- round(diff_res[,index], digit=3)
  
  ## combine test result & difference test result
  result_auc <- bind_rows(test_res,diff_res)
  result_auc$P_value[is.na(result_auc$P_value)]<-"-"
  result_auc$Treatment<-str_replace_all(result_auc$Treatment, "Trt", "Test")
  
  ########################################################
  ##AUC result by reader
  ########################################################
  id_num <- as.numeric(str_remove_all(colnames(data$fomArray),"[Rdr]")) 
  id <- paste0("Reader",ifelse(id_num < 10, paste0("0", as.character(id_num)), as.character(id_num)))
  colnames(data$fomArray) <- id
  result_auc_reader <- t(t(data$fomArray)[order(row.names(t(data$fomArray))), ])
  rownames(result_auc_reader)<-c("Test1","Test2")
  
  # Table1_LROC AUC(LOM) OUTPUT: [Table]

  result <- list(result_auc = result_auc , result_auc_reader = result_auc_reader)
  return(result)
}

#MRMC AUC
dataset = DfReadDataFile("lom.csv", format = "MRMC", delimiter = ",")

#data set(lom.csv) sample
#reader	treatment	case_id	truth	rating
#1       	1       	161   	1   	6
#1	      2	        161	    1	    6
#2	      1	        161	    1	    5
#2	      2	        161	    1	    5
#...
#1	      1       	162   	1   	4
#1	      2	        162   	1   	4
#2       	1	        162   	1   	2
#2       	2	        162   	1	    2


rrrc_result <-  StSignificanceTesting (dataset, FOM = "Wilcoxon", alpha = 0.05, 
                                          method = "DBMH", option = "RRRC")

auc_rrrc_result(rrrc_result)

auc_frrc_result<-function(data){ 
  #data:StSignificanceTesting ftn result w/ option ="FRRC"
  #######################################
  ##Merge AUC result and difference test result
  #######################################
  auc_result <- as.data.frame(t(data$fomArray))
  auc_result$Reader <- rownames(auc_result)
  
  total_result <- inner_join(auc_result, data$ciDiffTrtEachRdrFRRC, by = "Reader") %>% 
    mutate(p_value = if_else(PrGTt < 0.001, "<0.001", as.character(round(PrGTt, 3)))) %>% 
    dplyr::select(Reader, Trt1, Trt2, Estimate, CILower, CIUpper, p_value) %>% 
    rename(Diff = Estimate)
  total_result[,2:6] <- sapply(total_result[,2:6], function(x) {round(as.numeric(x), 3)})
  return(total_result)
}

frrc_result <-  StSignificanceTesting (dataset, FOM = "Wilcoxon", alpha = 0.05, 
                                          method = "DBMH", option = "FRRC")
auc_frrc_result(frrc_result)
