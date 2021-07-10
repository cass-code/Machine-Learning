draw_confusion_matrix1 <- function(cm) {
  
  library(dplyr)
  # extract the confusion matrix values as data.frame
  cm_d <- as.data.frame(cm$table)
  # confusion matrix statistics as data.frame
  cm_st <-data.frame(cm$overall)
  # confusion matrix byClass to look at sensitivity and specificity etc as data.frame
  cm_cl1 <-data.frame(cm$byClass) %>% select("Sensitivity", "Specificity", "Pos.Pred.Value", "Neg.Pred.Value", "Precision") %>%
    rename("Sen" = "Sensitivity", "Spec" = "Specificity", "Pos" = "Pos.Pred.Value", "Neg" = "Neg.Pred.Value", "Prec" = "Precision")
  cm_cl2 <-data.frame(cm$byClass) %>% select("Recall", "F1", "Prevalence", "Detection.Rate", "Detection.Prevalence", "Balanced.Accuracy") %>%
    rename("Rec"="Recall", "Prev" = "Prevalence", "DetRat" = "Detection.Rate", "DetPrev" = "Detection.Prevalence", "BalAcc" = "Balanced.Accuracy")
  
  # round the values
  cm_st$cm.overall <- round(cm_st$cm.overall,2)
  cm_cl1 <- round(cm_cl1,2)
  cm_cl2 <- round(cm_cl2,2)
  
  # rounded percentage values
  cm_p <- as.data.frame(prop.table(cm$table))
  cm_d$Perc <- round(cm_p$Freq*100,2)
  
  library(ggplot2)
  library(gridExtra)
  library(grid)
  
  # plotting
  
  
  cm_d_p <-  ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
    geom_tile() +
    geom_text(aes(label = paste("",Freq,",",Perc,"%")), color = 'black', size = 3) +
    theme_light() +
    guides(fill=FALSE)
  
  cm_d_p <- cm_d_p + scale_fill_distiller(palette = "Greens", direction = 1)
  
  
  # plotting the stats
  
  cm_cl_p1 <- tableGrob(cm_cl1)
  cm_cl_p2 <- tableGrob(cm_cl2)
  cm_st_p <- tableGrob(cm_st)
  
  
  
  # arranging all the graphs
  a <-  grid.arrange(cm_st_p, nrow = 1, ncol = 1,
                     top=textGrob("",gp=gpar(fontsize=30,font=1)))
  a
}
