draw_confusion_stats <- function(cm) {

    library(dplyr)
    # confusion matrix byClass to look at sensitivity and specificity etc as data.frame
    cm_cl1 <-data.frame(cm$byClass) %>% select("Sensitivity", "Specificity", "Pos.Pred.Value", "Neg.Pred.Value", "Precision") %>%
        rename("Sen" = "Sensitivity", "Spec" = "Specificity", "Pos" = "Pos.Pred.Value", "Neg" = "Neg.Pred.Value", "Prec" = "Precision")
    cm_cl2 <-data.frame(cm$byClass) %>% select("Recall", "F1", "Prevalence", "Detection.Rate", "Detection.Prevalence", "Balanced.Accuracy") %>%
        rename("Rec"="Recall", "Prev" = "Prevalence", "DetRat" = "Detection.Rate", "DetPrev" = "Detection.Prevalence", "BalAcc" = "Balanced.Accuracy")

    # round the values
    cm_cl1 <- round(cm_cl1,2)
    cm_cl2 <- round(cm_cl2,2)

    library(ggplot2)
    library(gridExtra)
    library(grid)

    # plotting the stats

    cm_cl_p1 <- tableGrob(cm_cl1)
    cm_cl_p2 <- tableGrob(cm_cl2)

    # arranging the tables

    b <-  arrangeGrob(cm_cl_p1, nrow = 1, ncol = 1, top=textGrob("", gp=gpar(fontsize=20,font=1)))
    c <-  arrangeGrob(cm_cl_p2, nrow = 1, ncol = 1, top=textGrob("", gp=gpar(fontsize=20,font=1)))

    g <- grid.arrange(b, c, nrow = 2, ncol = 1)
    g

}
