rm(list = ls())
otu <- read.delim("AW.txt", na.strings = "NA", header = TRUE, fileEncoding = "UTF-16")
otu <- na.omit(otu)  
################
##
library(randomForest)

#
set.seed(123)
otu_forest <- randomForest(amewig~., data = otu, importance = TRUE, ntree = 500, nPerm = 1000)
otu_forest
#
importance_otu.scale <- data.frame(importance(otu_forest, scale = TRUE), check.names = FALSE)
importance_otu.scale

#
importance_otu.scale <- importance_otu.scale[order(importance_otu.scale$'%IncMSE', decreasing = TRUE), ]

# 
library(ggplot2)

#
color_gradient <- scale_fill_gradientn(
  colors = c("#2b8cbe", "#a6bddb", "#fee08b", "#fdae61", "#d73027"),
  limits = c(0, max(importance_otu.scale$`%IncMSE`, na.rm = TRUE))  
)
#
importance_otu.scale$OTU_name <- rownames(importance_otu.scale)
importance_otu.scale$OTU_name <- factor(importance_otu.scale$OTU_name, levels = importance_otu.scale$OTU_name)

p <- ggplot(importance_otu.scale, aes(x = OTU_name, y = `%IncMSE`, fill = `%IncMSE`)) +
  geom_bar(stat = "identity", width = 0.7) +
  color_gradient +  
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme_minimal(base_size = 14) + 
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = 'black'),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1) 
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(importance_otu.scale$`%IncMSE`, na.rm = TRUE) * 1.1))  

#
print(p)

#
p <- p +
  annotate('text', label = 'amewig', x = 20, y = max(importance_otu.scale$`%IncMSE`, na.rm = TRUE) * 0.7, size = 4) +
  annotate('text', label = sprintf('italic(R^2) == %.2f', 0.87), x = 20, y = max(importance_otu.scale$`%IncMSE`, na.rm = TRUE) * 0.6, size = 3, parse = TRUE)
p

# 
new_data1 <- read.delim("rcp45.txt", na.strings = "NA", header = TRUE, fileEncoding = "UTF-16")
# 
new_data2 <- read.delim("rcp85.txt", na.strings = "NA", header = TRUE, fileEncoding = "UTF-16")
#
new_data1 <- na.omit(new_data1)
new_data2 <- na.omit(new_data2)
#
expected_vars <- setdiff(names(otu), "amewig") 
new_data1 <- new_data1[, expected_vars, drop = FALSE]
new_data2 <- new_data2[, expected_vars, drop = FALSE]
#
for (col in expected_vars) {
  if (is.factor(otu[[col]])) {
    new_data1[[col]] <- factor(new_data1[[col]], levels = levels(otu[[col]]))
    new_data2[[col]] <- factor(new_data2[[col]], levels = levels(otu[[col]]))
  }
}
#
predictions1 <- predict(otu_forest, new_data1)
predictions2 <- predict(otu_forest, new_data2)

#
new_data1$predicted_American_wigeon <- predictions1
new_data2$predicted_American_wigeon <- predictions2

write.csv(new_data1, "AWpredicted_results rcp45.csv", row.names = FALSE)
write.csv(new_data2, "AWpredicted_results rcp85.csv", row.names = FALSE)

################
library(rfPermute)

#
set.seed(123)
otu_rfP <- rfPermute(amewig~., data = otu, importance = TRUE, ntree = 500, nPerm = 1000,nrep = 1000, num.cores=16)
otu_rfP

#
importance_otu.scale <- data.frame(importance(otu_rfP, scale = TRUE), check.names = FALSE)
importance_otu.scale

#
importance_otu.scale.pval <- (otu_rfP$pval)[ , , 2]
importance_otu.scale.pval

#
importance_otu.scale <- importance_otu.scale[order(importance_otu.scale$'%IncMSE', decreasing = TRUE), ]

importance_otu.scale$OTU_name <- rownames(importance_otu.scale)
importance_otu.scale$OTU_name <- factor(importance_otu.scale$OTU_name, levels = importance_otu.scale$OTU_name)


#
for (OTU in rownames(importance_otu.scale)) {
  importance_otu.scale[OTU,'%IncMSE.pval'] <- importance_otu.scale.pval[OTU,'%IncMSE']
  if (importance_otu.scale[OTU,'%IncMSE.pval'] >= 0.05) importance_otu.scale[OTU,'%IncMSE.sig'] <- ''
  else if (importance_otu.scale[OTU,'%IncMSE.pval'] >= 0.01 & importance_otu.scale[OTU,'%IncMSE.pval'] < 0.05) importance_otu.scale[OTU,'%IncMSE.sig'] <- '*'
  else if (importance_otu.scale[OTU,'%IncMSE.pval'] >= 0.001 & importance_otu.scale[OTU,'%IncMSE.pval'] < 0.01) importance_otu.scale[OTU,'%IncMSE.sig'] <- '**'
  else if (importance_otu.scale[OTU,'%IncMSE.pval'] < 0.001) importance_otu.scale[OTU,'%IncMSE.sig'] <- '***'
}
# 
write.csv(importance_otu.scale, "AWimportance_otu_scale.csv", row.names = FALSE)
################
