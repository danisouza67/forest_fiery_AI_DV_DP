install.packages("plotly")
install.packages("pillar")
install.packages("skimr")

ff_dataset <- read.csv(file = "forestfires.csv", stringsAsFactors = FALSE)
View(ff_dataset)

#Skimr
library(skimr)
library(dplyr)
skimr::skim(ff_dataset)
glimpse(ff_dataset)

#outliers

complete.cases(ff_dataset)
hist(ff_dataset$DMC)
boxplot(ff_dataset$FFMC)

#noutlier_ff <- subset(ff_dataset, select = c('FFMC','DMC','DC','ISI','temp','RH','wind','rain','area'))

#' Detect outliers using IQR method
#' @param x A numeric vector
#' @param na.rm Whether to exclude NAs when computing quantiles
is_outlier <- function(x, na.rm = FALSE) {
  qs = quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  
  lowerq <- qs[1]
  upperq <- qs[2]
  iqr = upperq - lowerq 
  
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  
  # Return logical vector
  x > extreme.threshold.upper | x < extreme.threshold.lower
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!is_outlier(df[[col]]),]
  }
  df
}

#creating data set without outliers in it
vars_of_interest <- c("FFMC","DMC","DC","ISI","temp","RH","wind","rain","area")
noutlier_ff <- remove_outliers(ff_dataset, vars_of_interest)
noutlier_ff


 #Hot Encoding
install.packages("mltools")
library(magrittr)
library(caret)
library(mltools)
library(data.table)
dummy_data <- subset(ff_dataset, select = c(X,Y,month,day,ISI))
oneHotData <- one_hot(as.data.table(dummy_data))
dummy <- dummyVars(" ~ .", data = oneHotData)
newData <- data.frame(predict(dummy, newdata = oneHotData))
library(reshape2)
newData <- dcast(data = oneHotData, ISI ~ day, length)
newData <- dcast(data = oneHotData, X ~ day, length)


#-----------EDA-------------
install.packages("ggplot2")
library(ggplot2)
install.packages("devtools")
library(devtools)
install_github("ujjwalkarn/xda")
#numSum is a function from xda library, with that we can see useful information for our EDA as a result, as such:
# mean, Standar Deviation, max-min value, range, number unique, quantity of zeros, mode, kurtosis and skewness, missing values, etc
library(xda)
numSummary(ff_dataset)
#charSummary is great for categorical variables we can see information about the counting of the values, missing values, 
charSummary(ff_dataset)
bivariate(ff_dataset, 'FFMC', 'DMC')

#check variation
var(ff_dataset)
#check quantile values (Numeric values)
quantile(ff_dataset$FFMC)
quantile(ff_dataset$DMC)
quantile(ff_dataset$DC)
quantile(ff_dataset$ISI)
#

#Scatterplot for 4 variables, FFMC, DMC, DC and area comparing with ISI values
library("ggpubr")
ggarrange(ggplot(noutlier_ff, aes(x = FFMC, y = ISI)) + geom_point() + ggtitle("Scaterplot comparing FFMC with ISI") + coord_cartesian(ylim = c(1,25)),
          ggplot(noutlier_ff, aes(x = DMC, y = ISI)) + geom_point() + ggtitle("Scaterplot comparing DMC with ISI") + coord_cartesian(ylim = c(1,25)),
          ggplot(noutlier_ff, aes(x = DC, y = ISI)) + geom_point() + ggtitle("Scaterplot comparing DC with ISI") + coord_cartesian(ylim = c(1,25)),
          ggplot(noutlier_ff, aes(x = area, y= ISI)) + geom_point() + ggtitle("Scaterplot comparing Area with ISI") + coord_cartesian(ylim = c(1,25)),
          ncol = 2, nrow = 2)
#Bar Chart for month
ggplot(data = noutlier_ff)+
  geom_bar(mapping = aes(x = month)) + ggtitle("Bar Chart for Categorical variable 'Month' comparing with fire cases")
#Bar Chart for day
ggplot(data = ff_dataset)+
  geom_bar(mapping = aes(x = day)) + ggtitle("Bar Chart for Categorical variable 'DAY'")
#Histogram for FFMC with binwidth
ggplot(data = noutlier_ff)+
  geom_histogram(mapping = aes(x = FFMC), binwidth = 5, color = "black", fill = "gray") + ggtitle("Histogram for FFMC with binwidth")
#COVARITATION Freqpoly for DMC rates counting by week days
ggplot(data = ff_dataset, mapping = aes(x=DMC, color = day))+
  geom_freqpoly(binwidth = 0.1)+ggtitle("Freqpoly for DMC rates counting by week days")
ggplot(data = ff_dataset, mapping = aes(x = DC)) +
  geom_freqpoly(mapping = aes(color = day), binwidth = 500)	+ggtitle("Freqpoly for DC rates counting by week days")
#Barplot counting ISI rates by day of the week
barplot(height=ff_dataset$ISI, names=ff_dataset$day, col=coul, main="Barplot counting ISI rates by day of the week", xlab="Week Days", ylab="ISI rate") 
#-------------------------------------

#normalizing cols FFMC, DMC, DC, ISI
#Min Max Norm
normaData <- function(x){
  return((x-min(x)) / (max(x) - min(x)))
}
ff_dataset$ISI_Norm <- normaData(ff_dataset$ISI)
ff_dataset$FFMC_Norm <- normaData(ff_dataset$FFMC)
ff_dataset$DMC_Norm <- normaData(ff_dataset$DMC)
ff_dataset$DC_Norm <- normaData(ff_dataset$DC)
ggplot(ff_dataset, aes(x=FFMC_Norm, y=ISI_Norm))+geom_point()


#Z-Score
ff_dataset$FFMC_Scaled <- scale(ff_dataset$FFMC)
ff_dataset$DMC_Scaled <- scale(ff_dataset$DMC)
ff_dataset$DC_Scaled <- scale(ff_dataset$DC)
ff_dataset$ISI_Scaled <- scale(ff_dataset$ISI)
ggplot(ff_dataset, aes( x = DMC_Scaled, y = ISI_Scaled )) + geom_point()

#Robust Scalar
robust_scalar <- function(x){
  (x - median(x)) / (quantile(x, probs = .75) - quantile(x, probs = .25))
}
ff_dataset$FFMC_RobustS <- robust_scalar(ff_dataset$FFMC)
ff_dataset$DMC_RobustS <- robust_scalar(ff_dataset$DMC)
ff_dataset$DC_RobustS <- robust_scalar(ff_dataset$DC)
ff_dataset$ISI_RobustS <- robust_scalar(ff_dataset$ISI)
ggplot(ff_dataset, aes( x = DC_RobustS, y = ISI_RobustS )) + geom_point()

#reorganizing the datasets
indexes_data <- subset(ff_dataset, select = c(FFMC, DMC, DC, ISI))
full_ff <- ff_dataset
ff_dataset <- subset(full_ff, select = -c(FFMC_Norm, DMC_Norm, DC_Norm, FFMC_Scaled, DMC_Scaled, DC_Scaled, FFMC_RobustS, DMC_RobustS, DC_RobustS, ISI_RobustS, ISI_Scaled, ISI_Norm))
robust_ff <- subset(full_ff, select = c(FFMC_RobustS, DMC_RobustS, DC_RobustS, ISI_RobustS))
scaled_ff <- subset(full_ff, select = c(FFMC_Scaled, DMC_Scaled, DC_Scaled, ISI_Scaled))
norm_ff <- subset(full_ff, select = c(FFMC_Norm, DMC_Norm, DC_Norm, ISI_Norm))
#---------------------------------------

#Line Plot 
#variable for colors
colors <- c("FFMC" = "green", "DMC" = "red", "DC" = "blue")
#Line plot analysing FFMC, DMC and DC against ISI, in Z-score Scale
ggplot(full_ff)+
  geom_line( mapping = aes(x = ISI_Scaled, y = FFMC_Scaled, color = "FFMC")) +
  geom_point( mapping = aes(x = ISI_Scaled, y = FFMC_Scaled, color = "FFMC")) +
  geom_line( mapping = aes(x = ISI_Scaled, y = DMC_Scaled, color = "DMC")) +
  geom_point( mapping = aes(x = ISI_Scaled, y = DMC_Scaled, color = "DMC"))+
  geom_line( mapping = aes(x = ISI_Scaled, y = DC_Scaled, color = "DC")) +
  geom_point( mapping = aes(x = ISI_Scaled, y = DC_Scaled, color = "DC"))+
  labs(x = "ISI (Scaled)", y = "FFMC, DMC and DC (Scaled)", color = "Legend", title = "Line Plot comparing FFMC, DMC and DC ratings against ISI rates (Scaled)")+
  theme( legend.position = c(.71, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6))+
  scale_color_manual(name = "Colors",values = colors)

#Line plot analysing FFMC, DMC and DC against ISI rates, in Robust Scalar
ggplot(full_ff)+
  geom_line( mapping = aes(x = ISI_RobustS, y = FFMC_RobustS, color = "FFMC")) +
  geom_point( mapping = aes(x = ISI_RobustS, y = FFMC_RobustS, color = "FFMC")) +
  geom_line( mapping = aes(x = ISI_RobustS, y = DMC_RobustS, color = "DMC")) +
  geom_point( mapping = aes(x = ISI_RobustS, y = DMC_RobustS, color = "DMC"))+
  geom_line( mapping = aes(x = ISI_RobustS, y = DC_RobustS, color = "DC")) +
  geom_point( mapping = aes(x = ISI_RobustS, y = DC_RobustS, color = "DC"))+
  labs(x = "ISI (Robust Scalar)", y = "FFMC, DMC and DC (Robust Scalar)", color = "Legend", title = "Line Plot comparing FFMC, DMC and DC ratings against ISI rates (Robust Scalar)")+
  theme( legend.position = c(.71, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6))+
  scale_color_manual(name = "Colors",values = colors)+
  coord_cartesian(ylim = c(-10,10), xlim = c(-2,4))

#Scatterplot to see correlation between FFMC and DMC (robust scalar), 
library(ggplot2)
ggplot(full_ff, aes(x= FFMC_RobustS, y = DMC_RobustS)) + geom_point() + geom_smooth(formula = y ~ poly(x,2), method = "lm")



#Correlation, corrplot and HeatMap
#install.packages("reshape")
library(reshape)
cor(indexes_data)
cor.test(indexes_data$FFMC, indexes_data$DMC, method = "pearson")
cor.test(indexes_data$FFMC, indexes_data$DMC, method = "kendall")
m_corr_data <- cor(indexes_data)
#corrplot
install.packages("corrplot")
library(corrplot)
corrplot(m_corr_data, method = "number", type = "upper")
#Heat Map
color_heatmap <- colorRampPalette(c("cyan", "deeppink3"))
heatmap(as.matrix(m_corr_data[,-1]), Rowv = NA, Colv = NA, col = color_heatmap(100))

#Stacked Bar Chart of FFMC fires by Day
noutlier_ff %>%
  ggplot(ylim = c(0,1500))+
  geom_bar(mapping = aes(x=FFMC, fill = day), color = "black")+
  labs(title = "Stacked Bar Chart of FFMC fires by Day", x="Number of fires (FFMC)", y="Weekday")+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size = 14))



#-----------------PCA---------------------------
# Code extrated from: https://www.youtube.com/watch?v=hmp9KIPb5Ig&ab_channel=CARVALHORIBEIRO by CARVALHORIBEIRO
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)

#generating PCA
res.pca <- PCA(scaled_ff, graph = F)
View(res.pca)
#Extract the proportion of variance from principal component values
eig.val <- get_eigenvalue(res.pca)
eig.val
#Bar Plot the eigenvalues | Visualize eigenvalues/variances
fviz_screeplot(res.pca,addlabels=TRUE)
barplot(eig.val[, 2], names.arg=1:nrow(eig.val), 
        main = "Variances by Component",
        xlab = "Principal Components",
        ylab = "% of variances",
        col ="goldenrod3")
#Graphic plot showing variance proportion of each variable
fviz_eig(res.pca, addlabels = T, ylim = c(0,90))
#Extract results from PCA variables to use in the graphic plot
var <- get_pca_var(res.pca)
ind <- get_pca_ind(res.pca)
#Plot PCA Graph
fviz_pca_var(res.pca, col.var = "blue")
#Creating Cluster group
groupCluster <- as.factor(ff_dataset[,6])
#Plot of Graph Biplot
fviz_pca_biplot(res.pca, habillage = groupCluster, title = "Graph PCA of Forest Fiery")
#Additional Plot to check the quality of the representation with COS2 and Graph
library(corrplot)
var$cos2
corrplot(var$cos2, is.corr = F)

#Code extrated from week9_Data Exp Class by Muhammad
# Plot for individuals
ind$cos2
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage="none", 
         col.ind="#0000ff", col.ind.sup="blue", col.quali="magenta", label=c("ind","ind.sup", "quali"),new.plot=TRUE, title=" Individuals Factor Map")



# Control variable colors using their contributions
# Use red and blue
library(ggplot2)
fviz_pca_var(res.pca, col.var="contrib") +
  scale_color_gradient2(low="red",high="blue",midpoint = 90) +
  theme_minimal()

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))}
  if (numPlots==1) {
    print(plots[[1]]) } 
  
  else {# Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    } } }
# Variable contributions on axes 1 + 2
p1<-fviz_contrib(res.pca, choice="var", axes = 1:2)
# Individuals contributions on axes 1 + 2
p2<-fviz_contrib(res.pca, choice="ind", axes = 1:2)
multiplot(p1, p2,cols=2)

#Another customized plot, this time to view Individuals (ind)
resVG <- prcomp(scaled_ff[, -5],  scale = TRUE)
# Add Color by groups
p <- fviz_pca_ind(resVG, geom = "point",
                  habillage=ff_dataset$day, addEllipses=TRUE,
                  ellipse.level= 0.90)+ theme_minimal()
p

#Performs Principal Component Analysis (PCA) a Shiny application.
install.packages('Factoshiny')
library(Factoshiny)
PCAshiny(scaled_ff) 
#-----------------------------------------------

