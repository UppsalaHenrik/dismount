install.packages("portfolio")
install.packages("RColorBrewer")

library(RColorBrewer)
library(portfolio)

input<-read.csv("http://spatialanalysis.co.uk/wp-content/uploads/2011/08/tree_eg_data.csv")

compOFVsGroups <- read.csv("C:/Users/hnyberg/Dropbox/Doktorandsaker/PrecondProject/RealModels/Jonsson-S-2005-NXY059_original/OFVandGroupComparison.csv")

groups1and2 <- compOFVsGroups[compOFVsGroups$paraRetriesGroup == 1 || compOFVsGroups$paraRetriesGroup == 1][,c("retry", "paraRetriesOFV", "paraRetriesGroup")]

minOFV <- min(groups1and2$paraRetriesOFV)

groups1and2$minOFV <- ifelse(groups1and2$paraRetriesOFV <= minOFV + 1, 1, 0)



minCovMinOFV <- subset(groups1and2, paraRetriesGroup == 1 & minOFV == 1)
minCov <- subset(groups1and2, paraRetriesGroup == 1 & minOFV == 0)
minMinOFV <- subset(groups1and2, paraRetriesGroup == 2 & minOFV == 1)
minOnly <- subset(groups1and2, paraRetriesGroup == 2 & minOFV == 0)

df <- data.frame(c("Min \nCov \nminOFV","Min \nCov","Min \nminOFV", "Min Only"), 
           c(nrow(minCovMinOFV), nrow(minCov), nrow(minMinOFV), nrow(minOnly)), 
           c(1,1,1,1), c(0.75,-1,1,-0.75))
names(df) <- c("id", "area", "group", "color")

attach(df)

map.market(id, area, group, color, lab = c(FALSE, TRUE), main = "")
