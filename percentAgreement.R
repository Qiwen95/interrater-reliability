# install.packages("irr")
library(irr)
########################
## Functions
########################

countCodes <- function(codesByCondition, nodeNames){
  # Generates frequency counts for any given number of codes
  #
  # Args:
  #    codesByCondition: matrix of nxm codes by interaction condition.
  #    nodeNames: vector of node names used by researchers to categorize the data.
  overallCodeFreq <- table(codesByCondition["Consensus.Tag"])
  overallSubPopFreq <- table(codesByCondition["Final.Population.Subcategory"])
  for (node in nodeNames){
    tempCodeByInter <- (codesByCondition[codesByCondition["Consensus.Tag"] == node ,])
    subPopByCatFreq <- table(tempCodeByInter["Final.Population.Subcategory"])
  }
  return(c(node, overallCodeFreq,overallSubPopFreq, subPopByCatFreq))
}

########################
## Main execution
########################

## ALL NODES:
nodes <- c("A person not on the team made a suggestion",
           "A person not on the team pointed out a concern",
           "Accessibility considered but not addressed",
           "Accessibility MIGHT be considered in this situation",
           "Original design of software included inherit accessibility components",
           "Revising of software to consider accessibility",
           "Site customization allows for accessibility",
           "Students explicitly discuss accessibility",
           "Target market includes a group requiring accessibility",
           "Vision goal or purpose focused on accessibility")

codes <- read.csv(file="QualitativeCoding.csv", header=TRUE, sep=",")

########################
## SIGSCE CUSTOM CODE
########################
## Overwrites variables 'codes' and 'nodes'

## Keep non-repeated team codes (SIGSCE paper)
codes <- codes[(codes[,16]=="N"),]

## Analyze only relevant nodes for SIGSCE paper
nodes <- c("Accessibility considered but not addressed",
           "Students explicitly discuss accessibility")
codes_rater1 <- codes[ codes[, "Paulas.Final.Tag"] %in% nodes, ]
codes_rater2 <- codes[ codes[, "Nidhis.Final.Tag"] %in% nodes, ]
codes <- rbind(codes_rater1, codes_rater2)
codes <- subset(codes, !duplicated(codes[, "Index"])) 
write.csv(codes, file = "SIGSCEcodes.csv")

## End of SIGSCE customization
########################

###
# Percent Agreement (use index 17,18 for original tags, to consider possibility
# of missing a quote)
###
print(agree(codes[,c(19,20)]))
cat("\n")

## Per category agreement
for (node in nodes) {
  subsetCodes <- (codes[codes["Consensus.Tag"] == node ,])
  if (nrow(subsetCodes) !=0 ){
    print(paste0(node, ": ", nrow(subsetCodes[subsetCodes["Rater.Agreement"] 
                                              == "Y",]),"/",nrow(subsetCodes)))
  }
}

cat("\n")

###
# Krippendorff's Alpha
###
vec_results <- c(0,0)

rater1 <- table(codes["Paulas.Final.Tag"])
rater2 <- table(codes["Nidhis.Final.Tag"])

## initialize as NA
x_kipp <- matrix(data=NA, nrow=2, ncol=length(nodes))
for(node in nodes){
  if(node %in% names(rater1)){
    x_kipp[1, which(nodes==node)] <- rater1[[node]][1]
  }
  if (node %in% names(rater2)){
    x_kipp[2, which(nodes==node)] <- rater2[[node]][1]
  }
}
print(kripp.alpha(x_kipp))

###
# Cohens Kappa
###
cat("\n")
print(kappa2(codes[,c(19,20)]))

###
# Summary Frequencies by Interaction Condition
###
noInter <- codes[(codes["Exposure.to.an.individual.with.disabilities"]=="N"),]
noInterFreq <- countCodes(noInter, nodes)

yesInter <- codes[(codes["Exposure.to.an.individual.with.disabilities"]=="Y"),]
yesInterFreq <- countCodes(yesInter, nodes)
