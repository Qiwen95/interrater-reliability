# install.packages("irr")
library(irr)

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

## Remove repeated codes for teams (SIGSCE paper)
codes <- codes[!(codes[,15]=="Y"),]

###
# Percent Agreement (use index 16,17 for original tags, to consider possibility
# of missing a quote)
###
print(agree(codes[,c(18,19)]))
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
print(kappa2(codes[,c(18,19)]))

###
# Summary Frequencies by Interaction Condition
###
noInter <- codes[!(codes["End.user.disability.interaction"]=="N"),]
noInterFreq <- table(noInter["Consensus.Tag"]) 
noInterFreqPop <- table(noInter["Final.Population.Subcategory"])

yesInter <- codes[!(codes["End.user.disability.interaction"]=="Y"),]
yesInterFreq <- table(yesInter["Consensus.Tag"]) 
yesInterFreqPop <- table(yesInter["Final.Population.Subcategory"])
print(nrow(codes[codes["Rater.Agreement"] == "N"]))
