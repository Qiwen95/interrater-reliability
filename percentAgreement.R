# install.packages("irr")
library(irr)

############################################
## Functions
############################################
percentAgreement <- function(codes, consensusHeader, agreementHeader, node, returnVec){
  # Computes the overall agreement and agreement per node. Unlike irr agree() function,
  # will also count the values where a "rater failed to [initially] note the quote."
  # but where agreement was found in the final stages of coding.
  #
  # Args:
  #   codes: CSV file of all combined codes from raters
  #   consensusHeader: CSV header name of consensus tag ("Consensus.Tag")
  #   agreementHeader: CSV header name of agreement tag ("Rater.Agreement")
  #   node: All possible nodes (see NVivo file)
  #
  # Returns:
  # Prints the agreement per node and returns the overall agreement
  # 
  subsetCodes <- (codes[codes[consensusHeader] == node, ])
  if (nrow(subsetCodes)!=0){
    nodeTotal <- nrow(subsetCodes)
    nodeAgreement <- nrow(subsetCodes[subsetCodes[agreementHeader] == "Y",]) 
    missedCodes <- nrow(subsetCodes[subsetCodes["Rater.Agreement.Reasons"] == 
                                      "Rater failed to note the quote.",])
    if (!is.null(missedCodes)){
      nodeAgreement <- nodeAgreement+missedCodes
    }
    print(paste0(node,": ", nodeAgreement,"/", nodeTotal))
    returnVec[1] <- returnVec[1]+nodeAgreement
    returnVec[2] <- returnVec[2]+nodeTotal
  }
  return(returnVec)
}

KrippendorffSetup <- function(codes, rater, nodes, vec_results){
  #subsetCodes <- codes[rater]
 # for (node in nodes) {
 #   vec_results <- c(vec_results, nrow(subsetCodes[]))
 # }
  print("here")
}

############################################
## Main Execution
############################################
vec_results <- c(0,0)
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

###
# Percent Agreement
###
cat('\nPercentage Agreement:\n')
for (node in nodes) {
  vec_results <- percentAgreement(codes,"Consensus.Tag", "Rater.Agreement", 
                                    node, vec_results)
  if(node == tail(nodes, n=1)){
    cat('Overall agreement: ', vec_results[1], '/', vec_results[2],'\n\n')
  }
}

###
# Krippendorff's Alpha
###
vec_results <- c(0,0)

rater1 <- table(codes["Paulas.Tag"])
rater2 <- table(codes["Nidhis.Tag"])

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
print(kappa2(codes[,c(14,15)]))

