calculateAgreement <- function(codes, consensusHeader, agreementHeader, node, vec){
  # Computes the overall agreement and agreement per node
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
    print(paste0(node,": ", nodeAgreement,"/", nodeTotal))
    vec[1] <- vec[1]+nodeAgreement
    vec[2] <- vec[2]+nodeTotal
  }
  return(vec)
}

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

for (node in nodes) {
  vec_results <- calculateAgreement(codes,"Consensus.Tag", "Rater.Agreement", 
                                    node, vec_results)
  if(node == tail(nodes, n=1)){
    print(paste0('Overall agreement: ', vec_results[1], '/', vec_results[2]))
  }
}

