# interrater-reliability
Inter-rater reliability calculations (percentage agreement, Cohen’s kappa, and Krippendorff’s Alpha) for NSF-Ethics research and SIGSCE submission.

# Citation
If you reference this content, please cite:
- Garcia, P. 2017. Inter-rater Reliability. Github repository. https://github.com/paulaux/interrater-reliability

# Requirements
- QualitativeCoding.csv (not included due to sensitive information)
- irr.r package. Install by using: install.packages("irr")

# Notes
All codes used in this research are nominal. There are limitations to each calculation method:

1. Percentage Agreement: This is a good measure for obtaining an overview of the rater’s agreement. Percentage agreement does not consider the probability of the code happening by chance (e.g., a rater’s bias towards a particular category/node when coding). To consider these probabilities, use Cohen’s Kappa or Krippendorff’s Alpha. Both methods assume that the definitions of each node/category are very distinct (therefore independent in their probability of occurring).

2. Cohen’s Kappa: Considers the probability of the raters choosing a particular category but disregards the probability of disagreement between raters. When disagreement between raters is high, Krippendorff’s Alpha is more informative.

3. Krippendorff’s Alpha: Considers the probability of agreements and disagreements between raters. Krippendorff’s alpha assumes that all nodes/categories are equally likely to emerge in the data. If there are outliers in the frequencies of one of more codes, then Cohen’s kappa is more informative.