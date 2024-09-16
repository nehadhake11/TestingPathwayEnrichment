library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Fisher's Exact Test function
fisher_test <- function(allMetabolitesSet, pathwayMetabolites, inputMetabolites, pathway) {
  pathwayMetabolites = unlist(strsplit(pathwayMetabolites, ","))
  a = length(intersect(pathwayMetabolites, inputMetabolites))
  b = length(setdiff(inputMetabolites, pathwayMetabolites))
  c = length(setdiff(pathwayMetabolites, inputMetabolites))
  d = length(setdiff(allMetabolitesSet, union(inputMetabolites, pathwayMetabolites)))

  contingency_table <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
  rownames(contingency_table) <- c("In Input Metabolite Set", "Not in Input Metabolite Set")
  colnames(contingency_table) <- c("In Pathway", "Not in Pathway")

  fisher_test_result <- fisher.test(contingency_table, alternative = "two.sided")

  return(list(
    pathwayName = pathway,
    fisherResult = fisher_test_result,
    contingency_table = contingency_table,
    p_value = fisher_test_result$p.value,
    log_p_value = -log10(fisher_test_result$p.value)
  ))
}

# Read data
PathwayVsMetabolites <- read.csv("/Users/nehadhake/Moffit/PathwayEnrichmentTesting/resources/Human-PathwaysVsMetabolites.csv")

# Preprocess data
data <- PathwayVsMetabolites %>%
  mutate(Metabolites = strsplit(Metabolites, ",")) %>%
  unnest(Metabolites)

allMetabolitesSet <- unique(data$Metabolites)
inputMetabolites <- c("C00164" , "C00099" , "C00300" , "C01026" , "C00122" , "C00037" , "C00155" , "C00097" , "C00079" , "C00065" , "C00188" , "C00082" , "C00183" , "C00166" , "C00163" , "C00022" , "C00213" , "C06269" , "C01407" , "C01481" , "C01413" , "C12144" , "C01755")

# Perform Fisher's Exact Test on all pathways
results <- list()
for (i in 1:nrow(PathwayVsMetabolites)) {
  row <- PathwayVsMetabolites[i, ]
  pathway <- row$"Pathway"
  pathwayMetabolites <- row$"Metabolites"
  result = fisher_test(allMetabolitesSet, pathwayMetabolites, inputMetabolites, pathway)
  results[[i]] <- result
}

# Compile results into a dataframe
results_df <- do.call(rbind, lapply(results, function(res) {
  data.frame(
    Pathway = res$pathwayName,
    P_value = res$p_value,
    Log_P_value = res$log_p_value
  )
}))

# Define significance threshold and filter significant hits
significance_threshold <- 0.05
significant_results_df <- results_df %>%
  filter(P_value < significance_threshold)

# View significant results
print(significant_results_df)

# Save all results to Excel
write_xlsx(results_df, path = "/Users/nehadhake/Moffit/PathwayEnrichmentTesting/Results/results_dfMetaboAnalystExampleData.xlsx")

# Save only significant results to Excel
write_xlsx(significant_results_df, path = "/Users/nehadhake/Moffit/PathwayEnrichmentTesting/Results/significant_results_dfMetaboAnalystExampleData.xlsx")

# Plot significant pathways based on -log10(p-value)
ggplot(significant_results_df, aes(x = reorder(Pathway, -Log_P_value), y = Log_P_value)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Significantly Enriched Pathways",
       x = "Pathway",
       y = "-log10(P-value)") +
  theme_minimal()
