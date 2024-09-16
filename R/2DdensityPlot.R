# Load necessary libraries
library(ggplot2)

# Input the data
data <- data.frame(
  Pathway = c("Carbon metabolism","2-Oxocarboxylic acid metabolism","Biosynthesis of amino acids","Biosynthesis of cofactors","Citrate cycle (TCA cycle)","Pyruvate metabolism","Glyoxylate and dicarboxylate metabolism","Propanoate metabolism","Butanoate metabolism","Sulfur metabolism","Sphingolipid metabolism","Cysteine and methionine metabolism","Lysine degradation","Arginine and proline metabolism","Tyrosine metabolism","Phenylalanine metabolism","Taurine and hypotaurine metabolism","Phosphonate and phosphinate metabolism","D-Amino acid metabolism","Glutathione metabolism","Thiamine metabolism","Nicotinate and nicotinamide metabolism","Pantothenate and CoA biosynthesis","Lipoic acid metabolism","Aminoacyl-tRNA biosynthesis","ABC transporters","Neuroactive ligand-receptor interaction","NOD-like receptor signaling pathway","Glucagon signaling pathway","Melanogenesis","Protein digestion and absorption","Mineral absorption","Chemical carcinogenesis - reactive oxygen species","Central carbon metabolism in cancer","Renal cell carcinoma","Cocaine addiction","Amphetamine addiction","Type II diabetes mellitus","Glycine, serine and threonine metabolism","Phenylalanine, tyrosine and tryptophan biosynthesis","Valine, leucine and isoleucine biosynthesis","Alanine, aspartate and glutamate metabolism","Valine, leucine and isoleucine degradation","Arginine biosynthesis","Ubiquinone and other terpenoid-quinone biosynthesis","beta-Alanine metabolism","Glycolysis / Gluconeogenesis","Porphyrin metabolism","Pyrimidine metabolism","Primary bile acid biosynthesis"),
  MetaboAnalyst = c(NA,
                    NA,
                    NA,
                    NA,
                    0.020758,
                    0.027086,
                    0.0050563,
                    0.024898,
                    0.15902,
                    NA,
                    0.049979,
                    3.98E-04,
                    NA,
                    0.061799,
                    0.0010206,
                    6.78E-05,
                    0.088042,
                    NA,
                    0.15902,
                    0.039127,
                    0.077451,
                    NA,
                    0.0012668,
                    0.039127,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    NA,
                    5.71E-10,
                    4.99E-06,
                    0.003318,
                    0.039127,
                    0.074489,
                    0.14921,
                    0.18782,
                    0.21568,
                    0.26012,
                    0.30217,
                    0.3648,
                    0.4152),
  My_R_Tool = c(0.000230597,
                0.000740012,
                2.59849E-10,
                0.00104998,
                0.004713402,
                0.011843962,
                0.004253221,
                0.019029926,
                0.001754263,
                0.00067682,
                0.014073383,
                0.000353105,
                0.034084205,
                0.005258066,
                0.000632441,
                4.0739E-06,
                0.00676025,
                0.034084205,
                3.49793E-08,
                0.016470477,
                1.64293E-05,
                0.002761766,
                1.43592E-05,
                0.021746527,
                4.58198E-09,
                0.000608867,
                0.03078885,
                0.031050646,
                0.007909981,
                0.031050646,
                6.04009E-13,
                2.71771E-07,
                0.003059237,
                5.10553E-12,
                0.015642426,
                0.036135253,
                0.046228009,
                0.031050646,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                NA)
)

# Remove rows with NA or non-finite values in either MetaboAnalyst or My_R_Tool
data_clean <- na.omit(data)

# Filter out rows with missing values
filtered_data <- data[complete.cases(data$MetaboAnalyst, data$My_R_Tool), ]

# Create the plot with the filtered data
ggplot(filtered_data, aes(x = MetaboAnalyst, y = My_R_Tool)) +
  geom_point(size = 3, alpha = 0.7) +  # scatter plot to show points
  stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +  # 2D density plot
  scale_fill_viridis_c() +  # color scale
  labs(title = "2D Density Plot of p-values: MetaboAnalyst vs My R Tool",
       x = "MetaboAnalyst p-values",
       y = "My R Tool p-values") +
  theme_minimal()
# Install ggExtra if not already installed
install.packages("ggExtra")

library(ggExtra)

# Create the scatter plot
p <- ggplot(filtered_data, aes(x = MetaboAnalyst, y = My_R_Tool)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot of p-values: MetaboAnalyst vs My R Tool",
       x = "MetaboAnalyst p-values",
       y = "My R Tool p-values") +
  theme_minimal()

# Add marginal histograms
ggMarginal(p, type = "histogram", fill = "lightblue")

