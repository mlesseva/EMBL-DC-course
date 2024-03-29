#Renato starts here
#PCA analysis (dimensionality reduction technique)
#t is used to transpose matrix
#as.matrix is used to convert a dataframe to a matrix

library(tidyverse)

trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")

pca_matrix <- trans_cts %>% 
  column_to_rownames("gene") %>% 
  as.matrix() %>% 
  t()

sample_pca <- prcomp(pca_matrix)
#explore the data
class(sample_pca)
str(sample_pca)
summary(sample_pca)

#see 10 rows and 5 columns of the matrix
pca_matrix[1:10, 1:5]

as_tibble(pca_matrix)
as_tibble(pca_matrix, rownames = "sample")

#Eigen values as sdev
pc_eigenvalues <- sample_pca$sdev^2

#create a tibble by hand
pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)),
                         varience = pc_eigenvalues) %>% 
  mutate(pct = varience/sum(varience)*100) %>% 
  mutate(pct_cum = cumsum(pct))

#create a pareto plot/ pareto chart
pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) +
  geom_point(aes(y = pct_cum)) +
  geom_hline(yintercept = 90) +
  labs(x = "principal component", y = "fraction of variance explained")

#visualize PCs
pc_scores <- sample_pca$x %>% 
  as_tibble(rownames = "sample")

pc_scores %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()
 
pc_scores %>% 
  full_join(sample_info, by ="sample") %>% 
  ggplot(aes(x = PC1, y = PC2, color = minute, shape = strain)) +
  geom_point()

pc_scores %>% 
  full_join(sample_info, by ="sample") %>% 
  ggplot(aes(x = PC1, y = PC2, color = factor(minute), shape = strain)) +
  geom_point()


pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames = "gene")

 top_genes <- pc_loadings %>% 
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loading))) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
   unique()

 top_loadings <- pc_loadings %>% 
   filter(gene %in% top_genes)

ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
                arrow = arrow(length = unit(0.1, "in")),
                color = "brown") +
                 geom_text(aes(x = PC1, y = PC2, label = gene),
                           nudge_y = 0.005, size = 3) +
                 scale_x_continuous(expand = c(0.02, 0.02))

#Day5
#Execute the whole page again using the Source button
#store the plot in variable loadings_plot

loadings_plot <- ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")),
               color = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

pca_plot <- pc_scores %>% 
  full_join(sample_info, by ="sample") %>% 
  ggplot(aes(x = PC1, y = PC2, color = factor(minute), shape = strain)) +
  geom_point()

#align plots side by side
library(patchwork)
(pca_plot | loadings_plot)
#align one plot above the other
(pca_plot / loadings_plot)

#to have multiple plots in a figure
(pca_plot | pca_plot | pca_plot / loadings_plot)

(pca_plot | pca_plot | pca_plot / loadings_plot) +
  plot_annotation(tag_levels = "A")
#automated way to make plots instead of manually writing code
library(ggfortify)
autoplot(sample_pca)

autoplot(sample_pca, data = sample_info, 
         colour = "minute", shape = "strain")

#making the tibble from before (with mutate function) can be automated with the tidy function
library(broom)
tidy(sample_pca, matrix = "eigenvalues")

tidy(sample_pca, matrix = "loadings")

#Francesco

autoplot(sample_pca,
         data = sample_info %>% mutate(minute = as.factor(minute)),
         colour = "minute",
         shape = "strain")


#Differential expression analysis results
test_results <- read_csv("data_rnaseq/test_result.csv")

test_results
#gene column -> gene name
#baseMean column -> normalized expression level of a gene
#raw read counts
#log2Fold change column-> amount of change btw two conditions
#lfcSE -> stanrat error associated with log2Fold change
#stat column -> stats value computed as log2Fold Change / lfcSE compared to standard normal distribution
#p-value -> p value associated with change
#p-adj -> corrected p-value for multiple hypothesis testing
#comparisons -> multiple comparison group for timepoints


#MA (from microarray era) plot
#challenge
test_results %>% 
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  facet_wrap(facets = vars(comparison))

#significance testing  
test_results %>% 
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) %>% 
  ggplot(aes(x= log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_point(aes(y = sig), color = "brown", size = 1) +
  geom_hline(yintercept = 0, colour = "dodgerblue") +
  facet_wrap(facets = vars(comparison))


ma_plot <- test_results %>% 
  mutate(sig = ifelse(padj < 0.01, log2FoldChange, NA)) %>% 
  ggplot(aes(x= log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_point(aes(y = sig), color = "brown", size = 1) +
  geom_hline(yintercept = 0, colour = "dodgerblue") +
  facet_wrap(facets = vars(comparison))


pca_plot <- autoplot(sample_pca,
         data = sample_info %>% mutate(minute = as.factor(minute)),
         colour = "minute",
         shape = "strain")

#combine plots
(ma_plot | pca_plot)

#Visualizing expression trends; plot candidate genes
# get candidate genes (padj < 0.01); test_results[, "gene] aka test_results$gene

candidate_gene <- test_results %>% 
  filter(padj < 0.01) %>% 
  pull(gene) %>%
  unique()

#1. get trans_cts_long table

trans_cts_long <- trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample",
               values_to = "cts") %>% 
  full_join(sample_info, by = "sample")

#2. filter trans_cts_long for candidate genes and compute mean expression value for each gene and timepoint and genotype
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_gene) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts = mean(cts), nrep = n()) %>% 
  ungroup()

#3 plot trends
trans_cts_mean %>% 
  ggplot(aes(x = minute, y = mean_cts)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  facet_grid(rows = vars(strain))

#Scaling data to improve the plot  
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_gene) %>% 
  group_by(gene) %>% 
  mutate(cts_scaled = (cts - mean(cts)) / sd(cts)) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts_scaled = mean(cts_scaled), 
            nrep = n()) %>% 
  ungroup()

trans_cts_mean %>% 
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(rows = vars(strain))

#Clustering analysis (supervised vs unsupervised)
#create a distance matrix of ccounts
trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")

hclust_matrix <- trans_cts %>% 
  select(-gene) %>% 
  as.matrix()

#assign rownames
rownames(hclust_matrix) <- trans_cts$gene

hclust_matrix <- hclust_matrix[candidate_gene, ]

#transpose the z-scores over the rows (because the scale function calculates z-scores over columns), then transpose back (columns into rows, rows into columns)
#apply scalling to each column of the matrix (genes)
hclust_matrix <- hclust_matrix %>% 
  t() %>% 
  scale() %>% 
  t()
#to check dimensions
dim(hclust_matrix)

#calculate pairwise distances btw genes
gene_dist <- dist(hclust_matrix)

#hierarchical clustering
gene_hclust <- hclust(gene_dist, method = "complete")

plot(gene_hclust, labels = F)
abline(h = 10, col = "brown", lwd = 2)

#make clusters based on a cutoff (gene names will be associated with a number)
cutree(gene_hclust, k = 5)

gene_cluster <- cutree(gene_hclust, k = 5) %>% 
  enframe() %>% 
  rename(gene = name, cluster = value)

trans_cts_cluster <- trans_cts_mean %>% 
  inner_join(gene_cluster, by = "gene")

trans_cts_cluster %>% 
  ggplot(aes(x = minute, y = mean_cts_scaled)) +
  geom_line(aes(group = gene)) +
  facet_grid(cols = vars(cluster), rows = vars(strain)) 


dev.off()


BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)

Heatmap(hclust_matrix, show_row_names = F)


























































 

































































































































































































