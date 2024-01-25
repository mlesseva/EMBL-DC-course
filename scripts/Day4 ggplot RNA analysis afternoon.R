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






































