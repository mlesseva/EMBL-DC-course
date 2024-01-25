# Create a "data" directory
dir.create("data_rnaseq")

# Download the data provided by your collaborator
# using a for loop to automate this step
for(i in c("counts_raw.csv", "counts_transformed.csv", "sample_info.csv", "test_result.csv")){
  download.file(
    url = paste0("https://github.com/tavareshugo/data-carpentry-rnaseq/blob/master/data/", i, "?raw=true"),
    destfile = paste0("data_rnaseq/", i)
  )
}

#create objects using tidyverse (load library tidyverse first)
library(tidyverse)

raw_cts <- read_csv("data_rnaseq/counts_raw.csv")
trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")
test_result <- read_csv("data_rnaseq/test_result.csv")

#distribution plot, first convert to long format (working with transformed data)
trans_cts_long <- trans_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)

#first combine tables sample info and transformed long (join based on common columns)

trans_cts_long <- full_join(trans_cts_long, sample_info, by = "sample")

trans_cts_long %>% 
  ggplot(aes(x = cts)) + 
  geom_freqpoly()

trans_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly(binwidth = 1)

#to separate by strain use facet grid function
trans_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

#challenge: do the same figure with the raw data
raw_cts_long <- raw_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)

raw_cts_long <- full_join(raw_cts_long, sample_info, by = "sample")

raw_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute)) +
  scale_x_log10()
#or log like this:
raw_cts_long %>% 
  ggplot(aes(x = log10(cts), colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))

#but log10(0) is infinity in R so it gives a warning. if the warning is "missing values" then it means there are NA values
raw_cts_long %>% 
  ggplot(aes(x = log10(cts + 1), colour = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

#for boxplots you need to define x and y. Minute has to become a factor, not numerical.
raw_cts_long %>% 
  ggplot(aes(x = factor(minute), y = log10(cts + 1), fill = strain)) +
  geom_boxplot() +
  facet_grid(cols = vars(replicate)) 

raw_cts_long %>% 
  ggplot(aes(x = factor(minute), y = log10(cts + 1), fill = replicate)) +
  geom_boxplot() +
  facet_grid(cols = vars(strain)) 

#correlations between samples; scatterplot wt time 0 and 30 (use the original wide table, because those columns exist there)
trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_30_r1)) +
  geom_point()

trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_30_r1)) +
  geom_point() + 
  geom_abline(colour = "brown")

trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point() + 
  geom_abline(colour = "brown")

trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_180_r1)) +
  geom_point() + 
  geom_abline(colour = "brown")

#to look at correlations of cts across all samples (pairwise); first remove non-numericals
#creates a matrix in table form
trans_cts_corr <- trans_cts %>%
  select(-gene) %>% 
  cor(method = "spearman")
 
#make heatmap
install.packages("corrr") 
  
library(corrr)  

rplot(trans_cts_corr)

rplot(trans_cts_corr) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#compare trans_cts and raw_cts
summary(raw_cts_long$cts)
summary(trans_cts_long$cts)

raw_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point()

raw_cts %>% 
  ggplot(aes(x = wt_0_r1 + 1, y = wt_0_r2 + 1)) +
  geom_point() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

#to space the samples out (use the long table format)
raw_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts))

raw_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() +
  geom_abline(colour = "brown") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

#but our data is already transformed

trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point() +
  geom_abline(colour = "brown") +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  ggplot(aes(x = mean_cts, y = var_cts)) +
  geom_point()


#student question
trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts = mean(cts), var_cts = var(cts)) %>% 
  mutate(above_four = var_cts > 4) %>% 
  ggplot(aes(x = mean_cts, y = var_cts, colour = above_four)) +
  geom_point()






















  
  
  
  
  
  








































