plt <- ggplot(data = surveys_complete, 
              mapping = aes(x = weight, 
                            y = hindfoot_length))

plt()
str(plt)
plt + geom_point()

plt + geom_point() + 
  ggtitle("My first plot")

#define ggplot object
#plt <- ggplot(data = <data.frame>, mapping = <aesthetics>)
#x aesthetics
#y aesthetics
#color aesthetics
#shape aesthetics
#..

#add geometry layers(s)
#functions have predictable names
#geom_{point, line, bar, histogram, violin, hex...}

plt + ggtitle("Weight vs Hindfoot length")
#layering
install.packages("hexbin")
library(hexbin)
ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) + geom_hex()

#alpha corresponds to transperancy                                                                                            
ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) + geom_point(alpha = 0.1)

ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) + geom_point(alpha = 0.1, color = "blue")

ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length)) + geom_point(alpha = 0.1, aes(color = species_id))

ggplot(data = surveys_complete, 
       mapping = aes(x = weight, y = hindfoot_length, color = species_id)) + geom_point(alpha = 0.25)
  
#challenge scaterplot wieght vs species_id  
  
ggplot(data = surveys_complete, mapping = aes(
  x = species_id, 
  y = weight, 
  color = plot_type)) + geom_point() 

#boxplots  

ggplot(data = surveys_complete, mapping = aes(
  x = species_id, 
  y = weight)) + geom_boxplot() 

#jitter adds a little value for each x coordinate
ggplot(data = surveys_complete, mapping = aes(
  x = species_id, 
  y = weight)) + geom_boxplot() + geom_jitter(alpha = 0.20, color = "salmon")

ggplot(data = surveys_complete, mapping = aes(
  x = species_id, 
  y = weight)) + geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.25, color = "salmon")

ggplot(data = surveys_complete, mapping = aes(
  x = species_id, 
  y = weight)) + geom_jitter(alpha = 0.25, color = "green") + geom_boxplot(outlier.shape = NA, fill = NA)

#challenge produce a violin plot of weight by species_id

ggplot(data = surveys_complete, mapping = aes(
  x = species_id,
  y = weight
)) + geom_violin() + scale_y_log10() + ylab("Weight (log10)")

#challenge create boxplot _ jittered scatterplot of hindfoot_length by species_id. Boxplot in front of dots and filled with white
ggplot(data = surveys_complete, mapping = aes(
  x = species_id,
  y = hindfoot_length
)) + geom_jitter(alpha = 0.2, color = "firebrick") + geom_boxplot(outlier.shape = NA)

ggplot(data = surveys_complete, mapping = aes(
  x = species_id,
  y = hindfoot_length
)) + geom_jitter(alpha = 0.2, aes(color = plot_id)) + geom_boxplot(outlier.shape = NA)

ggplot(data = surveys_complete, mapping = aes(
  x = species_id,
  y = hindfoot_length
)) + geom_jitter(aes(color = plot_id)) + geom_boxplot(outlier.shape = NA)

ggplot(data = surveys_complete, mapping = aes(
  x = species_id,
  y = hindfoot_length
)) + geom_jitter(aes(color = factor(plot_id))) + geom_boxplot(outlier.shape = NA)

#redefining surveys complete
yearly_count <- surveys_complete %>%
  count(year, genus)

#single line for each genus
ggplot(data = yearly_count, mapping = aes(
  x = year,
  y = n,
  group = genus
)) + geom_line()
#color each genus differently
ggplot(data = yearly_count, mapping = aes(
  x = year,
  y = n,
  color = genus
)) + geom_line()



















































