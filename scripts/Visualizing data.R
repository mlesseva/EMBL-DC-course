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
#add geometry layers(s)
#functions have predictable names
#geom_{point, line, bar, histogram, violin, hex...}
