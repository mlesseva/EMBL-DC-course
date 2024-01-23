weight_g <- c(50, 60, 65, 82)
animals <- c("mouse", "rat", "dog")

length(animals)
length(weight_g)
# get length of vector

class(animals)
class(weight_g)
#get the type of data contained in vector

#get the structure of the object
str(animals)

#how to add element to begining of vector
animals <- c("cincilla", animals)
animals <- c(animals, "frog")


typeof(animals)

#what happens if we mix types in vectors

num_char <- c(1, 2, 3, "a")
num_logical <- c(1, 2, 3, FALSE)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1, 2, 3, "4")
# logical -> numeric -> character
# logical -> character


#subsetting vectors
animals[2]
animals[c(1, 2)]

more_animals <- animals[c(1, 2, 3, 2, 1, 4)]

weight_g
weight_g[c(FALSE, FALSE, T, TRUE)]
weight_g > 63
weight_g(weight_g > 63)
weight_g[weight_g > 63 & weight_g < 80]
weight_g[weight_g < 58 | weight_g > 80]
weight_g==65
# <, >, ==, !=, <=, >=

animals[animals== "rat" | animals "frog"]
# %in% helps us find all elements corresponding to a vector
animals %in% c("rat", "frog", "cat", "duck", "dog")
animals[animals %in% c("rat", "frog", "cat", "duck", "dog")]

# an example of a vector with missing data
height <- c(2, 4, 4, NA, 6)
mean(height)
mean(height, na.rm = T)
max(height, na.rm = T)
#identify missing data
is.na(height)
height[!is.na(height)]
#omit missing data
na.omit(height)

#extract complete cases
complete.cases(height)

heights <- c(63, 69, 60, 65, NA, 68, 61, 70, 61, 59, 64, 69, 63)
na.omit(heights)
median(heights)
median(heights, na.rm = T)
heights[heights > 67]
heights(heights > 67)

is.na(heights)
heights_no_na <- na.omit(heights)
heights_no_na[heights_no_na > 67]
length(heights_no_na[heights_no_na > 67])
sum(heights_no_na > 67)

median(heights_no_na)

