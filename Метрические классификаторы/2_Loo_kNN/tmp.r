#  here is how you assign and use arithmetic
x = 11
y = 17
x * y
x / y
abs(-x)
exp(x)
log(y) / log(2)
str = "a string"

#  here are some examples on sequences
vec = c(1, 3, 5, 7, 9)
strvec = c("male", "female")
s = 2:7 # a sequence
s = seq(from=1, to=7, by=3) # general sequence command
r = rep("str", times=10) # repeat command
r = rep(seq(from=2, to=4, by=0.2), times=5) # combo

#  here are operations of vectors
vec = c(1, 3, 5, 7)
vec + 10
vec * 10
vec2 = c(1, 6, 4, 5)
vec + vec2 # same lengh -> operation on correspoding elements
vec * vec2
vec[1] # vector extracion, NOTE: 1 INDEXING!!!!!!!!!!!!!!
vec[-2] # all elements except second
vec[1:2] # sequence selection
vec[vec > 3] # condition extraction

#  here are matrix examples
# create matrix from a sequence
m = matrix(1:9, nrow=3) # first index changes fastest (column is filled first)
print(m)
m = matrix(1:9, nrow=3, byrow=TRUE) # second index changes fastest (row is filled first)
print(m)
m[1:2,1:2] # extraction is same as vectors
m[2,] # extract whole row, NOTE: m[2] without comma will not work - it will extract just the first element
m[,2] # extract whole columnm NOTE: the result will be casted to a single row vector
table(m) # counts how many different entries in matrix
which.max(table(m)) # argmax in array

#  here is how you import some data
# csv file example:
data = read.table("data.csv", header=T)
data[,2] # use data as a matrix
names(data) # fields' names
summary(data) # useful info: mean, etc..

#  here is how you select data by condition
# "data$B==2" produces a vector of TRUE and FALSE values.
# if index of this vector is TRUE then the row/column will be selected
# otherwise it will be excluded:
data[data$B==2,]
data[c(TRUE,FALSE,FALSE),] # is an example of what happens internally

#  command cbind appends a vector AS a column to a matrix
cbind(data,rep("qwe", times=length(data)))

#  fast function example:
fib = function(a) {
  if (a == 0) {
    return(0)
  } else if (a == 1) {
    return(1)
  } else {
    return(fib(a-1) + fib(a-2))
  }
}
fib(10)


s = c(5, 4, 4, 4, 3, 1, 4, 11)
which.max(s)
# 8
table(s)
#  1  3  4  5 11 
#  1  1  4  1  1 

