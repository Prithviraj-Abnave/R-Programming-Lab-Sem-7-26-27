#Subsetting Vector

#Example 1: Basic Logical Condition
x <- c(10,20,30,40,50)
x[x>25]

#Example 2: Multiple Conditions

x[x>=20 & x<=40]
  
x[x==10 | x>40]


#Example 3: Using which() for Position Indexing

which(x>25)

x[which(x > 25)] 

#Example 4: NA Handling

y <- c(1,NA,3,4,NA)

y[y>2]

y[which(y>2)]

#Example 5: Character Vectors

colors <- c("red", "blue", "green", "red")

colors[colors == "red"] 

colors[nchar(colors) > 3] 

#Example 6: Recycling Behavior

z<- 1:10

z[c(TRUE,FALSE)]


#Special cases and edge conditions

# 1. Empty Logical Behavior
x[logical(0)]

# 2.All FALSE
x[rep(FALSE,length(x))]

# 3. All TRUE
x[rep(TRUE, length(x))]  

# 4. NA in Logical Vector
x[c(TRUE, NA, FALSE, TRUE, NA)] 

# Character Indexing (Named Vectors)
v <- c(10, 20, 30, 40, 50)
names(v) <- letters[1:5]   # a, b, c, d, e

v["b"]

v[c("a", "e")]

marks <- c(85, 91, 76)
names(marks) <- c("Amit", "Riya", "John")
marks["Riya"]

#Empty Indexing

v <- c(10, 20, 30, 40, 50)
v[]

v[] <- 100
v

#Zero indexing
v <- c(10, 20, 30, 40, 50)
v[0]

name <- c("Amit", "Riya", "John")
name[0]

#1.2 Matrix and Array Subsetting

# 1. Basic Matrix Subsetting

m <- matrix(1:12, nrow=3, dimnames=list(c("r1","r2","r3"),c("c1","c2","c3","c4")))
m[]


m[2, 3]       
m[2, ]        
m[, 3]
m[c(1,3), 2:3]

a <- array(1:24, dim = c(2,3,4))
a[]

a[2,3,4]

a[,1,3]

# 1.3 List Subsetting in R: Three Methods

# Create a list
lst <- list(
  a = 1:3,
  b = "text",
  c = matrix(1:4, 2)
)
lst