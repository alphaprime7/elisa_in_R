#call Rcpp
library(Rcpp)
#calculate the magnitude
f <- function(u) {if (u<0){-u} else {u}}
f(-5)
#[1] 5
#f(5)
#[1] 5
#f(-15)
#[1] 15
#(15)
#[1] 15
#create an x+1 function
g <- function(x) {x+1}
#compute g(2)
g(2)
#make primes list 
primes_list <- list(2,3,5,7,11,13)
for (i in 1:length(primes_list)) { print(primes_list[[i]])}
#[1] 2
#[1] 3
#[1] 5
#[1] 7
#[1] 11
#[1] 13
#create a data frame
df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10), d = rnorm(10))
#print the median for each column 
for (i in 1:ncol(df)) {print(median(df[[i]]))}
#make storage vector
storage <- numeric(4)
#for loop to calculate median then store in storage
for (i in 1:ncol(df)) {storage[i] <- median(df[[i]])}
#print out storage
storage
#[1]  0.1962450  0.3813339  0.2252474 -0.4353736
for(degc in c(40,50,35,60,120)) {degf <- degc*(9/5)+32;print(c(degc,degf))}
#[1]  40 104
#[1]  50 122
#[1] 35 95
#[1]  60 140
#[1] 120 248
for(i in c(40,50,35,60,120)) {degf <- i*(9/5)+32;print(c(degf))}
storage1 <- vector('numeric', 5)
storage1
#[1] 0 0 0 0 0
result
n <- 5
result <- matrix(ncol=1, nrow=n)
library(magicfor)
magic_for(print, silent = T)
magic_result_as_dataframe()
for(i in c(40,50,35,60,120)) {degf <- i*(9/5)+32;result[i] <- c(degf)}
#    i c(degf)
#1  40     104
#2  50     122
#3  35      95
#4  60     140
#5 120     248
#magic_for(): Magicalize for.
#magic_free(): Free magicalization
#Get results:
#magic_result(): as a list.
#magic_result_as_vetor(): as a vector.
#magic_result_as_dataframe(): as a data.frame.
#put(): Display values.
#GOOD EXAMPLES
n <- 10
mat <- matrix(ncol=2, nrow=n)

for (i in 1:n) {
    var1 <- function_one(i,par1)
    var2 <- function_two(i,par2)
    mat[i,] <- c(var1,var2)
}

print(mat)

a <- 0
for (i in 1:10) {
     a[i] <- mean(rnorm(50))
}

print(a)

a <- 1:10
b <- 1:10
res <- numeric(length = length(a))
for (i in seq_along(a)) {
  res[i] <- a[i] + b[i]
}
res
#create a list
list <- c()
# Create a list with three vectors
fruit <- list(Basket = c('Apple', 'Orange', 'Passion fruit', 'Banana'), 
Money = c(10, 12, 15), purchase = FALSE)
for (p  in fruit) 
{ 
	print(p)
}
# Create a matrix
mat <- matrix(data = seq(10, 20, by=1), nrow = 6, ncol =2)
# Create the loop with r and c to iterate over the matrix
for (r in 1:nrow(mat))   
    for (c in 1:ncol(mat))  
         print(paste("Row", r, "and column",c, "have values of", mat[r,c]))  