#Data Types
TRUE
class(TRUE)
class(2)
class("cc")
is.numeric(2)
is.numeric(2L)
is.character("aaa")

#Corecion
as.numeric(TRUE)
as.character(4.5)
as.numeric("Hello")


#Vectors
drawn_suites <- c("heart", "spades", "diamond", "diamond", "spades")
drawn_suites
is.vector(drawn_suites)
remain <- c(11, 12, 11, 13)
remain

suites <- c("spades", "heart", "diamond", "clubs")
suites
names(remain) <- suites
remain

remain <- c(spades = 11, heart = 12, diamond = 11, clubs = 13)
remain

remain <- c("spades" = 11, "heart" = 12, "diamond" = 11, "clubs" = 13)
remain
str(remain)
length(remain)

c("a", 1)
c(TRUE, 5)
class(c("a", 1))
class(c(TRUE, 5))

#Vector Calculus
earnings <- c(50, 100, 30)
3 * earnings
3 + earnings
earnings^2
earnings / c(1, 2, 3)

expenses <- c(30, 40, 80)

bank <- earnings - expenses
sum(bank)

earnings > expenses


#Subsetting Vectors
remain <- c(spades = 11, heart = 12, diamond = 11, clubs = 13)
remain[1]
remain["heart"]

remain[c(1, 4)]
remain[c("heart", "clubs")]
remain[-2]
remain[-c(1, 4)]
remain[-c("heart", "clubs")]

remain[c(FALSE, TRUE, FALSE, TRUE)]
remain[c(FALSE, TRUE)]


#Matrix
matrix(1:6, nrow = 2)
matrix(1:6, ncol = 2)
matrix(1:6, ncol = 2, byrow = TRUE)

matrix(1:2, nrow = 3, ncol = 4)

cbind(1:3, 1:3)
rbind(1:3, 1:3)


m <- matrix(1:6, nrow = 2)
rbind(m, c(3, 4, 5))
rownames(m) <- c("row1", "row2")
colnames(m) <- c("col1", "col2", "col3")

m <- matrix(1:6, nrow = 2, dimnames = list(c("row1", "row2"), c("col1", "col2", "col3")))

num <- matrix(1:6, nrow = 2)
char <- matrix(LETTERS[1:6], nrow = 2)
cbind(num, char)

#Subsetting Matrices
m <- matrix(sample(1:15, 12), nrow = 3)
m
m[1, 3]
m[1,]
m[, 3]
m[9]
m[2, c(2, 3)]
m[c(1, 2), c(2, 3)]
is.matrix(m[c(1, 2), c(2, 3)])

rownames(m) <- c("row1", "row2", "row3")
colnames(m) <- c("col1", "col2", "col3", "col4")
m
m["row2", "col1"]
m[2, c("col1", "col2")]

m[c(TRUE,TRUE, FALSE), c("col1", "col2")]

#Matrix Arithmetics
rowSums(m)
colSums(m)

m / 1.1

#Categotrical Variables
blood <- c("B", "AB", "O", "A", "O", "O", "A", "B")
blood

blood_factor <- factor(blood)
blood_factor

str(blood_factor)

blood_factor2 <- factor(blood, c("O", "A", "B", "AB"))
blood_factor2
str(blood_factor2)
levels(blood_factor) <- c("BT_O", "BT_A", "BT_B", "BT_AB")
str(blood_factor)

blood_factor3 <- factor(blood, labels = c("BT_O", "BT_A", "BT_B", "BT_AB"))
str(blood_factor3)


blood_factor4 <- factor(blood,
            levels = c("O", "A", "B", "AB"),
            labels = c("BT_O", "BT_A", "BT_B", "BT_AB"))
str(blood_factor3)

blood_factor4 <- factor(blood,ordered = TRUE,
            levels = c("O", "A", "B", "AB"),
            labels = c("BT_O", "BT_A", "BT_B", "BT_AB"))
blood_factor4[2] > blood_factor4[3]
str(blood_factor3)

#Lists
song <- list("aa", 2, 3)
is.list(song)

names(song) <- c("aa", "bb", "cc")
song

song <- list("aa" = "aa", "bb" = 2, "Cc" = 3)
str(song)

similar_song <- list("zz" = "zz", "xx" = 21, "cc" = 31)
song <- list("aa" = "aa", "bb" = 2, "Cc" = 3, "similar" = similar_song)
str(song)

#Extend/Subset Lists
song[1]
song[[1]]
song[["aa"]]

song[c(1, 3)]
song[[4]][[1]]
song[[c(4, 1)]]


song$similar
song$aa

friends <- c("55f", "333")
song$sent <- friends


#Data Frames
df <- read.csv(file = "TextFile.csv")
str(df)
df[2, 3]
df[2, "d"]
df[2,]
df[,]
df[, "c"]
df[c(2, 7), c("ï..a", "b")]

df$c
df$`ï..a`
df[[2]]
df["b"]
df[["b"]]

height <- seq(2:11)
df$height <- height
cbind(df, height)
sort(df$d)
ranks <- rank(df$b)
ranks <- order(df$b)

df[order(df$b, decreasing = FALSE),]







