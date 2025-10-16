# ===============================
# 1. Działania na wektorach i macierzach
# ===============================

# 1.a
x <- 1:10
y <- seq(2, 20, by=2)
A <- cbind(x, y)           # macierz 10x2
At <- t(A)                 # transpozycja
At_y <- At %*% y           # iloczyn At przez y

# 1.b
A2 <- matrix(c(4,1,1,
               1,5,2,
               1,2,6), nrow=3, byrow=TRUE) # symetryczna, dodatnia, diagonalnie zdominowana
b <- c(7, 8, 9)
det_A2 <- det(A2)
x_sol <- solve(A2, b)

# 1.c
c_vec <- c(10,11,12)
B <- cbind(A2, c_vec)
d_vec <- c(13,14,15,16)
G <- rbind(B, d_vec)

# 1.d
colnames(G) <- c("Róża","Tulipan","Stokrotka","Fiołek")
rownames(G) <- c("Anna","Kasia","Ewa","Maria")
dim(G)

# 1.e
mat <- matrix(1:9, nrow=3, ncol=3, dimnames=list(c("r1","r2","r3"), c("c1","c2","c3")))

# 1.f
Z <- array(x, dim=c(3,3,2))
# lub poleceniem dim
Z2 <- x
dim(Z2) <- c(3,3,2)

# ===============================
# 2. Działania na listach i ramkach
# ===============================

# 2.a
list1 <- list(strings=c("A","B","C"), numbers=matrix(1:9,3,3), logicals=matrix(c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE),3,3))
typeof(list1)
sqrt(list1$numbers)

# 2.b
set.seed(123)
palenie <- sample(c(TRUE,FALSE), 10, replace=TRUE)
plec <- sample(c("K","M"), 10, replace=TRUE)
wiek <- sample(1:100, 10, replace=TRUE)
badanie <- data.frame(czy_pali=palenie, plec=plec, wiek=wiek)
sapply(badanie, class)               # sprawdzenie typu kolumn
table(badanie$plec)                  # zliczenie kobiet i mężczyzn

# ===============================
# 3. Pliki i dane
# ===============================

# 3.a
write.csv(badanie, "badanie.csv", row.names=FALSE)
Nowe_badanie <- read.csv("badanie.csv")

# 3.b
data(beaver1, package="boot")
write.csv(beaver1, "beaver1.csv", row.names=FALSE)

# ===============================
# 4. Funkcje, pętle i warunki
# ===============================

# 4.a
beav <- beaver1
iloczyn <- 0
for(i in 1:nrow(beav)){
  iloczyn <- iloczyn + beav$temp[i] * beav$activ[i]
}
iloczyn

# 4.b (funkcje zerowe)
zeros_count <- function(v) {
  sum(v == 0)
}

first_last_true <- function(vec) {
  if(any(vec)){
    c(which(vec)[1], tail(which(vec), 1))
  } else {
    c(NA, NA)
  }
}

# 4.c funkcja moda
moda <- function(x){
  tab <- table(as.vector(x))
  as.numeric(names(tab[tab == max(tab)]))[1]  # jeśli wiele mod, zwraca pierwszą
}

# Test moda
tablica <- array(sample(1:5, 27, replace=TRUE), dim=c(3,3,3))
moda(tablica)
