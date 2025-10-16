# ===============================
# Szeregi statystyczne
# ===============================

# Załaduj potrzebne pakiety
library(boot)
library(MASS)  # dla catsM

# ===============================
# Zadanie 1
# ===============================
data(acme, package="boot")

# Szereg rozdzielczy dla acme$market według reguł 1-4
x <- acme$market

# Reguła 1: sqrt(n) (liczba klas)
n <- length(x)
k1 <- ceiling(sqrt(n))
breaks1 <- seq(min(x), max(x), length.out = k1+1)
freq1 <- table(cut(x, breaks=breaks1))
barplot(freq1, main="Rozkład częstości (reguła sqrt(n))", col="skyblue")

# Reguła 2: Sturgesa
k2 <- ceiling(log2(n)+1)
breaks2 <- seq(min(x), max(x), length.out = k2+1)
freq2 <- table(cut(x, breaks=breaks2))
barplot(freq2, main="Rozkład częstości (reguła Sturgesa)", col="lightgreen")

# Reguła 3: Rice'a
k3 <- ceiling(2 * n^(1/3))
breaks3 <- seq(min(x), max(x), length.out = k3+1)
freq3 <- table(cut(x, breaks=breaks3))
barplot(freq3, main="Rozkład częstości (reguła Rice'a)", col="lightpink")

# Reguła 4: Scott
h4 <- 3.5*sd(x)*n^(-1/3)  # szerokość klasy
breaks4 <- seq(min(x), max(x)+h4, by=h4)
freq4 <- table(cut(x, breaks=breaks4))
barplot(freq4, main="Rozkład częstości (reguła Scott)", col="orange")

# ===============================
# Zadanie 2
# ===============================
# Szereg rozdzielczy ilości danych w każdym roku
# Zakładam, że acme ma kolumnę year
if("year" %in% colnames(acme)){
  tab_year <- table(acme$year)
  barplot(tab_year, main="Liczba danych w każdym roku", col="lightblue")
  
  # średnia acme$market w każdym roku
  aggregate(acme$market, by=list(acme$year), FUN=mean)
} else {
  print("Dane acme nie mają kolumny 'year'.")
}

# ===============================
# Zadanie 3
# ===============================
# Funkcja dzieląca dane numeryczne na przedziały o zadanej proporcji
podzial <- function(dane, proporcje){
  n <- length(dane)
  prop <- round(proporcje*n)
  prop[length(prop)] <- n - sum(prop[1:(length(prop)-1)])  # wyrównanie do n
  dane_sorted <- sort(dane)
  przedzialy <- cumsum(prop)
  cut_points <- c(0, przedzialy)
  bins <- cut(1:n, breaks=cut_points, labels=FALSE)
  freq <- table(bins)
  barplot(freq, main="Rozkład według proporcji", col="violet")
  return(freq)
}

# Przykład użycia
proporcje <- c(0.1,0.2,0.3,0.2,0.1,0.1)
podzial(x, proporcje)

# ===============================
# Zadanie 4
# ===============================
data(catsM, package="MASS")

cols <- c("Bwt","Hwt")
for(col in cols){
  cat("\nKolumna:", col, "\n")
  v <- catsM[[col]]
  cat("Średnia:", mean(v), "\n")
  cat("Mediana:", median(v), "\n")
  cat("Min:", min(v), "Max:", max(v), "\n")
  cat("Wariancja:", var(v), "\n")
  cat("Odchylenie standardowe:", sd(v), "\n")
  cat("Zakres:", diff(range(v)), "\n")
}

# różnica Bwt - Hwt
diff_col <- catsM$Bwt - catsM$Hwt
cat("\nRóżnica Bwt - Hwt\n")
cat("Średnia:", mean(diff_col), "\n")
cat("Mediana:", median(diff_col), "\n")
cat("Min:", min(diff_col), "Max:", max(diff_col), "\n")
cat("Wariancja:", var(diff_col), "\n")
cat("Odchylenie standardowe:", sd(diff_col), "\n")
cat("Zakres:", diff(range(diff_col)), "\n")

# ===============================
# Zadanie 5
# ===============================
# Kwantyle rzędu 0.1, 0.3, 0.9 dla acme$market
quantile(x, probs=c(0.1, 0.3, 0.9))
