# ===============================
# Interpretacja graficzna danych
# ===============================

library(boot)
library(car)   # do scatterplot
library(ggplot2)

data(acme, package="boot")

# ===============================
# Zadanie 1: Wykres pudełkowy
# ===============================
boxplot(acme$market, acme$acme,
        names=c("market","acme"),
        main="Wykres pudełkowy kolumn acme",
        col=c("skyblue","lightgreen"))

# ===============================
# Zadanie 2: Wykres wiolinowy
# ===============================
# Przy użyciu ggplot2, trzeba utworzyć dane w formacie długim
library(reshape2)
acme_long <- melt(acme[,c("market","acme")])
ggplot(acme_long, aes(x=variable, y=value, fill=variable)) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1, fill="white") +
  labs(title="Wykres wiolinowy acme$market i acme$acme") +
  theme_minimal()

# ===============================
# Zadanie 3: Wykres mozaikowy / punktowy
# ===============================
# Przy plot()
plot(acme$market, acme$acme,
     main="Wykres punktowy acme$market vs acme$acme",
     xlab="market", ylab="acme",
     pch=19, col="blue")

# Przy scatterplot z pakietu car
scatterplot(acme$acme ~ acme$market,
            main="Scatterplot acme$market vs acme$acme",
            xlab="market", ylab="acme",
            pch=19, col="red")

# ===============================
# Zadanie 4: Wykresy gęstości rozkładów
# ===============================
x_vals <- seq(0,10,length.out=500)

# Rozkład normalny N(0,3)
dens_norm <- dnorm(x_vals, mean=0, sd=3)
# Rozkład F(3,6)
dens_F <- df(x_vals, df1=3, df2=6)
# Rozkład chi-kwadrat n=3
dens_chi <- dchisq(x_vals, df=3)

# Wykres wspólny
plot(x_vals, dens_norm, type="l", col="blue", lwd=2,
     ylim=c(0,max(c(dens_norm,dens_F,dens_chi))),
     main="Gęstości rozkładów", ylab="gęstość", xlab="x")
lines(x_vals, dens_F, col="red", lwd=2, lty=2)
lines(x_vals, dens_chi, col="green", lwd=2, lty=3)
legend("topright", legend=c("N(0,3)","F(3,6)","Chi-kwadrat 3"),
       col=c("blue","red","green"), lty=c(1,2,3), lwd=2)
