podatoci<-read.csv("world-happiness-report-2016.csv")
podatoci

Happiness.Score<-podatoci[,4]
Health..Life.Expectancy.<- podatoci[,11]

#obelezhje - happiness score (d)
d=c(Happiness.Score)
n=length(d)
d=sort(d)
#DEL 1
#BARANJE 1

breaks1 = seq(2.8, 7.6, by=0.4)
d.int = cut(d, breaks1, right=FALSE) 
freq = table(d.int) 
Rfreq= freq/n 
Cumfreq= cumsum(freq)
R_Cumfreq= cumsum(freq)/n
R_Cumfreq2 = cumsum(Rfreq)
Pfreq= Rfreq*100 
P_Cumfreq= R_Cumfreq*100
d.table= cbind(freq,Rfreq,Cumfreq,Pfreq,P_Cumfreq)


mid=c()
for (i in 1:length(breaks1)-1)
{mid = c(mid, (breaks1[i]+breaks1[i+1])/2)} 
colors = c("pink", "purple", "blue", "lightblue")
h1 = hist(d, right = FALSE, breaks = breaks1, col= colors, main = "Happiness Score",ylab= "честоти"
          ,xlab = "Ниво на среќа", xlim=c(2.8,8.0))
lines(mid,freq)

h1$counts = h1$counts / sum(h1$counts)
plot(h1, freq = TRUE, col = "lightgreen",ylab = "Релативни фреквенции",
     xlab = "Ниво на среќа", main = "Стапка на среќа")

procent_freq = paste(ceiling(Pfreq),"%",sep = "")
procent_freq
pie(Pfreq,labels = procent_freq, main = "Честоти во проценти за стапката на среќа")
P_Cumfreq0 = c(0, P_Cumfreq) 
plot(breaks1, P_Cumfreq0, axes = F, main = "Полигон на кумулативни честоти во % (Ogive) - Стапка на среќа", xlab= "интервали", ylab= "Кумулативни честоти %") 
axis(side = 1, at = breaks1) 
axis(side = 2) 
lines(breaks1, P_Cumfreq0)

#BARANJE 2 (steblo list dijagram)

d = sort(d)
library(data.table)
# leftDigits - pozicija na '|' vo odnos na decimalnata tocka, rounding - br. na decimali,
myStem <- function(x, leftDigits, rounding = 1) {
  data = data.table("x" = x)
  data[, left := floor(x/10^leftDigits)]
  data[, right := (round(x - left*10^leftDigits, rounding))*10^rounding]
  data = data[, paste(sort(right), collapse = " "), by = left]
  data[, out := paste(left, " | ", V1), by = left]
  cat(data$out, sep = "\n")
}
myStem(d, 0, 2)


#BARANJE 4 (moda, medijana, prosek)

mean (Happiness.Score , na.rm=TRUE)
median(Happiness.Score, na.rm = TRUE)

getmode <- function(d) {
  uniqv <- unique(d)
  uniqv[which.max(tabulate(match(d, uniqv)))]
}

result <- getmode(d)
print(result)

#BARANJE 5 (kvartali, opseg, interkvartalen raspon)
quantile(d)
range(d, na.rm = TRUE, finite=FALSE)
max(d)-min(d)
IQR(d)

#BARANJE 6 (disperzija i standardna devijacija)
devijacija <-sd(d)
devijacija
disperzija <- sd(d) * sd(d)
disperzija

#BARANJE 7 
v=c(Health..Life.Expectancy.)
n=length(v)
v=sort(v)
cor(d, v) 

#BARANJE 3 (grafika na rasejuvanje )
plot(d,v,pch = 19, main = "Стапката на среќа наспроти очекувања за здрав живот", xlab = "Стапка на среќа", ylab = "Очекувања за здрав живот")

#VTOR DEl

#BARANJE 1 (Interval na doverba(95%))
simple.z.test = function(d, devijacija, conf.level=0.95)
{
  n=length(d)
  xbar=mean(d)
  alpha=1 - conf.level
  z=qnorm(1-alpha/2)
  SE=disperzija/sqrt(n)
  xbar + c(-z*SE, z*SE)
}

simple.z.test(d,devijacija)

#BARANJE 2 (hipotezi), H0
#testiranje hipotezi za prosek
# Ho: mi>=4;
#Ha: mi<4 so 95% interval na doverba, alpha=0.05
mi=4;
xbar=mean(d);
alternative="smaller";
alpha=0.05;
n=length(d);
Zo=(xbar-mi)/(disperzija/sqrt(n))
if(alternative=="smaller"){
  z=-qnorm(1-alpha);
  print(z);
  print(Zo);
  if(Zo<z){
    print("Nulta hipoteza se otfrla")
  }
  else {
    print("NUltata hipoteza e tochna.")
  }
}
#regresiona analiza
model<- lm(d ~ v)
round(coefficients(model), 3)
plot(d,v)
abline(reg=model)
Z<-coefficients(model)
summary(model)
lm(formula=d ~ v)
Z
 
#baranje 4
x = punif(d, min=2.905, max=7.526, lower.tail=TRUE)
chisq.test(d, NULL, TRUE, x, n)


