dane = c(22,31,51,68,104,125,177,238,287,355,425,536,634,749,901,1051,1221,1389,1638,1862,2055,2311,2554,2946,3383,3627,4102,4413,4646,5205,5575,5955)
#Regresja liniowa:
model = lm(dane~czas,data.frame(czas,dane))             
summary(model)
zlin = predict(model,data.frame(czas=42))
dwudziesty - zlin
#Błąd regresji liniowej wynosi 3005.034
#Regresja wieloraka:
modelx = lm(dane~1*czas,data.frame(czas,dane))
summary(modelx)
zx = predict(modelx,data.frame(czas=42))
dwudziesty - zx
#Błąd regresji wielorakiej
#(zależność liczby przypadków od x)
#wynosi  1832.375
modely = lm(dane~czas+I(czas^2),data.frame(czas,dane))
summary(modely)
zy = predict(modely,data.frame(czas=42))
abs(dwudziesty - zy)
#Błąd regresji wielorakiej
#(zależność liczby przypadków od x^2)
#wynosi  1471.99
modelz = lm(dane~czas+I(czas^3),data.frame(czas,dane))
summary(modelz)
zz = predict(modelz,data.frame(czas=42))
abs(dwudziesty - zz)
#Błąd regresji wielorakiej
#(zależność liczby przypadków od x^3)
#wynosi  3414.183
#Regresja wykładnicza:
mwykl = lm(log10(dane)~czas)
c = 10^coef(mwykl)
sum((y-c[1]*c[2]^x)^2)
sum((dane-c[1]*c[2]^czas)^2)
sum((dane-c[1]*c[2]^czas)^2)

#Najmniejszą różnicę miał model regresji wielorakiej











