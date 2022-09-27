#______________________________________________
#>>Laboratorio di R, esercizio d'esame
#>>Autore: Giulio Fantuzzi
#>>Matricola: EC2100728
#>>Contatto: GIULIO.FANTUZZI@studenti.units.it
#>>Corso di laurea: Statistica
#______________________________________________


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#1)Scrivere una funzione che, per n arbitrario, restituisca la probabilità 
#approssimata mediante simulazione di Pr(max{Y_1, ..., Y_n}>0.5)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Implementazione della funzione
probabilita_approssimata<- function(n){
    nsim<- 10000
    count <- vector(mode = "numeric", length = nsim)
    for(i in 1:nsim){
        valori_generati<-runif(n, min = 0, max = 1)
        massimo=max(valori_generati)
        count[i]<- (massimo>0.5)
    }
    probabilita_empirica<- sum(count)/nsim
    return (probabilita_empirica)
}
#Scelta della numerosità campionaria
n<- 5
#Chiamata della funzione 
risultato_empirico= probabilita_approssimata(n)

#Confrontiamo il risultato da quello che ci aspettiamo dalla teoria
risultato_teorico= 1- (punif(0.5, min=0, max=1))^n

#Stampa dei risultati
round(risultato_empirico, digits=5)
round(risultato_teorico, digits=5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#2)Verifichiamo se -log(1-Y) / theta sono iid a X~Esp(theta)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Fisso arbitrariamente il parametro dell'esponenziale
theta=1.5
#genero i valori da una U(0,1)
n<- 1000
values<-runif(n,min=0,max=1)

#calcolo i vari -log(1-Y) / theta
log_values<- vector(mode = "numeric", length = n)
for(i in 1:n){
    log_values[i]= -log(1-values[i])/theta
}

i<-0#azzero il contatore (buona prassi)

#istogramma rappresentativo dei risultati empirici
hist(log_values, prob = T, breaks = 25, plot=T, 
     main='Verifica della i.i.d.', xlab=expression(- log(1-y)/theta))
#Sovrapponiamo la curva dell'esponenziale
curve(dexp(x,theta),from=0, add=TRUE, col='dark green', lwd=2)
legend("topright", legend= expression(Esp(theta)), lwd=2, 
       lty=1,col="dark green")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#3)Posto n=5, valutare il comportamento di T
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Simulazione Monte Carlo:ad ogni ciclo genero 5 Y_i,calcolo la funzione e sommo
#Ogni ciclo della simulazione mi darà una determinazione t di T
#Plotteremo poi il risultato empirico e confronteremo la distribuzione esatta
#NB: -log(1-Y) / theta ~Esp(theta), quindi (dalla teoria) T~Ga(5,theta)


theta=1.5 #arbitrario
n<- 5 #come da consegna
nsim<- 10000 #arbitrario (purché sufficientemente alto)

#vettore che conterrà le determinazioni (empiriche) di T
T_empirica1 <- vector(mode = "numeric", length = nsim)
for(i in 1:nsim){
    #1)generiamo n valori da Y~U(01)
    values<-runif(n, min = 0, max = 1)
    #2)calcoliamone la funzione -log(1-Y) / theta
    log_values<- vector(mode = "numeric", length = n)
    #3)somma dei valori ottenuti
    for(j in 1:n){
        log_values[j]= -log(1-values[j])/theta
    }
    t<- sum(log_values)
    T_empirica1[i]<- t
}
#Azzeriamo i contatori (buona prassi)
i<-0
j<-0
#istogramma rappresentativo dei risultati empirici
hist(T_empirica1, prob = T, breaks = 25, plot=T,
     xlab='T', main='Distribuzione empirica di T')
#Controlliamo se la Gamma teorica approssima i dati empirici
curve(dgamma(x, n, theta), add=T, col=2, lwd=2)
legend("topright", legend= expression(Ga(5,theta)), lwd=2, lty=1,col="red")

#_______________________________________________________________________________

#Cosa succede se n>>5? Interpretando T come somma di esponenziali
#Per TLC T~N(n/theta; n/theta^2) [essendo E(X)=1/theta e V(X)=1/theta^2]
#Usiamo ad esempio n=100 (nsim e theta rimarranno uguali a prima)

n<- 100
#vettore che conterrà le determinazioni (empiriche) di T
T_empirica2 <- vector(mode = "numeric", length = nsim)
for(i in 1:nsim){
    #1)generiamo n valori da Y~U(01)
    values<-runif(n, min = 0, max = 1)
    log_values<- vector(mode = "numeric", length = n)
    for(j in 1:n){
        log_values[j]= -log(1-values[j])/theta
    }
    t<- sum(log_values)
    T_empirica2[i]<- t
}
#Azzeriamo i contatori (buona prassi)
i<-0
j<-0

#istogramma rappresentativo dei risultati empirici:
hist(T_empirica2, prob = T, breaks = 25, plot=T, xlab='T',
     main='Distribuzione empirica di T')

#Distribuzioni teoriche:
mu= n/theta
sigma= sqrt(n/(theta^2))
curve(dnorm(x, mu, sigma), add=T, col='dark green', lwd=2, lty=2)
curve(dgamma(x, n, theta), add=T, col='red', lwd=2, lty=2)
legend("topright", legend= c("Gamma", "Normale"), lwd=3, 
       lty=3,col=c("red", "dark green"))

