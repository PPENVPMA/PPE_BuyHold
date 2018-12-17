#Initialisations
Kapital=10000
stock_price=c(90,100)

#frais de courtage
# d1= read.table("DATA_AGREGEE.txt", header= TRUE)
############################### IMPORTATION DONNES #################################

# Les données sont importées sous forme de tableau. Première colonne : Nom du broker
# Les tarifs sont notés comme suit: Seuil Minimum,Seuil Maximum,Frais fixe,Frais Variables.
# les lignes ayant moins d'élément se verront compléter par 0.

d2= read.table("C:/Users/nmace/Documents/ING4/PPE/PPE_BuyHold/Courtiers.txt",header= TRUE, sep=" " )
############################### FONCTIONS #################################

# La fonction phy retourne les frais de courtages(frais fixe+ frais variables d'un broker)
# Selon nombre de positions et le tarif de l'action
# Si frais de courtage =0 alors le seuil n'a pas été trouvé. La function renvoie alors NaN
# La boucle parcourant les colonnes la parcour de 4 en 4.

phi = function(m_frais_broker,broker,nb_positions,stock_price_unique)
{
  frais_courtages=NaN
  for(i in 2:length(m_frais_broker[,1]))
  {
    if (broker==m_frais_broker[i,1])
    {
      for(j in seq(2, length(m_frais_broker[1,]), by = 4))
      {
        if (nb_positions*stock_price_unique>as.double(m_frais_broker[i,j]) && nb_positions*stock_price_unique<=as.double(m_frais_broker[i,j+1]))
        {
          #frais_fixe=as.double(m_frais_broker[i,j+2])
          #frais_variable= as.double(m_frais_broker[i,j+3])
          frais_courtages= as.double(m_frais_broker[i,j+2])+nb_positions*stock_price_unique*as.double(m_frais_broker[i,j+3])
        }
      }
    }
  }

  return(frais_courtages)
}


# La fonction lowest_phi retourne le broker le moins chère pour un prix et un nombre d'actifs
# donné. 
# Le minima est trouvé par la position dans la liste L.

lowest_phi= function(m_frais_broker,nb_positions,stock_price_unique)
{
  L=phi(m_frais_broker,m_frais_broker[1,1],nb_positions,stock_price_unique)
  nom_broker=m_frais_broker[1,1]
  for(i in 2:length(m_frais_broker[,1]))
  {
    L=c(L,phi(m_frais_broker,m_frais_broker[i,1],nb_positions,stock_price_unique))
  }
  index= which.min(L)
  nom_broker= m_frais_broker[index,1]
  returnValue(nom_broker)
}


#Calcul de la quantite d'actions a acheter
f_nb_actions=function(Kapital,stock_price)
{
  n=Kapital%/%stock_price[1]
  reste=Kapital%%stock_price[1]
  while (phi(d2,"BoursoramaClassic",n,stock_price[1])>reste) {
    reste=reste+stock_price[1]
    n=n-1
  }
  return(n)
} 



#Calcul du cash restant dans le portefeuille
f_cash_restant=function(d2,broker,nb_positions,stock_price,Kapital)
{
  cash =Kapital - stock_price[1]* f_nb_actions(Kapital,stock_price)-phi(d2,"BoursoramaClassic",nb_positions,stock_price[1])
  return(cash)
}


#Calcul des Profits and Loss
f_PL=function(d2,broker,nb_positions,stock_price)
{
  PL=nb_positions*(stock_price[2]-stock_price[1])-phi(d2,"BoursoramaClassic",nb_positions,stock_price[1])-phi(d2,"BoursoramaClassic",nb_positions,stock_price[2])
  return(PL)
}

#Calcul du rendement
f_rdt=function(PL,Kapital)
{
  r=PL/Kapital
  return(r)
}





f_ultimate = function(Kapital,m_frais_broker,broker,stock_price)
{
  nb_actions=f_nb_actions(Kapital,stock_price[1])
  PL= f_PL(m_frais_broker,broker,nb_actions,stock_price)
  rdt= f_rdt(PL,Kapital)
  cash_restant = f_cash_restant(m_frais_broker,broker,nb_actions,stock_price,Kapital)
  return(c("Nombre d'action : ",nb_actions," PL : ",PL, " rendement :", rdt, "cash restant : ", cash_restant))
}

f_ultimate(10000,d2,"BoursoramaClassic",stock_price)