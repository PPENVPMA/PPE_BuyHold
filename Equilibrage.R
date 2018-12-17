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

f_nb_actions_seuil = function(objectif,stock_price)
{
  nb_action_inf=f_nb_actions(objectif,stock_price)
  nb_action_sup=f_nb_actions((objectif+stock_price),stock_price)
  
  if (objectif-nb_action_inf*stock_price< nb_action_sup*stock_price-objectif)
  {
    return(nb_action_inf)
  }
  else
  {
    return(nb_action_sup)
  }
}

f_objectif_nb_actions = function(objectif,stock_price)
{
  nb_action_inf=objectif%/%stock_price
  nb_action_sup=nb_action_inf+1
  
  if (objectif-nb_action_inf*stock_price< nb_action_sup*stock_price-objectif)
  {
    return(nb_action_inf)
  }
  else
  {
    return(nb_action_sup)
  }
}

f_equilibrage=function(m_frais_broker,broker,objectif,stock_price,nb_action_actuel,cash){
  
  nb_action_objectif=f_objectif_nb_actions(objectif,stock_price)
  
  variation=nb_action_objectif-nb_action_actuel
  
  PnL=0
  nb_stock=nb_action_actuel

  print(phi(m_frais_broker,broker,variation,stock_price))
  {
  
  if(phi(m_frais_broker,broker,variation,stock_price) <cash){
    PnL=variation*stock_price-phi(m_frais_broker,broker,variation,stock_price)
    nb_stock=nb_stock+variation
  }}
  return(c(PnL,nb_stock))
}



Capital=10000
Objectif=1000
stock_price=matrix(runif(n=10,0,100))
T=10
nb_stock=0

cash=matrix(Capital,T,ncol=1)

for(i in 2:T){
  val=f_equilibrage(d2,"BoursoramaClassic",Objectif,stock_price[i],nb_stock,cash[i])
  nb_stock=val[2]
  cash[i]=cash[i]+val[1]
}














