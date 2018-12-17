#frais de courtage
# d1= read.table("DATA_AGREGEE.txt", header= TRUE)
############################### IMPORTATION DONNES #################################

# Les données sont importées sous forme de tableau. Première colonne : Nom du broker
# Les tarifs sont notés comme suit: Seuil Minimum,Seuil Maximum,Frais fixe,Frais Variables.
# les lignes ayant moins d'élément se verront compléter par 0.

d_broker= read.table("Courtiers.txt",header= TRUE, sep=" " )
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
  nb_stock=nb_action_actuel
  nb_action_objectif=f_objectif_nb_actions(objectif,stock_price)
  variation=nb_action_objectif-nb_stock
  PnL=0
  prix_achat=0
  print(phi(m_frais_broker,broker,variation,stock_price))
  if(phi(m_frais_broker,broker,variation,stock_price)+abs(variation)*stock_price <cash && variation!=0){
    prix_achat=-variation*stock_price-phi(m_frais_broker,broker,variation,stock_price)
    nb_stock=nb_stock+variation
  }
  return(c(prix_achat,nb_stock))
}



Capital=10000
Objectif=1000
stock_price=matrix(c(100,140,100))
T=3
nb_stock=0

cash=matrix(0.0,T,ncol=1)
cash[1]=Capital

for(i in 2:T){
  val=f_equilibrage(d_broker,"BoursoramaClassic",Objectif,stock_price[i],nb_stock,cash[i-1])
  nb_stock=val[2]
  cash[i]=cash[i-1]+val[1]
  print(c(nb_stock,nb_stock*stock_price[i]+cash[i]))
}














