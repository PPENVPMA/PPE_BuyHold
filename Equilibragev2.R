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

phi = function(m_frais_broker,broker,nb_actions,stock_price_unique)
{
  frais_courtages=NaN
  for(i in 2:length(m_frais_broker[,1]))
  {
    if (broker==m_frais_broker[i,1])
    {
      for(j in seq(2, length(m_frais_broker[1,]), by = 4))
      {
        if (nb_actions*stock_price_unique>as.double(m_frais_broker[i,j]) && nb_actions*stock_price_unique<=as.double(m_frais_broker[i,j+1]))
        {
          #frais_fixe=as.double(m_frais_broker[i,j+2])
          #frais_variable= as.double(m_frais_broker[i,j+3])
          frais_courtages= as.double(m_frais_broker[i,j+2])+nb_actions*stock_price_unique*as.double(m_frais_broker[i,j+3])
        }
      }
    }
  }
  return(frais_courtages)
}

f_nb_actions_objectif = function(objectif,stock_price)
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

f_equilibrage=function(m_frais_broker,broker,objectif,stock_price,nb_action_actuel,cash)
{
  delta_cash=0
  nb_action= nb_action_actuel
  nb_action_objectif=f_nb_actions_objectif(objectif,stock_price)
  variation=nb_action_objectif-nb_action_actuel
  frais_de_courtage = phi(m_frais_broker,broker,abs(variation),stock_price)
    if(!is.null(frais_de_courtage))
    {
      if(variation !=0)
      {
        if (frais_de_courtage+ abs(variation)*stock_price<cash)
        {
          delta_cash= +frais_de_courtage+variation*stock_price
          nb_action = nb_action_actuel+variation
          
        }
      }
    }
  return(c(delta_cash,nb_action))
}

############################################ BODY 
Capital = 10000
T=4
cash=matrix(0.0,T,ncol=1)
cash[1]=Capital

stock_price = matrix(data = c(100,110,120,130))

f_ultimate= function(Objectif, Nb_obs, nb_stock_initial,t_cash,stock_price)
{
  nb_stock= matrix(0,nrow=Nb_obs)
  nb_stock[1]=nb_stock_initial
  for(i in 2:Nb_obs){
    
    val=f_equilibrage(d_broker,"BoursoramaClassic",Objectif,stock_price[i],nb_stock[i-1],cash[i-1])
    nb_stock[i]=val[2]
    cash[i]=cash[i-1]-val[1]
    
  }
  print(c("Exposition :", Objectif, "Nombre d'observation :", Nb_obs, "Nombre d'actions à t0 :",nb_stock_initial,"Capital initial", t_cash[1]))
  m=matrix(c(stock_price,nb_stock,cash),ncol=3)
  colnames(m)=c("Stockprice","   Nombre d'actions","    Capital")
  print(c("Gain de ", (cash[length(cash)]+nb_stock[length(nb_stock)]*stock_price[length(stock_price)]-cash[1])))
  return(m) 
}

print(f_ultimate(1000,T,0,cash,stock_price))









