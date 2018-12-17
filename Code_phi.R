# d1= read.table("DATA_AGREGEE.txt", header= TRUE)
############################### IMPORTATION DONNES #################################

# Les données sont importées sous forme de tableau. Première colonne : Nom du broker
# Les tarifs sont notés comme suit: Seuil Minimum,Seuil Maximum,Frais fixe,Frais Variables.
# les lignes ayant moins d'élément se verront compléter par 0.

d2= read.table("Courtiers.txt",header= FALSE, sep=" " )
############################### FONCTIONS #################################

# La fonction phy retourne les frais de courtages(frais fixe+ frais variables d'un broker)
# Selon nombre de positions et le tarif de l'action
# Si frais de courtage =0 alors le seuil n'a pas été trouvé. La function renvoie alors NaN
# La boucle parcourant les colonnes la parcour de 4 en 4.

phi = function(m_frais_broker,broker,nb_positions,stock_price)
{
  frais_courtages=NaN
  for(i in 1:length(m_frais_broker[,1]))
  {
    if (broker==m_frais_broker[i,1])
    {
      for(j in seq(2, length(m_frais_broker[1,]), by = 4))
      {
        if (nb_positions*stock_price>as.double(m_frais_broker[i,j]) && nb_positions*stock_price<=as.double(m_frais_broker[i,j+1]))
        {
          #frais_fixe=as.double(m_frais_broker[i,j+2])
          #frais_variable= as.double(m_frais_broker[i,j+3])
          frais_courtages= as.double(m_frais_broker[i,j+2])+nb_positions*stock_price*as.double(m_frais_broker[i,j+3])
        }
      }
      
    }
  }
  return(frais_courtages)
}

# La fonction lowest_phi retourne le broker le moins chère pour un prix et un nombre d'actifs
# donné. 
# Le minima est trouvé par la position dans la liste L.

lowest_phi= function(m_frais_broker,nb_positions,stock_price)
{
  L=phi(m_frais_broker,m_frais_broker[1,1],nb_positions,stock_price)
  nom_broker=m_frais_broker[1,1]
  for(i in 2:length(m_frais_broker[,1]))
  {
    L=c(L,phi(m_frais_broker,m_frais_broker[i,1],nb_positions,stock_price))
  }
  index= which.min(L)
  nom_broker= m_frais_broker[index,1]
  returnValue(nom_broker)
}