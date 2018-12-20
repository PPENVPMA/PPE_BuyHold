#charger la librairie
library(R6)
#version du package > 1.0.1 -- classes portables source("/path/to/file/my_fn_lib1.r")

packageVersion("R6")

Broker <- R6Class(
  "Broker",
  public = list(
    #Constructor
    initialize = function(broker_name)
    {
      private$broker_name = broker_name
      private$broker_fees = NA
    },
    
    #ACCESSEUR
    get_broker_name = function() {
      return(private$broker_name)
    },
    get_broker_fees = function() {
      return(private$broker_fees)
    },
    get_user_risk = function() {
      return(private$user_risk)
    },
    set_broker_name = function(broker_name) {
      private$broker_name = broker_name
    },
    set_broker_fees = function(broker_fees) {
      private$broker_fees = broker_fees
    },

    # Methode
    broker_update_fees = function(d_broker)
    {
      for(i in 2:length(d_broker[,1]))
      {
        if (private$broker_name==d_broker[i,1])
        {
          private$broker_fees= matrix(d_broker[i,2:length(d_broker[i,])])
        }
      }
    },
    
    broker_comp_fees = function(asset_quantity,asset_price){
      frais_courtages=NaN
      #print(private$broker_fees)
          for(j in seq(1, length(private$broker_fees), by = 4))
          {
            if (asset_quantity*asset_price>as.double(private$broker_fees[j])
                && asset_quantity*asset_price<=as.double(private$broker_fees[j+1]))
            {
              frais_fixe=as.double(private$broker_fees[j+2])
              frais_variable= as.double(private$broker_fees[j+3])
              frais_courtages= frais_fixe+
                  asset_quantity*asset_price*frais_variable
            }
          }
      
      return(frais_courtages)
    }
    
  ),
  #membres privé
  private = list(
    #champs
    broker_name = NA,
    broker_fees = NA
    
  )
)
BoursoramaClassic= Broker$new("BoursoramaClassic")
BoursoramaClassic$broker_update_fees(d_broker)
fees=BoursoramaClassic$broker_comp_fees(10,100)