#charger la librairie
library(R6)
#version du package > 1.0.1 -- classes portables
packageVersion("R6")

Asset <- R6Class(
  "Asset",
  public = list(
    #Constructor
    initialize = function(asset_cost, asset_price, asset_code)
    {
      private$asset_cost = asset_cost
      private$asset_price = asset_price
      private$asset_code = asset_code
      
    },
    
    #ACCESSEUR
    get_asset_cost = function() {
      return(private$asset_cost)
    },
    get_asset_price = function() {
      return(private$asset_price)
    },
    get_asset_code = function() {
      return(private$asset_code)
    },
    set_asset_cost = function(asset_cost) {
      private$asset_cost = asset_cost
    },
    set_asset_price = function(asset_price) {
      private$asset_price = asset_price
    },
    set_asset_code = function(asset_code) {
      private$asset_code = asset_code
    }
  ),
  #membres privé
  private = list(
    #champs
    asset_cost = NA,
    asset_price = NA,
    asset_code = NA
    
  )
  
)
