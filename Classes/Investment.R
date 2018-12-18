#charger la librairie
library(R6)
#version du package > 1.0.1 -- classes portables
packageVersion("R6")

Investment <- R6Class(
  "Investement",
  public = list(
    #Constructor
    initialize = function(investment_quantity, investment_date)
    {
      private$investment_quantity = investment_quantity
      private$investment_date = investment_date
      
      private$investment_cost = NA
      private$investment_price = NA
    },
    
    #ACCESSEUR
    get_investment_quantity = function() {
      return(private$investment_quantity)
    },
    get_investment_date = function() {
      return(private$investment_date)
    },
    get_investment_cost = function() {
      return(private$investment_cost)
    },
    get_investment_price = function() {
      return(private$investment_price)
    },
    set_investment_quantity = function(investment_quantity) {
      private$investment_quantity = investment_quantity
    },
    set_investment_date = function(investment_date) {
      private$investment_date = investment_date
    },
    set_investment_cost = function(investment_cost) {
      private$investment_cost = investment_cost
    },
    set_investment_price = function(investment_price) {
      private$investment_price = investment_price
    }
  ),
  #membres privé
  private = list(
    #champs
    investment_quantity = NA,
    investment_date = NA,
    investment_cost = NA,
    investment_price = NA
  )
  
)
