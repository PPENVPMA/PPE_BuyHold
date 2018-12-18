#charger la librairie
library(R6)
#version du package > 1.0.1 -- classes portables
packageVersion("R6")

Stock <- R6Class(
  "Stock",
  inherit = Asset,
  
  public = list(
    #Constructor
    initialize = function(stock_ISIN, stock_currency, stock_flow)
    {
      private$stock_ISIN = stock_ISIN
      private$stock_currency = stock_currency
      private$stock_flow = stock_flow
    },
    
    #ACCESSEUR
    get_stock_ISIN = function() {
      return(private$stock_ISIN)
    },
    get_stock_currency = function() {
      return(private$stock_currency)
    },
    get_stock_flow = function() {
      return(private$stock_flow)
    },
    set_stock_ISIN = function(stock_ISIN) {
      private$stock_ISIN = stock_ISIN
    },
    set_stock_currency = function(stock_currency) {
      private$stock_currency = stock_currency
    },
    set_stock_flow = function(stock_flow) {
      private$stock_flow = stock_flow
    }
  ),
  #membres privé
  private = list(
    #champs
    stock_ISIN = NA,
    stock_currency = NA,
    stock_flow = NA
    
  )
  
)

CAC_40 = Stock$new("ABCD", "$", "2")
