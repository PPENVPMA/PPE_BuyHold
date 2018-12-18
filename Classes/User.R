#charger la librairie
library(R6)
#version du package > 1.0.1 -- classes portables source("/path/to/file/my_fn_lib1.r")

packageVersion("R6")

User <- R6Class(
  "User",
  public = list(
    #Constructor
    initialize = function(user_lastname, user_firstname)
    {
      private$user_lastname = user_lastname
      private$user_firstname = user_firstname
      private$user_risk = NA
    },
    
    #ACCESSEUR
    get_user_lastname = function() {
      return(private$user_lastname)
    },
    get_user_firstname = function() {
      return(private$user_firstname)
    },
    get_user_risk = function() {
      return(private$user_risk)
    },
    set_user_lastname = function(user_lastname) {
      private$user_lastname = user_lastname
    },
    set_user_firstname = function(user_firstname) {
      private$user_firstname = user_firstname
    },
    set_user_risk = function(user_risk) {
      private$user_risk = user_risk
    }
    
    
  ),
  #membres privé
  private = list(
    #champs
    user_lastname = NA,
    user_firstname = NA,
    user_risk = NA
    
  )
  
)
