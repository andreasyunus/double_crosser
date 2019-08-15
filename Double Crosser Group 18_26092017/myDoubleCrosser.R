library(R6)

Agent <- R6Class(
  "Agent",
  
  public = list(
    bid = NULL,
    book = NULL,
    greeting = NULL,
    id = NULL,
    opponent_id = NULL,
    opponent_greeting = NULL,
    round = NULL,
    response = NULL,
    
    set_book = function(book = NA) {
      self$book <- book
    },
    
    set_id = function(id = NA) {
      self$id = id
    },
    
    set_opponent_id = function(opponent_id = NA) {
      self$opponent_id = opponent_id
    },
    
    set_response = function(response = NA) {
      self$response <- response
    },
    
    set_round = function(round = NA) {
      self$round <- round
    },
    
    set_greeting = function() {
      greeting <- "Let's Rumble!"
    },
    
    receive_greeting = function(greeting = NA) {
      self$opponent_greeting = greeting
    },
    
    get_bid = function() {
      if(!(nrow(book))){
        self$myBackStabber()
      } else {
        if(self$opponent_trigger_A()){
          self$myAStrategy()
        } else {
          self$myBackStabber()
        }
      }
      },
    
    myAStrategy = function(){
      if(self$id == id1){
        if(!(nrow(book))){
          
          self$bid <- "cooperate"
          
        } else if (nrow(book) > 2){
          if ((tail(book,2)[['bid2']] == c('defect','defect'))[1] & (tail(book,2)[['bid2']] == c('defect','defect'))[2]){
            self$bid <- "defect"
          }
        } else {
          self$bid <- "cooperate"
        }
        
      } else if(self$id == id2){
        if(!(nrow(book))) {
          
          self$bid <- "cooperate"
          
        } else if (nrow(book) > 2){
          if ((tail(book,2)[['bid1']] == c('defect','defect'))[1] & (tail(book,2)[['bid1']] == c('defect','defect'))[2]){
            self$bid <- "defect"
          }
        } else {
          self$bid <- "cooperate"
        }
      }
    },
    
    opponent_trigger_A = function(){
      
      if (self$opponent_defectin_firstround()){
        return(FALSE)
      }
      
      # return(TRUE)
      if(!(nrow(book))){
        condition <- (7 < (nrow(book)+1)) & (((nrow(book)+1) <= 180))
        return(condition)
      } else {
        return(FALSE)
      }
    },
    
    opponent_defectin_firstround = function(){
      if((self$id == id1) & (nrow(book) < 7)){
        if(nrow(subset(book, bid2 == "defect")) > 0){
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else if((self$id == id2) & (nrow(book) < 7)){
        if(nrow(subset(book, bid1 == "defect")) > 0){
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else {
        return(TRUE)
      }
    },
    
    myBackStabber = function(){
      if(self$id == id1){
        if(!(nrow(book))){
          
          self$bid <- "cooperate"
          
        } 
        else if (nrow(subset(book, bid2 == "defect")) > 3){
          self$bid <- "defect"
          
        } else {
          self$bid <- "cooperate"
        }
        
      } else if(self$id == id2){
        if(!(nrow(book))){
          
          self$bid <- "cooperate"
          
        } 
        else if (nrow(subset(book, bid1 == "defect")) > 3){
          self$bid <- "defect"
        } else {
          self$bid <- "cooperate"
        }
      }
    }
  )
)