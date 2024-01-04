Play_Session = function(min_bet, bet_delta, strategy,a,iter,money_start,bet_start){
  #the equivalent of one betting session of sitting at the wheel.Calls play round repeatedly and keeps track 
  #of maximum and minimum money on hand as well as how many rounds you have more money than you started with.
  money = money_start
  minimum_money = money
  maximum_money = money
  bet = bet_start
  money_bet = list(money, bet)
  rounds_in_green = 0
  
  for(i in 1:iter){
    
    money_bet = Play_Round(money_bet[[1]],money_bet[[2]],min_bet,bet_delta,strategy,a)
    
    if(money_bet[[1]] > maximum_money){
      maximum_money = money_bet[[1]]
    }
    if(money_bet[[1]] < minimum_money){
      minimum_money = money_bet[[1]]
    }
    if(money_bet[[1]] > money_start){
      rounds_in_green = rounds_in_green + 1
    }
  }
  
  return(list(maximum_money,minimum_money,rounds_in_green))
}
  



Play_Round = function(money, bet, min_bet, bet_delta, strategy,a){
  #the equivalent of one spin of the wheel and the resulting outcome to your money and bet amount.
  
  spin_result = runif(1)
  if(spin_result <= a){
    money = money+bet
    bet = Change_Bet(strategy,bet,bet_delta,TRUE,min_bet)
  }
  if(spin_result > a){
    money = money-bet
    bet = Change_Bet(strategy,bet,bet_delta,FALSE,min_bet)
  }
  return(list(money,bet))
  
}

Change_Bet = function(strategy,bet,bet_delta,win,min_bet){
  #how to change the bet amoutn depending on if you won or lost and the strategy
  if(strategy == "dalembert"){
    if(win){
      if(bet > min_bet){
      return(bet-bet_delta)
      }
      else{
      return(bet)
      }
    }
    if(!win){
      return(bet+bet_delta)
    }
  }
  if(strategy == "dalembert_rev"){
    if(win){
      if(bet > min_bet){
        return(bet+bet_delta)
      }
      else{
        return(bet)
      }
    }
    if(!win){
      if(bet > min_bet){
        return(bet-bet_delta)
      }
      else{
        return(bet)
      }
    }
  }
  if(strategy == "constant"){
    return(bet)
  }
  if(strategy == "random"){
    change = runif(1)
    if(change >= 0.5){
      return(bet+bet_delta)
    }
    if(change < 0.5){
      if(bet > min_bet){
        return(bet-bet_delta)
      }
      else{
        return(bet)
      }
    }
  }
  
}