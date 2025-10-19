# ********************
#  Last names:
#  Language:
#  Paradigm(s):
#  ********************

main_menu <- function (){
  print("Select Transaction")
  print("[1] Register Account Name") 
  print("[2] Deposit Amount")
  print("[3] Withdraw Amount")
  print("[4] Currency Exchange") 
  print("[5] Record Exchange Rates")
  print("[6] Show Interest Computation") 
  
  response <- readline(prompt="Number: ")
  
  if(response == 1){
    register_acc_name()
  } else if (response == 2){
    
  }
}

deposit_amount <- function(balance, currency){
  print("Deposit Amount \n")
  paste("Account Name: ", name)
  paste("Current Balance: ", balance)
  paste("Currency: ", currency)
  
  get_amount <- readline(prompt="Deposit Amount:")
}

register_acc_name <- function(){
  name <- readline(prompt= "Account Name: ")
  
  response <- readline(prompt= "Back to Main Menu (Y/N): ")
  
  if (response == 'Y' | response == 'y'){
    main_menu()
  } else if (response == 'N' | response == 'n'){
    register_acc_name()
  }
}


main_menu()
