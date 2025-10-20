# ********************
#  Last names:
#  Language: R
#  Paradigm(s): Functional, Procedural
#  ********************

# Global variables
exchange_rates <- list(PHP = 1.0, USD = 0.0, JPY = 0.0,
                       GBP = 0.0, EUR = 0.0, CNY = 0.0)

main_menu <- function(account = NULL, rate = exchange_rates) {

  print("Select Transaction")
  print("[1] Register Account Name")
  print("[2] Deposit Amount")
  print("[3] Withdraw Amount")
  print("[4] Currency Exchange")
  print("[5] Record Exchange Rates")
  print("[6] Show Interest Computation")

  response <- readline(prompt = "Enter Choice: ")

  if (response == 1) {
    account <- register_acc_name()
    main_menu(account)
  } else if (response == 2) {
    if (is.null(account)) {
      print("Please register an account first!")
      main_menu(account)
    } else {
      account <- deposit_amount(account)
      main_menu(account)
    }
  } else if (response == 3) {
    if (is.null(account)) {
      print("Please register an account first!")
      main_menu(account)
    } else {
      account <- withdraw_amount(account)
      main_menu(account)
    }
  } else if (response == 4) {
    if (is.null(account)) {
      print("Please register an account first!")
      main_menu(account)
    } else {
      currency_exchange(account)
    }
  } else if (response == 5) {
    if (is.null(account)) {
      print("Please register an account first!")
      main_menu(account)
    } else {
      account <- record_exchange_rate(rate)
      main_menu(account)
    }
  } else if (response == 6) {
    if (is.null(account)) {
      print("Please register an account first!")
      main_menu(account)
    } else {
      account <- show_interest_amount(account)
      main_menu(account)
    }
  } else {
    print("Invalid Option!")
    main_menu(account)
  }
}

show_interest_amount <- function(account) {
  print("Show Interest Amount")
  print(paste("Account Name:", account$name))
  print(paste("Current Balance:", sprintf("%.2f", account$balance)))
  print(paste("Currency:", account$currency))

  interest_rate <- 0.05
  print(paste("Interest Rate:", paste0(interest_rate * 100, "%")))

  get_total_days <- as.numeric(readline(prompt = "Total Number of Days: "))

  if (!is.na(get_total_days) && get_total_days > 0) {
    print(paste("Total Number of Days:", paste0(get_total_days, " days")))

    cat(sprintf("%-5s | %-10s | %-10s\n", "Day", "Interest", "Balance"))
    cat(strrep("-", 35), "\n")

    current_balance <- account$balance

    for (day in 1:get_total_days) {
      # Daily Interest = (End-of-Day Balance) x (Annual Interest Rate / 365)
      daily_interest <- current_balance * (interest_rate / 365)
      current_balance <- current_balance + daily_interest

      # Print the day, interest, and balance
      cat(sprintf("%-5d | %-10s | %-10s\n",
                  day,
                  sprintf("%.2f", daily_interest),
                  sprintf("%.2f", current_balance)))
    }

  } else {
    print("Invalid number of days!")
  }

  response <- readline(prompt = "Back to Main Menu (Y/N): ")

  if (response == "Y" || response == "y") {
    return(account)
  } else if (response == "N" || response == "n") {
    return(show_interest_amount(account))
  }

  return(account)
}

record_exchange_rate <- function(rate) {
  print("Record Exchange Rate")
  print("[1] Philippines Peso (PHP)")
  print("[2] United States Dollar (USD)")
  print("[3] Japanese Yen (JPY)")
  print("[4] British Pound Sterling (GBP)")
  print("[5] Euro (EUR)")
  print("[6] Chinese Yuan Renminbi (CNY)")

  get_num <- as.numeric(readline(prompt = "Select Foreign Currency: "))
  rate_value <- as.numeric(readline(prompt = "Exchange Rate: "))

  if (get_num == 1) {
    rate$PHP <- rate_value
  } else if (get_num == 2) {
    rate$USD <- rate_value
  } else if (get_num == 3) {
    rate$JPY <- rate_value
  } else if (get_num == 4) {
    rate$GBP <- rate_value
  } else if (get_num == 5) {
    rate$EUR <- rate_value
  } else if (get_num == 6) {
    rate$CNY <- rate_value
  }

  response <- readline(prompt = "Back to Main Menu (Y/N): ")
  if (response == "Y" || response == "y") {
    return(rate)
  } else if (response == "N" || response == "n") {
    return(record_exchange_rate(rate))
  }
  return(rate)
}

currency_exchange <- function(account, rate){
  print("Foreign Currency Exchange")
  print("Source Currency Option: ")
  print("[1] Philippines Peso (PHP)")
  print("[2] United States Dollar (USD)")
  print("[3] Japanese Yen (JPY)")
  print("[4] British Pound Sterling (GBP)")
  print("[5] Euro (EUR)")
  print("[6] Chinese Yuan Renminbi (CNY)")

  get_source <- as.numeric(readline(prompt = "Source Currency: "))
  get_amount <- as.numeric(readline(prompt = "Source Amount: "))

  print("Exchange Currency Options: ")
  print("[1] Philippines Peso (PHP)")
  print("[2] United States Dollar (USD)")
  print("[3] Japanese Yen (JPY)")
  print("[4] British Pound Sterling (GBP)")
  print("[5] Euro (EUR)")
  print("[6] Chinese Yuan Renminbi (CNY)")

  get_exchange <- as.numeric(readline(prompt="Exchange Currency: "))

  if (get_source == 1){
    if (get_exchange == 2){
      get <- get_amount * rate$USD
    } else if (get_exchange == 3){
      get <- get_amount * rate$JPY
    } else if (get_exchange == 4){
      get <- get_amount * rate$GBP
    } else if (get_exchange == 5){
      get <- get_amount * rate$EUR
    } else if (get_exchange == 6){
      get <- get_amount * rate$CNY
    }
  } else if (get_source == 2){
    if (get_exchange == 1){
      get <- get_amount * rate$PHP
    } else if (get_exchange == 3){
      get <- get_amount * rate$JPY
    } else if (get_exchange == 4){
      get <- get_amount * rate$GBP
    } else if (get_exchange == 5){
      get <- get_amount * rate$EUR
    } else if (get_exchange == 6){
      get <- get_amount * rate$CNY
    }
  } else if (get_source == 3){
    if (get_exchange == 1){
      get <- get_amount * rate$PHP
    } else if (get_exchange == 2){
      get <- get_amount * rate$USD
    } else if (get_exchange == 4){
      get <- get_amount * rate$GBP
    } else if (get_exchange == 5){
      get <- get_amount * rate$EUR
    } else if (get_exchange == 6){
      get <- get_amount * rate$CNY
    }
  } else if (get_source == 4){
    if (get_exchange == 1){
      get <- get_amount * rate$PHP
    } else if (get_exchange == 2){
      get <- get_amount * rate$USD
    } else if (get_exchange == 3){
      get <- get_amount * rate$JPY
    } else if (get_exchange == 5){
      get <- get_amount * rate$EUR
    } else if (get_exchange == 6){
      get <- get_amount * rate$CNY
    }
  } else if (get_source == 5){
    if (get_exchange == 1){
      get <- get_amount * rate$PHP
    } else if (get_exchange == 2){
      get <- get_amount * rate$USD
    } else if (get_exchange == 3){
      get <- get_amount * rate$JPY
    } else if (get_exchange == 4){
      get <- get_amount * rate$GBP
    } else if (get_exchange == 6){
      get <- get_amount * rate$CNY
    }
  } else if (get_source == 6){
    if (get_exchange == 1){
      get <- get_amount * rate$PHP
    } else if (get_exchange == 2){
      get <- get_amount * rate$USD
    } else if (get_exchange == 3){
      get <- get_amount * rate$JPY
    } else if (get_exchange == 4){
      get <- get_amount * rate$GBP
    } else if (get_exchange == 5){
      get <- get_amount * rate$EUR
    } else if (get_exchange == 6){
      get <- get_amount * rate$CNY
    }
  }

  print(paste("Exchanged Amount:", get))

  convert_response <- readline(prompt = "Convert another amount (Y/N): ")
  if (convert_response == "Y" || convert_response == "y") {
    return(currency_exchange(rate))
  } else if (convert_response == "N" || convert_response == "n") {
    main_menu(account)
  }
}


deposit_amount <- function(account){
  print("Deposit Amount")
  print(paste("Account Name:", account$name))
  print(paste("Current Balance:", account$balance))
  print(paste("Currency:", account$currency))

  get_amount <- as.numeric(readline(prompt="Deposit Amount: "))

  if (!is.na(get_amount) && get_amount > 0){
    account$balance <- account$balance + get_amount
    print(paste("Updated Balance:", account$balance))
  } else {
    print("Invalid amount!")
  }

  return(account)
}

withdraw_amount <- function(account){
  print("Withdraw Amount")
  print(paste("Account Name:", account$name))
  print(paste("Current Balance:", account$balance))
  print(paste("Currency:", account$currency))

  get_amount <- as.numeric(readline(prompt="Withdraw Amount: "))

  if(!is.na(get_amount) && get_amount > 0 && get_amount <= account$balance){
    account$balance <- account$balance - get_amount
    print(paste("Updated Balance:", account$balance))
  } else {
    print("Invalid amount!")
  }

  return(account)
}

register_acc_name <- function(){
  name <- readline(prompt = "Account Name: ")
  registered_name <- list(name = name, balance = 0, currency = "PHP")

  print(paste("Account registered:", registered_name$name))
  print(paste("Balance:", registered_name$balance))
  print(paste("Currency:", registered_name$currency))

  response <- readline(prompt = "Back to Main Menu (Y/N): ")

  if (response == "Y" | response == "y"){
    return(registered_name)
  } else if (response == "N" | response == "n"){
    return(register_acc_name())
  }

  return(registered_name)
}

main_menu()
