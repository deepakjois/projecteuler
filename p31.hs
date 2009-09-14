uk_coins = [200,100, 50, 20, 10, 5, 2, 1]

cc amount coin_values
  | amount == 0                         =  1
  | amount < 0 || (no_more coin_values) =  0
  | otherwise                           = let a1 = (cc amount (except_first_denomination coin_values))
                                              a2 = (cc (amount - (first_denominaton coin_values)) coin_values)
                                          in a1+a2

no_more = null
except_first_denomination = tail
first_denominaton = head