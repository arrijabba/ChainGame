#include "chaingame.mligo"

let generate_storage : storage =

  let start_time : timestamp = ("2022-12-04T01:30:00Z" : timestamp) in

  let one_tez : tez = 1mutez * 1_000_000n in

  let test_token : fa_currency = {
    fa2_address = ("KT1FgEEoYgnTjtWfYMGHy3t4y2ewbfFRATib": address);
    token_id = (Some 0n)
  } in 

  {
    start_time = start_time;
    
    tourney_currency = test_token;

    tourney_buyin = 1000n;
    
    tourney_payout = 0n;
    
    current_round = 0n;
    
    current_game_id = 0n;

    current_round_start = start_time;

    round_time = 150n;

    tourney_rounds  = 2n;

    tourney_players = 4n;

    pending_game = (None : game option);

    round_players = (Big_map.empty : (nat, address list) big_map);      
    play_history  = (Big_map.empty : (user_key, unit) big_map);    
    games         = (Big_map.empty : (nat, game) big_map);    
  }
