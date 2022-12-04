#include "./fa2/fa2_interface.mligo"
#include "fa_currency_lib.mligo"

(* You can also hot swap the commit/reveal scheme for off-chain oracle RNG resolver *)

(* Fow now, we use a simple 50/50 coinflip resolution, but going further users would enter with their own NFT that has stats, etc. for a better gameplay *)
(* Remember: The RNG for all games comes from the two players using a commit/reveal scheme for maximum security.  *)
(* RNG + NFTs w/ Stats = a good game, because it brings in some controllable variance to the normal RNG simulator process :D *)
type start_game = [@layout:comb] {
    hashed_secret : bytes;
}

type resolve_game = [@layout:comb] {
    game_id: nat;
    revealed_secret : bytes;
}

type game_stage =
  | Maker_Committed
  | Taker_Committed
  | Maker_Revealed
  | Game_Resolved

type game = [@layout:comb] {
    stage : game_stage;

    round : nat;

    player1 : address;
    p1_hash: bytes;
    p1_secret: bytes option;

    player2 : address option;
    p2_hash: bytes option;
    p2_secret: bytes option;

    start_time : timestamp;
    ended : timestamp option;
}

type user_key = [@layout:comb] {
    round : nat;
    player : address;
}

type storage = [@layout:comb] {
    
    (* Set before deployment (by a contract factory likely.) *)
    start_time : timestamp;

    (* Money/Currency *)

    (* FA1.2 or FA2 *)
    tourney_currency : fa_currency;

    (* token price per buyin *)
    tourney_buyin : nat;

    (* All goes to the winner. Operator can add some at the start or start at 0 *)
    tourney_payout : nat;
      
    (* Rounds *)

    (* Anyone can move rounds forward. *)
    current_round : nat;

    (* Initialize to equal start time *)
    current_round_start : timestamp;

    (* Seconds before round can move forward *)
    round_time : nat;

    (* The number of rounds for this tournament *)
    tourney_rounds : nat;
    
    (* Players *)

    (* This should be 2^(tourney_rounds) *)
    tourney_players: nat;

    (* For each round, we push the valid addresses into map. *)
    (* This is edited after games are won in specific rounds. *)
    round_players : (nat, address list) big_map;

    (* Holds the history so we can check for replayers.  *)
    play_history : (user_key, unit) big_map;

    (* Games *)

    (* Holds the next potential game, basically P1 is looking for a P2 *)
    pending_game : (game option);

    (* Holds the history of each game. *)
    games : (nat, game) big_map;
    
    current_game_id : nat;
}

type return = operation list * storage

(* 


  _    _ _______ _____ _      
 | |  | |__   __|_   _| |     
 | |  | |  | |    | | | |     
 | |  | |  | |    | | | |     
 | |__| |  | |   _| |_| |____ 
  \____/   |_|  |_____|______|
                              
                              


*)

let p1_commit_check (curr : game_stage) : bool = match curr with
  | Maker_Committed -> true
  | Taker_Committed -> false
  | Maker_Revealed -> false
  | Game_Resolved -> false


let p2_commit_check (curr : game_stage) : bool = match curr with
  | Maker_Committed -> false
  | Taker_Committed -> true
  | Maker_Revealed -> false
  | Game_Resolved -> false


let p1_reveal_check (curr : game_stage) : bool = match curr with
  | Maker_Committed -> false
  | Taker_Committed -> false
  | Maker_Revealed -> true
  | Game_Resolved -> false


let game_resolved_check (curr : game_stage) : bool = match curr with
  | Maker_Committed -> false
  | Taker_Committed -> false
  | Maker_Revealed -> false
  | Game_Resolved -> true

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit

let validate_secret (sec_hash, revealed : bytes * bytes) : bool = if Crypto.sha256 revealed <> sec_hash then false else true

let add_user_to_history (usr, storage : user_key * storage) : (user_key, unit) big_map = Big_map.update usr (Some unit) storage.play_history

let get_game_data (game_id, storage : nat * storage) : game =
  match (Big_map.find_opt game_id storage.games) with
      None -> (failwith "NO_GAME" : game)
    | Some g -> g

let get_round_players (round, storage : nat * storage) : address list =
  match (Big_map.find_opt round storage.round_players) with
      None -> ([] : address list)
    | Some g -> g

let add_user_to_players (user, round, storage : address * nat * storage) : (nat, address list) big_map = begin
  let current_players : address list = user::get_round_players(round, storage) in
  Big_map.update round (Some current_players) storage.round_players
end

let ensure_contest_running     (storage : storage) : unit = assert_msg (Tezos.get_now() >= storage.start_time, "CONTEST_NOT_STARTED")

let ensure_contest_not_started (storage : storage) : unit = assert_msg (Tezos.get_now() <  storage.start_time, "CONTEST_STARTED")

(* Since we start with a 0  index, this will fail is equal. i.e if you want 3 rounds, itll be valid on 0, 1, 2 *)
let ensure_valid_round (storage : storage) : unit = assert_msg (storage.tourney_rounds > storage.current_round, "CONTEST_FINISHED")

(* Should be equal on finish. TODO is check if it goes over *)
let ensure_contest_finished (storage : storage) : unit = assert_msg (storage.tourney_rounds <= storage.current_round, "CONTEST_ONGOING")

(* Add roundtime to start of round. *)
let ensure_round_complete (storage : storage) : unit = assert_msg (storage.current_round_start + int(storage.round_time) < Tezos.get_now(), "ROUND_ONGOING")

(* Check if adding one more user will go past users limit. *)
let fail_if_too_many_users (storage : storage) : unit = begin
  let users : address list = get_round_players(0n, storage) in
  assert_msg (List.length(users) + 1n <= storage.tourney_players, "TOO_MANY_USERS")
end

(* Check if adding one more user will go past users limit. *)
let fail_if_already_joined (user, storage : address * storage) : unit = begin
  let users : address list = get_round_players(0n, storage) in
  List.fold (fun (_, confirmed : unit * address) -> assert_msg (confirmed <> user, "USER_ALREADY_JOINED")) users (unit)
end

(* Check if adding one more user will go past users limit. *)
let ensure_in_player_list (user, storage : address * storage) : unit = begin
  let users : address list = get_round_players(storage.current_round, storage) in
  let check : bool = List.fold (fun (test, confirmed : bool * address) -> if confirmed = user then true else test) users false in
  assert_msg (check = true, "USER_NOT_FOUND")
end

(* Check if user already moved this round. *)
let ensure_hasnt_played (key, storage : user_key * storage) : unit =
  match (Big_map.find_opt key storage.play_history) with
      None -> unit
    | Some g -> (failwith "USER_PLAYED_ALREADY" : unit)

(* 


   _____          __  __ ______    _____ _______       _____ ______  _____ 
  / ____|   /\   |  \/  |  ____|  / ____|__   __|/\   / ____|  ____|/ ____|
 | |  __   /  \  | \  / | |__    | (___    | |  /  \ | |  __| |__  | (___  
 | | |_ | / /\ \ | |\/| |  __|    \___ \   | | / /\ \| | |_ |  __|  \___ \ 
 | |__| |/ ____ \| |  | | |____   ____) |  | |/ ____ \ |__| | |____ ____) |
  \_____/_/    \_\_|  |_|______| |_____/   |_/_/    \_\_____|______|_____/ 
                                                                           
                                                                           


*)

(* Btw, in production you would want to do some sort of trusted distribution to your project supporters. *)
(* You don't want ppl using multiple wallets here. *)
let buyin (self, storage : address * storage) : return = begin

    let user : address = Tezos.get_sender() in

    let () = ensure_contest_not_started(storage) in
    let () = fail_if_already_joined(user, storage) in
    let () = fail_if_too_many_users(storage) in

    (* Send token fee from user directly to self *)
    let buy_in_fee_transfer : operation = fa_transfer(Tezos.get_sender(), self, storage.tourney_buyin, storage.tourney_currency) in

    (* Should still be Round 0, game hasn't started. *)
    let add_user_to_round : (nat, address list) big_map = add_user_to_players(user, 0n, storage) in

    ([buy_in_fee_transfer], { storage with round_players = add_user_to_round; tourney_payout = storage.tourney_payout + storage.tourney_buyin })
end

(* This is sort of protection against maligned actors. Each round has a set time period, after that ANYONE can call this to move it forward. *)
(* I mean ANYONE, not just the owner. So all players know that if they try and lag, they'll miss getting to the next round. *)
let change_round (self, storage : address * storage) : return = begin

    let () = ensure_contest_running(storage) in
    let () = ensure_valid_round(storage) in
    let () = ensure_round_complete(storage) in

    (([] : operation list), { storage with current_round = storage.current_round + 1n; current_round_start = Tezos.get_now() })
end

(* Again, anyone can call this and make sure the winner actually gets paid out as they should, no cheats! *)
let finish (self, storage : address * storage) : return = begin

    let () = ensure_contest_finished(storage) in

    let round_list : address list = get_round_players(storage.current_round, storage) in

    (* If the tournament was set up correctly, there will be one player left. *)
    let winner : address = match List.head_opt round_list with
        None -> (failwith "FATAL_NO_WINNER" : address)
      | Some usr -> usr 
    in

    (* Send token fee from self directly to winner *)
    let winners_transfer : operation = fa_transfer(self, winner, storage.tourney_payout, storage.tourney_currency) in

    (* This is a single-use contract so after this transfer we are done. *)
    ([winners_transfer], { storage with current_round = storage.current_round + 1n; })
end

(* 


  _____  _           __     __   _______ ____  __  __ __  __ _____ _________  
 |  __ \| |        /\\ \   / /  / / ____/ __ \|  \/  |  \/  |_   _|__   __\ \ 
 | |__) | |       /  \\ \_/ /  | | |   | |  | | \  / | \  / | | |    | |   | |
 |  ___/| |      / /\ \\   /   | | |   | |  | | |\/| | |\/| | | |    | |   | |
 | |    | |____ / ____ \| |    | | |___| |__| | |  | | |  | |_| |_   | |   | |
 |_|    |______/_/    \_\_|    | |\_____\____/|_|  |_|_|  |_|_____|  |_|   | |
                                \_\                                       /_/ 
                                                                              


*)

let player1_make (self, params, storage : address * start_game * storage) : return = begin

    let user : address = Tezos.get_sender() in

    let user_key : user_key= {
      round = storage.current_round;
      player = user;
    } in

    let () = ensure_hasnt_played(user_key, storage) in

    (* Games run in lockstep. First user to hit play will be P1 *)
    (* On the next run, the user will be Player 2 *)
    let game_data : game = {

        stage = (Maker_Committed : game_stage);
        
        round = storage.current_round;

        player1 = user;
        p1_hash = params.hashed_secret;
        p1_secret = (None : bytes option);

        player2 = (None : address option);
        p2_hash = (None : bytes option);
        p2_secret = (None : bytes option);

        start_time = Tezos.get_now();
        ended = (None : timestamp option);
    } in

    let add_to_history : (user_key, unit) big_map = add_user_to_history(user_key, storage) in
      
    (([] : operation list), { storage with pending_game = (Some game_data); play_history = add_to_history})
end

let player2_take (game, self, params, storage : game * address * start_game * storage) : return = begin

    let user : address = Tezos.get_sender() in

    let user_key : user_key= {
      round = storage.current_round;
      player = user;
    } in

    let () = ensure_hasnt_played(user_key, storage) in

    (* Make sure Player 1 committed a secret. *)
    if p1_commit_check(game.stage) then 

      (* Commit player 2's info. Reupdate current timestamp. *)
      let updated_game_data : game = 
      { 
        game with 
          stage = (Taker_Committed : game_stage); 
          player2 = (Some user : address option);
          p2_hash = (Some params.hashed_secret : bytes option); 
          start_time = Tezos.get_now();
      } in   

      (* This time we store in the big map, confirming the game *)
      let updated_games : (nat, game) big_map = Big_map.update storage.current_game_id (Some updated_game_data) storage.games in

      let add_to_history : (user_key, unit) big_map = add_user_to_history(user_key, storage) in

      (* Increase Game IDs *)
      (([] : operation list), { storage with games = updated_games; current_game_id = storage.current_game_id + 1n; play_history = add_to_history; pending_game = (None : game option) })

    else (failwith "STAGE_MISMATCH" : return)
end

let play (self, params, storage : address * start_game * storage) : return = begin

  let user : address = Tezos.get_sender() in

  (* Make sure contest is active *)
  let () = ensure_contest_running(storage) in
  let () = ensure_in_player_list(user, storage) in
  let () = ensure_valid_round(storage) in

  match storage.pending_game with
    (* Start from Player 1 *)
    None      -> player1_make(self, params, storage)
    (* Start from Player 2 *)
  | Some game -> player2_take(game, self, params, storage)
end

(* 


  _____  ______  _____  ____  _ __      ________    _______  ________      ________          _    __  
 |  __ \|  ____|/ ____|/ __ \| |\ \    / /  ____|  / /  __ \|  ____\ \    / /  ____|   /\   | |   \ \ 
 | |__) | |__  | (___ | |  | | | \ \  / /| |__    | || |__) | |__   \ \  / /| |__     /  \  | |    | |
 |  _  /|  __|  \___ \| |  | | |  \ \/ / |  __|   | ||  _  /|  __|   \ \/ / |  __|   / /\ \ | |    | |
 | | \ \| |____ ____) | |__| | |___\  /  | |____  | || | \ \| |____   \  /  | |____ / ____ \| |____| |
 |_|  \_\______|_____/ \____/|______\/   |______| | ||_|  \_\______|   \/   |______/_/    \_\______| |
                                                   \_\                                            /_/ 
                                                                                                      


*)

let resolve_game_internal (self, game, storage : address * game * storage) : (nat, address list) big_map = begin

  let p1      : address = game.player1 in

  let p1_secret : bytes = match game.p1_secret with
    | None -> (failwith "NO_MAKER_SECRET" : bytes)
    | Some h -> h
  in

  let p2      : address = match game.player2 with
    None -> (failwith "NO_TAKER" : address)
  | Some c ->  c
  in

  let p2_secret : bytes = match game.p2_secret with
    | None -> (failwith "NO_TAKER_SECRET" : bytes)
    | Some h -> h
  in

  (* Out of personal preferencee i', cutting this to two bytes. *)
  let game_bytes : bytes = Bytes.sub 30n 2n (Crypto.sha256 (Bytes.concat p1_secret p2_secret)) in

  (* I took this from a dice game. They have equal chances. *)
  let winner : address =  (
    if      (game_bytes < 0x2AAA) then p1
    else if (game_bytes < 0x5554) then p1
    else if (game_bytes < 0x7FFE) then p1
    else if (game_bytes < 0xAAA8) then p2
    else if (game_bytes < 0xD552) then p2
                                  else p2
  ) in

  add_user_to_players(winner, storage.current_round + 1n, storage)
end

let resolve (self, r, storage : address * resolve_game * storage) : return = begin

  let () = ensure_contest_running(storage) in

  let user : address = Tezos.get_sender() in

  let game_data : game = get_game_data(r.game_id, storage) in

  let () = assert_msg (game_data.round = storage.current_round, "INVALID_ROUND") in

  let p1 : address = game_data.player1 in

  let p1_hash : bytes = game_data.p1_hash in

  let p2 : address = match game_data.player2 with
    None -> (failwith "NO_P2" : address)
  | Some c ->  c
  in

  let p2_hash : bytes = match game_data.p2_hash with
    | None -> (failwith "NO_P2HASH" : bytes)
    | Some h -> h
  in

  (* Looking for Player 1's move first *)
  if p2_commit_check(game_data.stage) && user = p1 && validate_secret(p1_hash, r.revealed_secret) then
    
    let updated_game_data : game = 
    {
      game_data with 
        stage = (Maker_Revealed : game_stage); 
        p1_secret = (Some r.revealed_secret);
    } in

    let updated_games : (nat, game) big_map = Big_map.update r.game_id (Some updated_game_data) storage.games in

    (([] : operation list), {storage with games = updated_games})

  (* Looking for Player 2's move, then resolve *)
  else if p1_reveal_check(game_data.stage) && user = p2 && validate_secret(p2_hash, r.revealed_secret)  then

    let endts : timestamp = Tezos.get_now() in

    let updated_game_data : game = 
    {
      game_data with 
        stage = (Game_Resolved : game_stage); 
        p2_secret = (Some r.revealed_secret);
        ended = (Some endts);
    } in

    let updated_games : (nat, game) big_map = Big_map.update r.game_id (Some updated_game_data) storage.games in

    (* The actual RNG outcome of this game will decide who goes to the next round. *)
    (* One user gets stored in the next round's list, the other is eliminated. *)
    (* This works because users can only play in a round once. Only way to move up rounds is to play *)
    let next_round_list : (nat, address list) big_map = resolve_game_internal(self, updated_game_data, storage) in

    (([] : operation list), { storage with games = updated_games; round_players = next_round_list; })
  else (failwith "RESOLVE_FAIL" : return)

end

type game_entrypoints =
  | Join    of unit
  | Play    of start_game
  | Resolve of resolve_game
  | Next    of unit
  | End     of unit

let chaingame_main (p , storage : game_entrypoints * storage) : return = begin
  let self : address = Tezos.get_self_address() in
  match p with
    | Join      -> buyin(self, storage)
    | Play    s -> play(self, s, storage)
    | Resolve r -> resolve(self, r, storage)
    | Next      -> change_round(self, storage)
    | End       -> finish(self, storage)
end