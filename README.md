# ChainGame

Usage

ligo compile contract chaingame.mligo -e chaingame_main > ./chaingame.tz

ligo compile storage storage_chaingame.mligo -e chaingame_main 'generate_storage'

tezos-client originate contract chaingame transferring 0 from %YOUR_ADDRESS% running ./chaingame.tz --init '%STORAGE%' --burn-cap 3

--

