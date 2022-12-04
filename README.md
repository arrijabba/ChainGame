# ChainGame

**ChainGame** is a generic contract made to be a game tournament contract with fully on-chain settlement to prevent cheating.

In its simplest case, ChainGame uses a round based system with commit-reveal gameplay to advance users to each round.]

The whole game is played on-chain, and the payout is **guaranteed** to go to the winner of the game at the end.

Moving forward, a major addition to this would be adding game liquidation for maligned actors.

In a real gaming ecosystem, this would use NFTs as characters or cards to apply different customizations and stats.

The commit-reveal architecture can also be changed to be external oracle RNG.

Parameters and fees obviously subject to tinkering. Contract is heavily commented, so look there for technicals.

Check the working happy path at [KT1FMYdjk4xjZpVvFLtL9fLmKZFgTUBsL9dh Ghostnet](https://better-call.dev/ghostnet/KT1FMYdjk4xjZpVvFLtL9fLmKZFgTUBsL9dh/operations).

--

Usage

ligo compile contract chaingame.mligo -e chaingame_main > ./chaingame.tz

ligo compile storage storage_chaingame.mligo -e chaingame_main 'generate_storage'

tezos-client originate contract chaingame transferring 0 from %YOUR_ADDRESS% running ./chaingame.tz --init '%STORAGE%' --burn-cap 3
