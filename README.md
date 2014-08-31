# intolerable-bot

## how to get it working

1. make sure you have a recent version of [cabal](http://www.haskell.org/cabal/) installed
2. clone this repo somewhere
3. clone the [`reddit`](https://github.com/intolerable/reddit) repo into `../reddit` (relative to this repo)
4. `cabal sandbox init`
5. `cabal sandbox add-source ../reddit`
6. `cabal install`
7. `.cabal-sandbox/bin/intolerable-bot USERNAME PASSWORD SUBREDDIT_NAME` like `intolerable-bot intolerable-bot $PASSWORD Dota2`

## installing it to your global cabal installation

1. make sure you have a recent version of [cabal](http://www.haskell.org/cabal/) installed
2. clone this repo somewhere
3. clone the [`reddit`](https://github.com/intolerable/reddit) repo into `../reddit` (relative to this repo)
5. `cabal install ../reddit`
6. `cabal install`
7. `intolerable-bot USERNAME PASSWORD SUBREDDIT_NAME` (assuming your `cabal/bin` directory is in your `$PATH`)

## modifying the comment it posts

the comment that the bot posts is pulled from `reply.md`. edit that if you want to make it say something else

## suggesting changes to the reply

if you want to change the text for the original bot that runs on [/r/Dota2](http://reddit.com/r/Dota2), fork this repo, edit it, and send me a pull request