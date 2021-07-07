## Futures

- A futures contract is an agreement to buy/sell an underlying asset at
  a set time.
- It locks in a strike price today, and the parties must transact. It is
  not optional, like options are.
- When you are long a futures contract, you are the party agreeing to
  make the purchase at the strike price. If the price subsequently goes
  down, you lose money (could have purchased at a lower price). If the
  price subsequently goes up, you make money (can purchase something
  more valuable for a lower price).
- The short side has the opposite exposure.
- Assuming non-negative prices, going long a futures contract has
  unlimited upside, but limited to 100% of the strike price as downside.
- If you own the underlying and could sell today at the strike price,
  then going short a futures contract would not make much sense.
  - One exception is the traditional case: farmers. Farmers wanted to
    lock in today's price for grain they would harvest later.
- Entering into a futures contract can be a way to bet on the delta
  between a strike price (such as today's price) and a future price.
  - It is symmetric, unlike an option, where the risk/reward is
    asymmetric.
  - Like options, it can be a way to gain leverage.
  - You need to put up some margin to assure that you can complete the
    future.
- I would assume that futures could be attractive to those who want to
  bet on price movements but aren't concerned about the trade moving
  against them?
- I believe you could manufacture a future by (1) buying a call option
  at the strike, and (2) selling a put option at the strike. If the
  stock goes up you make money from (1), while if the stock goes down
  you lose money from (2). You have gone short a future.

**Comment on leverage**

- If you want to bet big on a company, you should try to buy its stock
  on margin. There's high risk if it goes down, but you bet it will go
  up.
- You could buy futures on the stock at the current strike price. I'm
  not convinced this will increase your leverage further, though. You'll
  still need to settle the trade (in fact you have less flexibility
  about _when_ to settle).
- If you want to bet big, you could buy call options at very high
  prices. People will sell you this cheap because they will think you
  are smoking crack. Thus, you can buy much more. Of course, the price
  could increase and you could still lose your entire bet.

## Contract For Difference

- A contract for difference is a lot like a future.
- The difference is between the future and current price of an
  underlying asset.
  - The buyer gets the appreciation (hopes the underlying price will go
    up), while the seller gets the depreciation (hopes the underlying
    price will go down).
- Thus, the buyer is in the same position as the long side of a futures
  contract.
- So what is the difference from a future?
  - It's not super clear, buy CFDs appear to be mostly OTC, rather than
    exchange traded. Thus they have less price transparency.
  - Futures involve large blocks of shares, while CFDs can be for just a
    single unit of the underlying.
  - Also, it appears as if a CFD can be extended (by which side?) for as
    long as one side wants?
- Also: CFDs are cash-settled. Most contracts are physically settled:
  delivery of goods from short party to the exchange, and from exchange
  to long party. Though most contracts are effectively nullified by
  buying a covering contract.
  - Cash settlement was presumably a reason CFDs were invented. In the
    UK, buying or selling an asset meant that you had to pay a tax.
- CFDs are illegal in the United States.
- It sounds like the problem with CFDs are basically the same as with
  OTC stock trading? And that trading futures on the exchange is just
  better?

## Total Return Swaps

- A total rate of return swap is basically a CFD.
- The only difference is that you pay/receive the difference at set
  intervals of time.
- Archegos bought a bunch of TRS contracts. It's economically equivalent
  to buying stock on margin. It sounds like margin/swaps are practically
  the same thing, and that the only difference will be around
  regulation/reporting.

## Reminder

- These are all ways that insane people pump up leverage so they can
  make wack bets on huge price fluctuations.
