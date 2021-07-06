## Shorting a stock

How it works. The short seller _borrows_ the stock from a broker, and
sells it to someone else. This is a _loan_: the broker loans the stock,
and is entitled to get it back. But it is known that the broker _might
not get it back_. Because of that risk, the short seller must pay the
broker interest.

The buyer in the short sale literally gets the stock right away. The
buyer of short stock has no counter-party risk. The counter-party risk
is entirely assumed by the broker.

When shorting, your broker can force you to deliver the stock early if
it goes down too much. This is a _margin call_. Thus you are susceptible
to a _squeeze_: the stock may temporarily go up a lot, forcing a margin
call on you.

## Naked short selling

"Naked" short selling is when you don't borrow the asset before selling
it. How can you sell what you don't have? Well, trades have a "delivery
window." Short selling is betting that you can buy the shares cheaper
before the delivery window expires.

If you "fail to deliver," what happens? Surely it can't be that the
buyer is just fucked?

**TODO**: clarify this!

## Value to the market

Let's say you expect a stock to decline in value. If you own that stock,
you will sell it: the upside is worth less than the downside. Now you
don't own the stock anymore.

When you don't own the stock, you _still_ might want to trade. You still
think the upside is overvalued relative to the downside. For that
reason, you want to trade short. In this way, your "information" is
getting priced into the market.

If you don't own a stock and want to buy it, you like people who short
the stock. They are increasing supply and thus decreasing the price to
buy at.

## Put Options

A put option is _similar_ to a short sale. When you buy a put option,
you are buying the option to sell a security at a given price during a
specified window.

**Buyer's Perspective**

Buying a put is like shorting the stock, except without any of the
downside risk. Instead of the infinite downside risk, you pay a
premium to the put writer.

Consider you own a stock. You're worried about the downside, which you
want to eliminate. However, you don't want to _short_ the stock; that
would eliminate your exposure to the _upside_! You might as well not own
the stock.

Instead, you can buy a put option. This is now a hedge for you. You've
basically bought an insurance policy.

**Seller's Perspective**

Now consider the _seller_ of a put option. The seller is assuming
downside risk in exchange for a premium.

The seller of the put can lose up to the entire value of the stock. This
is non-infinite, but it could be substantial. Put writers will have to
put up margin.

You might sell a put option if you think a company will be mostly
stable, and people are fretting about possible disasters too much.

(Note: selling a put option is always "naked" in the sense that the only
way to "cover" it is to also be short the stock.)

## Call options

A call option gives the buyer the right to purchase a security at a
fixed price during a specified window.

**Buyer's Perspective**

If you think a stock's value will go up, you may purchase a call option.
Your downside is limited to the premium, but your upside is potentially
infinite.

Why not simply purchase the stock? Reason: buying put options could give
you better leverage. Think about Apple stock: it's expensive! If I think
Apple will 10x, but no one else does, I can only buy a little Apple
stock. I can't even buy too much stock on margin: the broker will worry
about my ability to pay them back if the stock goes down.

However, for the same amount, I can buy _way more_ call options on Apple
stock, especially at like 5x the current price. People think there's no
chance of that, so they'll sell it to me cheaply. And they don't have to
worry about me as a counter-party, because I'm not borrowing anything.

**Seller's Perspective: Naked**

A seller of a call option has unlimited downside. They are effectively
short the stock. However, unlike the short, they don't get anything
special if the stock goes down! The seller only gets the premium.

You might do a naked sale of a call option if you think that people are
too optimistic about how well a stock may perform, but you have no
particular opinion about bad things that could happen to the stock.

To sell a naked call you must have margin, for the same reason as when
short selling.

**Seller's Perspective: Covered**

If you own a stock, but you're not particularly optimistic about its
performance (within the specified window), you might sell a call option.
You've sold away upside of the stock. Now, if the stock goes up, you
collect the premium but lose the upside you previously had. You can
_miss out_ on infinite upside, but you can't _lose_ money by the stock
going up.

If the stock goes down, you're in the same position as when you just
owned the stock. The buyer will let the option will expire, you'll still
get the premium, but you lose the value in the stock. At least you were
compensated for the upside you could have missed out on.

**Covered Call = Naked Put**

Think about selling a covered call:

- If the stock goes up in value, the counter-party gets the upside,
  while you are back at zero.
- If the stock goes down, they don't exercise their option. So you lose.
  You can lose up to your entire investment.
- In recognition for trading upside away without trading away downside,
  you are collecting a premium: the price you sold the option for.

When you sell the naked put:

- If the stock goes up in value, the counter-party will not exercise it.
  You collect the premium.
- If the stock goes down in value, the counter-party _will_ exercise it
  and sell you the depreciated stock at the locked-in strike price. If
  the stock goes to $0, you could lose the entire price of the stock.

Note: the _counter-party's_ experience is different. In the first case
they buy a call, and in the second case they buy a put. It seems
counter-intuitive that two equivalent trades _from your perspective_
would _not_ be equivalent _from the counter-party's perspective_. But
where did your shares come from in the covered call? There was a _third
party_ who sold you those. If you account for the third party,
everything balances out. There is still a "conservation of mass" going
on.

**Buy-Write**

A _buy-write_ strategy is simply where a trader will buy some shares at
the same time as selling a (covered) call option. As we've seen, this is
the same as selling a naked put option.

The difference is to the counter-party: a covered call has no risk of
non-delivery, whereas a naked put has counter-party risk.

## Uses of options

One example: developer wants to buy right to purchase land, but not
yet exercise, because they want to wait until other parcels are
purchased too. If there is a holdout, their strategy is worthless.

## Why are these valuable?

Reason: because you want liquidity, because people want to get the
best price they can when selling (and buying). That reduces frictions
in the market.

People have nonlinear responses to money, so having only instruments
that have linear response is not desirable. They also may predict
future events that have nonlinear responses, too.

The only problem is that these instruments can magnify exposure, which
means that you can get more wild fluctuations in the market. But if
people are covered, is that a harm?

## Do we need shorts at all?

There is a limited amount of covered calls that can be sold: equal to
the number of shares that exist. Selling more calls means you are
effectively short. To cover more, you might have to start buying
shares today on margin (which is the flip of short selling).

However, consider: two calls can cancel each other out. They may just
trade different parts of the risk curve.

## Why short at all?

For one, you lose the time aspect with options? But with shorting you
still have limited duration to borrow at the current interest rate.

I think the problem is this. Who is on the other side of your options
trade? Is it a market maker? (My notes ended in mid sentence here? I
don't know what my point would have been.)
