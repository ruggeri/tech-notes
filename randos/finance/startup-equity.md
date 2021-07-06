## RSUs vs Options

**ISO Options**

For options to be ISO they must have a strike price at the FMV at time
of issue. They can only be given to employees.

The government does not tax options upon vesting (even though in theory
they have value). In fact, the government will not tax ISO even *at
exercise*. (Exception: the government will use the spread between FMV
and strike price for calculation of AMT.)

If you do a *qualified disposition*, the government will tax the profit
on the option (difference between strike price paid and sale price) at
*long term capital gains*. A qualified disposition requires: (a) the
stock acquired and disposed is at least 2yrs from the grant date, (b)
you've held the stock at least 1yr when you make the sale, (c) you
exercised the option within 3mo of leaving the company.

Note that (c) does not mean the option *expires* within 3mo of leaving.
Merely that you lose the beneficial tax treatment.

If you do not meet all the rules above, you pay two kinds of tax. First,
you pay compensation tax on the difference between the strike price and
the FMV at exercise. Any profit above FMV at exercise is treated as any
other capital gain. In short: if you do a disqualified disposition you
get treated like an NSO (below).

Maximum ISO duration is 10 years.

**NSO**

ISO can only be given to employees. NSO can be given also to consultants
and directors.

NSO is taxed as income at exercise (spread between strike and FMV at
exercise). From there it is treated like capital gains.

**Sources**

http://www.startupcompanylawyer.com/2008/03/05/whats-the-difference-between-an-iso-and-an-nso/
http://www.woodllp.com/Publications/Articles/ma/100201a.htm
https://www.cooleygo.com/early-exercisable-stock-options-what-you-need-to-know/

**RSUs**

RSU only vests (1) over time, and (2) on a liquidity event. So no tax is
owed until company triggers. When stock becomes unrestricted, you have
to pay income tax that year for the stock's value on the market. But now
subsequently you start capital gains clock.

This differs from ISOs (and NSOs), which don't get taxed as they vest:
only at exercise. (But ISO and NSO are probably only exercised at
liquidity event anyway.)

It sounds like the company can typically give equivalent FMV cash
instead of stock at their choice.

RSUs aren't real stock so they don't make you a shareholder.

**ISOs vs RSUs**

Of course, basic incentive is different. ISOs is to grow (at risk of
failure), while RSU rewards preservation of value.

If the company is older, RSUs may be *worse* for the company. An RSU
compensates an employee for preservation of value. But at a young age,
the company shouldn't really care about ISO vs RSU.

For tax purposes, the ISO is better. At the liquidity event, you must
pay ordinary income tax on the RSU's value. For ISO, you don't pay that
ordinary income tax. You'll only pay the capital gains tax on the whole
amount.

So, at the beginning, companies should give ISOs because taxes.

Why give RSUs as the company grows? It seems worse for the company. Is
it simply to keep the number of shareholders low? Apparently this was a
major consideration for Facebook (one of the first issuers of RSUs).

The RSU has downside protection. If Uber raises at a $50bn valuation,
does an employee want to get fucked if it sells for $49bn? Whereas, when
the company is new, there's very little left on the table with the ISO
if the company goes down in value.

https://www.capshare.com/blog/rsus-vs-options/

## 83b and (Non-option) Equity Vesting

Let's say you're granted regular stock in a company. For instance, App
Academy grants us regular stock that vests over time.

Normally the IRS will only tax that stock as it vests (well, also if the
stock is publicly tradeable?). Basically: the IRS won't tax on something
you may not get to keep. When the stock vests, you'll be taxed at
ordinary income rates.

This is bad if you have stock that is appreciating rapidly. If App
Academy gets valued at $100MM at day 2, we'll have to pay ordinary
income tax on $12.5MM income each year of vesting! And we don't have any
cash for that!

You want to file an 83b election. This says: assume that all the stock
will vest, and start the clock from today. I'll pay ordinary income tax
on my entire stock grant at day 1. But the valuation on day 1 for App
Academy was maybe $1,000. So we pay income tax on $500 on day 1.

Now we won't owe any more tax until we *sell* the shares. That's very
important. Also, we will pay *LTCG capital gains* taxes on the share
appreciation.

## Early Exercise of NSOs

Most companies will let you *early exercise* your NSOs. This gives you
restricted stock. The company now has the right to buy that back (at the
lesser of the strike price and FMV) if you leave. You've basically moved
yourself from NSOs into the vesting restricted stock grant world
(similar to founders equity).

If you do nothing else, you will pay income tax on the difference
between the FMV and the exercise price as the shares vest. This moves
*forward* your income tax liability. However, you may owe *less* income
tax. As shares vest, the capital gains clock is started. So the clock
starts earlier, and at a lower cost basis.

You can also file an 83b election. This asks the government to assume
you will vest *all* the shares. In this case, you owe no income tax
(because strike = FMV). You will pay all appreciation as capital gains
tax: possibly long term capital gains.

Of course, early purchase is dangerous in the same way investment into
the company is dangerous. If the company's value goes to basically zero,
you've bought something worthless!

If you leave early, the company is not *obligated* to buy back your
stock: it has the *right* to purchase at the lesser of the current FMV
and original strike price.

## Early Exercise and ISOs

Early exercise of ISOs does not typically reduce your income tax, since
a qualified disposition would not have incurred any income tax.

The primary reason to early exercise ISOs is to limit the AMT impact. By
doing early exercise (with 83b), you treat the exercised stock as vested
on day 1. But that transaction was worth nothing. No income tax, and no
AMT. As the restricted stock vests, you pay no tax.

When you sell the shares, you will pay LTCG if you meet the qualified
disposition rules. You still must wait 2yrs after the option grant to
sell the stock. Many people think the 1yr capital gains clock starts
earlier, but that apparently *is not true*.

We've seen that if you do a typical disqualified disposition of ISO
stock, then you owe income tax on the difference between the FMV and the
strike price at the time of exercise. In the case that you did an 83b,
you pay income tax on the spread that existed *when the restricted stock
vested*. (You pay this in the year of the disqualified disposition.) You
also only get to start the capital gains clock from the time the
restricted stock vested.

A disqualified ISO transaction with a forward exercise is *worse* than
the same forward exercise on an NSO. Because the NSO will always have
had no income tax, and the NSO will always have started the clock on day
one. For this reason, one of my sources says:

    If the company knows that an employee will immediately
    early exercise her options, it makes more sense to grant the employee
    her option as an NSO so as to avoid a special two-year ISO holding
    period requirement.

Last: if you do a disqualified disposition on ISO shares, it's still
better to have early exercised. Reason: you'll owe income tax on the
spread at the time of vesting, and be responsible for capital gains from
that time, too.

In conclusion:

* If you know you'll immediately exercise, ask for NSOs.
* If you don't expect to, go with ISO per usual.
* You still (probably) want to early exercise your ISOs as soon as you
  know that the (1) the value will hold, and (2) you'll vest the
  exercised options.

https://blog.visor.com/equity/tax-early-exercising-isos/
https://www.dorsey.com/newsresources/publications/client-alerts/2017/04/unexpected-risks-of-early-exercise-isos
https://www.cooleygo.com/early-exercisable-stock-options-what-you-need-to-know/
https://thestartuplawblog.com/immediately-exercisable-isos-the-problems/
