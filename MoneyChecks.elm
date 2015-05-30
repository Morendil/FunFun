import Check exposing (..)
import Shrink exposing (noShrink)
import Check.Investigator exposing (..)
import Check.Runner.Browser exposing (..)
import Random exposing (..)
import Random.Int exposing (..)
import Random.Float exposing (..)

filteredTuple : ((a,b) -> Bool) -> (Investigator a, Investigator b) -> Investigator (a, b)
filteredTuple filter (invA, invB) =
  investigator
    (conditionalZip filter invA.generator invB.generator)
    (Shrink.tuple (invA.shrinker, invB.shrinker))

conditionalZip : ((a,b) -> Bool) -> Generator a -> Generator b -> Generator (a,b)
conditionalZip filter a_gen b_gen =
    customGenerator (\ seed ->
        let (candidate_a, seed_a) = generate a_gen seed
            (candidate_b, seed_b) = generate b_gen seed_a
            candidate = (candidate_a,candidate_b)
        in if filter candidate then (candidate,seed_b)
           else generate (conditionalZip filter a_gen b_gen) seed_b
    )

type Currency = EUR | GBP | USD
type alias Money = {currency:Currency, amount:Float}

selectCurrency selector =
    case selector of
            1 -> EUR
            2 -> GBP
            3 -> USD

moneyGen seed = 
    let currencyGenerator = Random.int 1 3
        amountGenerator = anyFloat
        (currencySelector, seedA) = generate currencyGenerator seed
        (actualAmout, seedB) = generate amountGenerator seedA
        actualCurrency = selectCurrency currencySelector
    in ({currency = actualCurrency, amount=actualAmout}, seedB)

money = investigator (customGenerator moneyGen) noShrink

add money1 money2 =
    if money1.currency == money2.currency then Just {currency=money1.currency, amount=money1.amount+money2.amount}
    else Nothing

moneyClaim =
    claim
        "Adding two different currencies yields an error"
      `that`
        (\(money1,money2) -> add money1 money2)
      `is`
        (always Nothing)
      `for`
        let differentCurrencies = (\(money1,money2) -> money1.currency /= money2.currency)
        in filteredTuple differentCurrencies (money, money)

moneyClaim2 =
    claim
        "Adding identical currencies adds amounts"
      `that`
        (\(money1,money2) -> add money1 money2)
      `is`
        (\(money1,money2) -> Just {currency=money1.currency, amount=money1.amount+money2.amount})
      `for`
        let sameCurrencies = (\(money1,money2) -> money1.currency == money2.currency)
        in filteredTuple sameCurrencies (money, money)

moneySuite = suite "Money suite" [moneyClaim,moneyClaim2]

result = quickCheck moneySuite

main = display result