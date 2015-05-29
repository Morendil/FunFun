import Check exposing (..)
import Shrink exposing (..)
import Check.Investigator exposing (..)
import Check.Runner.Browser exposing (..)
import Random.Int exposing (..)

square : Int -> Int
square x = x * x
sqrti : Int -> Int
sqrti = floor << sqrt << toFloat

claim_squaring_root_yields_original =
  claim
    "Squaring the root of a number yields the original number"
  `that`
    (\x -> sqrti (square x))
  `is`
    (identity)
  `for`
    investigator positiveInt Shrink.int

suite_math =
  suite "Math suite"
    [ claim_squaring_root_yields_original
    ]

result = quickCheck suite_math

main = display result