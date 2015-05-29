import Check exposing (..)
import Check.Investigator exposing (..)
import Check.Runner.Browser exposing (..)

square x = x * x

claim_squaring_root_yields_original =
  claim
    "Squaring the root of a number yields the original number"
  `that`
    (\x -> square (sqrt x))
  `is`
    (identity)
  `for`
    float

suite_math =
  suite "Math suite"
    [ claim_squaring_root_yields_original
    ]

result = quickCheck suite_math

main = display result