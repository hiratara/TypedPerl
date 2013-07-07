module Main (main) where
import Test.HUnit
import Ore.Inferance
import Ore.Parsec
import Ore.Types

inferCode :: String -> Either String PerlType
inferCode code = parsed >>= infer
  where
    parsed = case parsePerl code of
      Right t -> Right t
      Left e -> (Left . show) e

tests :: Test
tests = TestList [
  (TestCase $ do
      let Right ty = inferCode "1"
      assertEqual "int" ty (TypeBuiltin TypeInt)
  ),
  (TestCase $ do
      let Right ty = inferCode "sub { $_[0] }->(1)"
      assertEqual "int" ty (TypeBuiltin TypeInt)
  )
  , (TestCase $ do
      let Right ty = inferCode "sub { my $x = $_[0]; $x }->(1)"
      assertEqual "my var" ty (TypeBuiltin TypeInt)
  )
  , (TestCase $ do
      let Left e = inferCode "sub { my $x = $_[0] }; $x"
      putStrLn ('\n':e)
      assertBool "out of scope" ((not . null) e)
  )
  , (TestCase $ do
      let Right ty = inferCode "sub { my $x = $_[0]; sub { $_[0] x $x } }->(3)"
      assertEqual "my var" ty (TypeArrow (TypeBuiltin TypeStr)
                                         (TypeBuiltin TypeStr))
  )
  ]

main :: IO ()
main = runTestTT tests >> return ()
