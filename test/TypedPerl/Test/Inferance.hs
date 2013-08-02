module TypedPerl.Test.Inferance (
  tests
  ) where
import Test.HUnit
import TypedPerl.Inferance
import TypedPerl.Parsec
import TypedPerl.Types

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
  )
  -- TODO: our parser can't parse start with empty sentences
  -- , (TestCase $ do
  --     let Right ty = inferCode ";;;;5"
  --     assertEqual "Semicollons" ty (TypeBuiltin TypeInt)
  -- )
  , (TestCase $ do
      let Right ty = inferCode "sub { 1 }->()"
      assertEqual "int(no args)" ty (TypeBuiltin TypeInt)
  )
  , (TestCase $ do
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
      let Right ty = inferCode "my $f = sub { my $x = $_[0]; sub { $_[0] x $x } }->(3); $f->(\"PASS STRING\")"
      assertEqual "my var" ty (TypeBuiltin TypeStr)
  )
  , (TestCase $ do
      let Right ty = inferCode "sub x { 1 }"
      assertEqual "decrare is statement" ty TypeUnknown
  )
  , (TestCase $ do
      let Left e = inferCode "sub x { sub y { 1 }; $_[0] }"
      putStrLn ('\n':e)
      assertBool "Don't next sub declare" ((not . null) e)
  )
  , (TestCase $ do
      let Right ty = inferCode "sub add { $_[0] + 2 } add(3)"
      assertEqual "Declare subroutin and call it" ty (TypeBuiltin TypeInt)
  )
  , (TestCase $ do
      let Right ty = inferCode "sub add { $_[0] + 2 }; add(3)"
      assertEqual "Sub-dec with semi-collon" ty (TypeBuiltin TypeInt)
  )
  , (TestCase $ do
      let Right ty = inferCode "sub {$_[0]->(0) + 0}->(sub {$_[0]})"
      assertEqual "decrare is statement" ty (TypeBuiltin TypeInt)
  )
  , (TestCase $ do
      let Right ty = inferCode "sub { $_[1] }->(\"\", 2)"
      assertEqual "2 args" ty (TypeBuiltin TypeInt)
  )
  , (TestCase $ do
      let Right ty = inferCode "sub { $_[1] }->(\"\", 2, 3)"
      assertEqual "more args are O.K." ty (TypeBuiltin TypeInt)
  )
  , (TestCase $ do
      let Left e = inferCode "sub { $_[1] }->(\"\")"
      putStrLn ('\n':e)
      assertBool "less args" ((not . null) e)
  )
  , (TestCase $ do
      let Right ty = inferCode "(bless {abc => 3}, \"X\")->{abc}"
      assertEqual "field access" ty (TypeBuiltin TypeInt)
  )
  , (TestCase $ do
      let Left e = inferCode "(bless {abc => 3}, \"X\")->{def}"
      putStrLn ('\n':e)
      assertBool "no fields" ((not . null) e)
  )
  , (TestCase $ do
      let Right ty = inferCode "sub new { bless {name => \"abcde\"}, \"Person\" }\nsub name { $_[0]->{name} }\nname(new());"
      assertEqual "complicated codes" ty (TypeBuiltin TypeStr)
  )
  , (TestCase $ do
      let Right ty = inferCode "my $x = bless {x => \"x\"}, \"Person\"; $x->{x}"
      assertEqual "complicated codes" ty (TypeBuiltin TypeStr)
  )
  , (TestCase $ do
      let Left e = inferCode "sub { my $f = $_[0]; my $g = sub { $f->($_[0]->($_[0])); }; $g->($g); }"
      putStrLn ('\n':e)
      assertBool "y-combinater" ((not . null) e)
  )
  , (TestCase $ do
      let Right ty = inferCode "my $x = sub { $_[0] }; $x->(\"\",0); $x->(0);"
      assertEqual "complicated codes" ty (TypeBuiltin TypeInt)
  )
  ]
