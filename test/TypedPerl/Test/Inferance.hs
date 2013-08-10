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

inferCodeRight :: String -> PerlType
inferCodeRight code = either error id (inferCode code)

inferCodeLeft :: String -> String
inferCodeLeft code = either id (error . showPerlType) (inferCode code)

tests :: Test
tests = TestList [
  (TestCase $ do
      let ty = inferCodeRight "1"
      assertEqual "int" (TypeBuiltin TypeInt) ty
  )
  -- TODO: our parser can't parse start with empty sentences
  -- , (TestCase $ do
  --     let Right ty = inferCode ";;;;5"
  --     assertEqual "Semicollons" ty (TypeBuiltin TypeInt)
  -- )
  , (TestCase $ do
      let ty = inferCodeRight "sub { 1 }->()"
      assertEqual "int(no args)" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "sub { $_[0] }->(1)"
      assertEqual "int" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "sub { my $x = $_[0]; $x }->(1)"
      assertEqual "my var" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let e = inferCodeLeft "sub { my $x = $_[0] }; $x"
      putStrLn ('\n':e)
      assertBool "out of scope" ((not . null) e)
  )
  , (TestCase $ do
      let ty = inferCodeRight "my $f = sub { my $x = $_[0]; sub { $_[0] x $x } }->(3); $f->(\"PASS STRING\")"
      assertEqual "my var" (TypeBuiltin TypeStr) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "sub x { 1 }"
      assertEqual "decrare is statement" TypeUnknown ty
  )
  , (TestCase $ do
      let e = inferCodeLeft "sub x { sub y { 1 }; $_[0] }"
      putStrLn ('\n':e)
      assertBool "Don't next sub declare" ((not . null) e)
  )
  , (TestCase $ do
      let ty = inferCodeRight "sub add { $_[0] + 2 } add(3)"
      assertEqual "Declare subroutin and call it" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "sub add { $_[0] + 2 }; add(3)"
      assertEqual "Sub-dec with semi-collon" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "sub {$_[0]->(0) + 0}->(sub {$_[0]})"
      assertEqual "decrare is statement" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "sub { $_[1] }->(\"\", 2)"
      assertEqual "2 args" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "sub { $_[1] }->(\"\", 2, 3)"
      assertEqual "more args are O.K." (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let e = inferCodeLeft "sub { $_[1] }->(\"\")"
      putStrLn ('\n':e)
      assertBool "less args" ((not . null) e)
  )
  , (TestCase $ do
      let ty = inferCodeRight "(bless {abc => 3}, \"X\")->{abc}"
      assertEqual "field access" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let e = inferCodeLeft "(bless {abc => 3}, \"X\")->{def}"
      putStrLn ('\n':e)
      assertBool "no fields" ((not . null) e)
  )
  , (TestCase $ do
      let ty = inferCodeRight "sub new { bless {name => \"abcde\"}, \"Person\" }\nsub name { $_[0]->{name} }\nname(new());"
      assertEqual "complicated codes" (TypeBuiltin TypeStr) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "my $x = bless {x => \"x\"}, \"Person\"; $x->{x}"
      assertEqual "complicated codes" (TypeBuiltin TypeStr) ty
  )
  -- TODO: I don't know what this type is.
  -- , (TestCase $ do
  --     let e = inferCodeLeft "sub { my $f = $_[0]; my $g = sub { $f->($_[0]->($_[0])); }; $g->($g); }"
  --     putStrLn ('\n':e)
  --     assertBool "y-combinater" ((not . null) e)
  -- )
  -- TODO: I got "recursive row variable a15"
  -- , (TestCase $ do
  --     let ty = inferCodeRight "sub { $_[0]->($_[0]) }->(sub { $_[0] })->(1);"
  --     assertEqual "recursion" (TypeBuiltin TypeInt) ty
  -- )
  , (TestCase $ do
      let ty = inferCodeRight "sub { $_[0]->($_[0]) }->(sub { 1 })"
      assertEqual "simple recursion" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "my $x = sub { $_[0] }; $x->(\"\",0); $x->(0);"
      assertEqual "complicated codes" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "my $x = sub { print($_[0]) }; $x->(\"Hello\"); print(\", \"); $x->(999); 1"
      assertEqual "complicated codes" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let ty = inferCodeRight "2; package Hoge; sub h { 1 } h()"
      assertEqual "with Package" (TypeBuiltin TypeInt) ty
  )
  , (TestCase $ do
      let e = inferCodeLeft "sub f { 1 } package Foo; f()"
      putStrLn ('\n':e)
      assertBool "Don't call function of other package" ((not . null) e)
  )
  , (TestCase $ do
      let ty = inferCodeRight "1; package Foo; sub f { $_[0]->{n} } (bless {n => 10}, \"Foo\")->f()"
      assertEqual "method calls" (TypeBuiltin TypeInt) ty
  )
  -- TODO: Our inferance of method calling might be broken.
  , (TestCase $ do
      let ty = inferCodeRight "1; package Foo; sub f { $_[0]->g() } sub g { $_[0]->{n} } (bless {n => 10}, \"Foo\")->f()"
      assertEqual "method calls" (TypeBuiltin TypeInt) ty
  )
  ]
