import Test.Cabal.Prelude
-- Test that we can resolve a module name ambiguity when reexporting
-- by explicitly specifying what package we want.
main = cabalTest $ do
    withDirectory "multiple-cabal-files" $
        fails $ cabal "v2-build" []
    -- withDirectory "multiple-cabal-files-project" $
    --     fails $ cabal "v2-build" ["all"]
