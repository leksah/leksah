import Test.DocTest

-- Unit Testing Entry Point
-- ------------------------
-- This is the main function uses by the cabal test
--
-- To run these tests
--   * Select Leksah menu item Package -> Test
--   * Select the tick icon on the Leksah toolbar (to enable "cabal test" during builds)
main :: IO ()
main = doctest ["-isrc", "src/Main.hs"]
