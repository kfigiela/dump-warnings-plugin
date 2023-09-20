module DumpWarningsExample where

import Data.Map as Unused -- should emit unused-import

foo = 123 -- should emit missing-signature and type-defaults

bar = do
    -- should emit missing-signature
    putStrLn $ show 123 -- should emit type-default
