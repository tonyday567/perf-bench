{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Text as Text
import Options.Generic
import Perf
import Perf.Analysis
import Protolude
import Readme.Lhs

-- | format a run median
formatMedian :: (Integral a) => Text -> [[a]] -> [Text]
formatMedian label xss =
  [label]
    <> (formatF 2 . percentile 0.5 <$> xss)

formatRunsMedian :: (Integral a) => [Text] -> [(Text, [[a]])] -> Block
formatRunsMedian h rs =
  table
    mempty
    (["run"] <> h)
    ([AlignLeft] <> replicate n AlignRight)
    []
    (fmap (uncurry formatMedian) rs)
  where
    n = length h

newtype Opts
  = Opts
      { runs :: Maybe Int -- <?> "number of runs"
      }
  deriving (Generic, Show)

instance ParseRecord Opts


u1 = reverse 
       . fmap snd 
       . takeWhile (/= (0::Int, 0)) 
       . iterate (divMod10 . fst) 
       . divMod10
  where divMod10 = flip divMod 10

u2 = reverse . toList . unfoldr ((\case (0,y) -> (y, Nothing); (x,y) -> (y, Just x)) . (`divMod` 10))
u3 = reverse . unfoldr ((\(d,m) -> bool (Just (m,d)) Nothing (d==0 && m==0)) . (`divMod` 10))


main :: IO ()
main = do
  o :: Opts <- getRecord "benchmarking day 4"
  let !n = fromMaybe 100 (runs o)
  _ <- warmup 100
  let ts :: [Text]
      ts = ["Iterate", "NonEmpty.unfoldr", "List.unfoldr"]

  let x = 234567
  t1 <- ticks n u1 x
  putStrLn ("unfolds" :: Text)
  putStrLn $ ("iterate" :: Text) <> formatF 2 (percentile 0.5 (fst t1))

  void
    $ runOutput
      ("other/results_.md", GitHubMarkdown)
      ("results.md", GitHubMarkdown)
    $ do
      pure ()
{-
      output "values" $ Native $ (: []) $
        bool
          (table mempty [] [] [] [[Text.pack . show <$> fst $ r100]])
          (Para [Str "results agree"])
          (and $ (==) <$> fst r100 <*> fst r100)
      output "inner" $
        Native
          [ formatRunsMedian
              ["2", "10", "100", "1000"]
              (zip ts $ P.transpose [snd r2, snd r10, snd r100, snd r1000])
          ]
      output "mmult" $
        Native
          [ formatRunsMedian
              ["10"]
              [ ("NumHask.Array.Fixed", [fst rma10]),
                ("Numeric.LinearAlgebra.R", [fst rmh10]),
                ("Statistics.Matrix.Fast", [fst rmd10])
              ]
          ]
-}

