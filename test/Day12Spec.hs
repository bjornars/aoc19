{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day12Spec
  ( spec,
  )
where

import Day12
import Import
import qualified Parser as P
import Test.Hspec

example1 :: Text
example1 = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>\n"

parse :: P.ReadP a -> Text -> Maybe a
parse p = P.parse (p <* P.eof)

spec :: Spec
spec = do
  describe "day12" $ do
    it "parser" $ do
      parse moon "<x=1, y=1, z=1>" `shouldBe` Just (Moon {position = Vec3 1 1 1, velocity = zero3})
      parse parser example1
        `shouldBe` Just
          ( Jupiter
              [ Moon {position = Vec3 (-1) 0 2, velocity = zero3},
                Moon {position = Vec3 2 (-10) (-7), velocity = zero3},
                Moon {position = Vec3 4 (-8) 8, velocity = zero3},
                Moon {position = Vec3 3 5 (-1), velocity = zero3}
              ]
          )
    it "step" $ do
      let j = parse parser example1
      step <$> j
        `shouldBe` Just
          ( Jupiter
              [ Moon
                  { velocity = Vec3 {x = 3, y = -1, z = -1},
                    position = Vec3 {x = 2, y = -1, z = 1}
                  },
                Moon
                  { velocity = Vec3 {x = 1, y = 3, z = 3},
                    position = Vec3 {x = 3, y = -7, z = -4}
                  },
                Moon
                  { velocity = Vec3 {x = -3, y = 1, z = -3},
                    position = Vec3 {x = 1, y = -7, z = 5}
                  },
                Moon
                  { velocity = Vec3 {x = -1, y = -3, z = 1},
                    position = Vec3 {x = 2, y = 2, z = 0}
                  }
              ]
          )
    it "part1" $ do
      res <- part1
      res `shouldBe` 6849

    it "example part2" $ do
      let j = parse parser example1
      findCycle <$> j `shouldBe` Just 2772

    it "part2" $ do
      res <- part2
      res `shouldSatisfy` (<1426561979982095)
      res `shouldBe` 356658899375688
