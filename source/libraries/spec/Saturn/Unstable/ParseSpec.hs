module Saturn.Unstable.ParseSpec where

import qualified Data.Either as Either
import qualified Saturn.Unstable.Parse as Parse
import qualified Saturn.Unstable.Type.ScheduleSpec as ScheduleSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Parse" $ do
  Hspec.describe "fromString" $ do
    Hspec.it "accepts wildcards" $ do
      schedule <- ScheduleSpec.new [] [] [] [] []
      Parse.fromString "* * * * *" `Hspec.shouldBe` Right schedule

    Hspec.it "accepts extra spaces" $ do
      schedule <- ScheduleSpec.new [] [] [] [] []
      Parse.fromString "  *  *  *  *  *  " `Hspec.shouldBe` Right schedule

    Hspec.it "accepts numbers" $ do
      schedule <- ScheduleSpec.new [[4]] [[3]] [[2]] [[1]] [[0]]
      Parse.fromString "4 3 2 1 0" `Hspec.shouldBe` Right schedule

    Hspec.it "accepts ranges" $ do
      schedule <- ScheduleSpec.new [[8, 9]] [[6, 7]] [[4, 5]] [[2, 3]] [[0, 1]]
      Parse.fromString "8-9 6-7 4-5 2-3 0-1" `Hspec.shouldBe` Right schedule

    Hspec.it "accepts choices" $ do
      schedule <- ScheduleSpec.new [[8], [9]] [[6], [7]] [[4], [5]] [[2], [3]] [[0], [1]]
      Parse.fromString "8,9 6,7 4,5 2,3 0,1" `Hspec.shouldBe` Right schedule

    Hspec.describe "minute" $ do
      Hspec.it "accepts a number" $ do
        schedule <- ScheduleSpec.new [[0]] [] [] [] []
        Parse.fromString "0 * * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a range" $ do
        schedule <- ScheduleSpec.new [[0, 1]] [] [] [] []
        Parse.fromString "0-1 * * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a choice" $ do
        schedule <- ScheduleSpec.new [[0], [1]] [] [] [] []
        Parse.fromString "0,1 * * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "rejects two wildcards" $ do
        Parse.fromString "*,* * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a number" $ do
        Parse.fromString "*,0 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a range" $ do
        Parse.fromString "*,0-0 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number" $ do
        Parse.fromString "60 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number as part of a choice" $ do
        Parse.fromString "0,60 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds range" $ do
        Parse.fromString "60-61 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a half out of bounds range" $ do
        Parse.fromString "0-60 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a backwards range" $ do
        Parse.fromString "1-0 * * * *" `Hspec.shouldSatisfy` Either.isLeft

    Hspec.describe "hour" $ do
      Hspec.it "accepts a number" $ do
        schedule <- ScheduleSpec.new [] [[0]] [] [] []
        Parse.fromString "* 0 * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a range" $ do
        schedule <- ScheduleSpec.new [] [[0, 1]] [] [] []
        Parse.fromString "* 0-1 * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a choice" $ do
        schedule <- ScheduleSpec.new [] [[0], [1]] [] [] []
        Parse.fromString "* 0,1 * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "rejects two wildcards" $ do
        Parse.fromString "* *,* * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a number" $ do
        Parse.fromString "* *,0 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a range" $ do
        Parse.fromString "* *,0-0 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number" $ do
        Parse.fromString "* 24 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number as part of a choice" $ do
        Parse.fromString "* 0,24 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds range" $ do
        Parse.fromString "* 24-25 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a half out of bounds range" $ do
        Parse.fromString "* 0-24 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a backwards range" $ do
        Parse.fromString "* 1-0 * * *" `Hspec.shouldSatisfy` Either.isLeft

    Hspec.describe "day" $ do
      Hspec.it "accepts a number" $ do
        schedule <- ScheduleSpec.new [] [] [[1]] [] []
        Parse.fromString "* * 1 * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a range" $ do
        schedule <- ScheduleSpec.new [] [] [[1, 2]] [] []
        Parse.fromString "* * 1-2 * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a choice" $ do
        schedule <- ScheduleSpec.new [] [] [[1], [2]] [] []
        Parse.fromString "* * 1,2 * *" `Hspec.shouldBe` Right schedule

      Hspec.it "rejects two wildcards" $ do
        Parse.fromString "* * *,* * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a number" $ do
        Parse.fromString "* * *,1 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a range" $ do
        Parse.fromString "* * *,1-1 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number" $ do
        Parse.fromString "* * 32 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number as part of a choice" $ do
        Parse.fromString "* * 1,32 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds range" $ do
        Parse.fromString "* * 32-33 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a half out of bounds range" $ do
        Parse.fromString "* * 1-32 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a backwards range" $ do
        Parse.fromString "* * 2-1 * *" `Hspec.shouldSatisfy` Either.isLeft

    Hspec.describe "month" $ do
      Hspec.it "accepts a number" $ do
        schedule <- ScheduleSpec.new [] [] [] [[1]] []
        Parse.fromString "* * * 1 *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a range" $ do
        schedule <- ScheduleSpec.new [] [] [] [[1, 2]] []
        Parse.fromString "* * * 1-2 *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a choice" $ do
        schedule <- ScheduleSpec.new [] [] [] [[1], [2]] []
        Parse.fromString "* * * 1,2 *" `Hspec.shouldBe` Right schedule

      Hspec.it "rejects two wildcards" $ do
        Parse.fromString "* * * *,* *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a number" $ do
        Parse.fromString "* * * *,1 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a range" $ do
        Parse.fromString "* * * *,1-1 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number" $ do
        Parse.fromString "* * * 13 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number as part of a choice" $ do
        Parse.fromString "* * * 1,13 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds range" $ do
        Parse.fromString "* * * 13-14 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a half out of bounds range" $ do
        Parse.fromString "* * * 1-13 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a backwards range" $ do
        Parse.fromString "* * * 2-1 *" `Hspec.shouldSatisfy` Either.isLeft

    Hspec.describe "weekday" $ do
      Hspec.it "accepts a number" $ do
        schedule <- ScheduleSpec.new [] [] [] [] [[0]]
        Parse.fromString "* * * * 0" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a range" $ do
        schedule <- ScheduleSpec.new [] [] [] [] [[0, 1]]
        Parse.fromString "* * * * 0-1" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a choice" $ do
        schedule <- ScheduleSpec.new [] [] [] [] [[0], [1]]
        Parse.fromString "* * * * 0,1" `Hspec.shouldBe` Right schedule

      Hspec.it "rejects two wildcards" $ do
        Parse.fromString "* * * * *,*" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a number" $ do
        Parse.fromString "* * * * *,0" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a range" $ do
        Parse.fromString "* * * * *,0-0" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number" $ do
        Parse.fromString "* * * * 7" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number as part of a choice" $ do
        Parse.fromString "* * * * 0,7" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds range" $ do
        Parse.fromString "* * * * 7-8" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a half out of bounds range" $ do
        Parse.fromString "* * * * 0-7" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a backwards range" $ do
        Parse.fromString "* * * * 1-0" `Hspec.shouldSatisfy` Either.isLeft
