module EightBallTests exposing (..)

import EightBall
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Pool"
        [ describe "view"
            [ describe "currentScore"
                [ test "no events sent, score is 0-0"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot []
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentScore
                                    |> Expect.equal
                                        { player1 = 0
                                        , player2 = 0
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "a ball is pocketed and target balls are decided, score is 1-0"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentScore
                                    |> Expect.equal
                                        { player1 = 1
                                        , player2 = 0
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "several balls are pocketed, score is 3-5"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 3) EightBall.fiveBall
                                        ]
                                    |> andKeepShooting []
                                    -- Player 2 starts shooting
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 5) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.tenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.elevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.twelveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.thirteenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentScore
                                    |> Expect.equal
                                        { player1 = 3
                                        , player2 = 5
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                ]
            ]
        , describe "update"
            [ describe "playerShot"
                [ test "after player shoots cue hits nothing, next players turn"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot []
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue hits ball, but doesn't pocket it, next players turn"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 0) EightBall.twoBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue hits nothing, and next player hits nothing, back to first"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot []
                                    |> andKeepShooting []
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 0

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets a ball, that group becomes the player's target"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentTarget >> Expect.equal EightBall.Solids
                                        , EightBall.currentPlayer >> Expect.equal 0
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets two stripes, then that player's target will become stripes"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fifteenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.tenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fifteenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentTarget
                                    |> Expect.equal EightBall.Stripes

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets a solid and two strips, table is still open"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.tenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> EightBall.currentTarget
                                    |> Expect.equal EightBall.OpenTable

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets a ball but also scratches, table is still open"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.scratch (Time.millisToPosix 1)
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentTarget >> Expect.equal EightBall.OpenTable
                                        , EightBall.currentPlayer >> Expect.equal 1
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets all of their target balls, then on the next shot targeting the 8-ball, it's still their shot!"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.sixBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.sevenBall
                                        ]
                        in
                        case nextAction of
                            EightBall.NextShot pool ->
                                pool
                                    |> Expect.all
                                        [ EightBall.currentPlayer >> Expect.equal 0
                                        ]

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue and pockets all of their balls, then hits 8-ball without scratching, they win!"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.sixBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.sevenBall
                                        ]
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.eightBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1) EightBall.eightBall
                                        ]
                        in
                        case nextAction of
                            EightBall.GameOver pool { winner } ->
                                Expect.equal winner 0

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots back and forth, then one finishes all of their target, then hits 8-ball without scratching, they win!"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    -- Player 2
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 6) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 7) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 8) EightBall.sixBall
                                        ]
                                    -- Player 2 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 10) EightBall.oneBall
                                        ]
                                    -- Player 1 shoots again
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 111) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 115) EightBall.nineBall
                                        ]
                                    -- Player 1 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1200) EightBall.tenBall
                                        ]
                                    -- Player 2 finishes off target group
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1205) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.oneBall
                                        ]
                                    -- Player 2 makes 8-ball
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1600) EightBall.eightBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1800) EightBall.eightBall
                                        ]
                        in
                        case nextAction of
                            EightBall.GameOver pool { winner } ->
                                Expect.equal winner 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player finishes all of their target, then hits 8-ball but scratches, they lose :("
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    -- Player 2
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.twoBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 5) EightBall.threeBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 6) EightBall.fourBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 7) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 8) EightBall.sixBall
                                        ]
                                    -- Player 2 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 10) EightBall.oneBall
                                        ]
                                    -- Player 1 shoots again
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 111) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 115) EightBall.nineBall
                                        ]
                                    -- Player 1 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1200) EightBall.tenBall
                                        ]
                                    -- Player 2 finishes off target group
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1205) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.sevenBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1500) EightBall.oneBall
                                        ]
                                    -- Player 2 makes 8-ball, but then scratches
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1600) EightBall.eightBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 1800) EightBall.eightBall
                                        , EightBall.scratch (Time.millisToPosix 1700)
                                        ]
                        in
                        case nextAction of
                            EightBall.GameOver pool { winner } ->
                                Expect.equal winner 0

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots the 8-ball early (before they have finished all of their target balls), they lose :("
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        ]
                                    -- Player 2
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.fiveBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 2) EightBall.twoBall
                                        ]
                                    -- Player 2 misses
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 10) EightBall.oneBall
                                        ]
                                    -- Player 1 shoots again and accidentally hits the 8-ball in early.
                                    |> andKeepShooting
                                        [ EightBall.cueHitBall (Time.millisToPosix 111) EightBall.nineBall
                                        , EightBall.ballFellInPocket (Time.millisToPosix 115) EightBall.eightBall
                                        ]
                        in
                        case nextAction of
                            EightBall.GameOver pool { winner } ->
                                Expect.equal winner 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                ]
            , describe "ballPlacedInHand"
                [ test "when player scratches, the other player must place ball in hand before continuing to shoot"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.scratch (Time.millisToPosix 789)
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "when player scratches and next player places ball in hand, they may continue to shoot"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueHitBall (Time.millisToPosix 1) EightBall.oneBall
                                        , EightBall.scratch (Time.millisToPosix 789)
                                        ]
                        in
                        case nextAction of
                            EightBall.PlayersFault pool ->
                                pool
                                    |> EightBall.ballPlacedInHand (Time.millisToPosix 800)
                                    |> EightBall.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.PlayersFault, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                ]
            ]
        ]


andKeepShooting : List ( Time.Posix, EightBall.ShotEvent ) -> EightBall.WhatHappened -> EightBall.WhatHappened
andKeepShooting shotEvents ruling =
    case ruling of
        EightBall.NextShot pool ->
            EightBall.playerShot shotEvents pool

        _ ->
            ruling
