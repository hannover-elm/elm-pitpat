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
                ]
            ]
        , describe "update"
            [ describe "playerShot"
                [ test "no events sent, still current player's turn"
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
                                    |> Expect.equal 0

                            other ->
                                Expect.fail <|
                                    "Should be EightBall.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue hits nothing, next players turn"
                    (\_ ->
                        let
                            nextAction =
                                EightBall.start
                                    |> EightBall.rack (Time.millisToPosix 0)
                                    |> EightBall.ballPlacedBehindHeadString (Time.millisToPosix 0)
                                    |> EightBall.playerShot
                                        [ EightBall.cueStruck (Time.millisToPosix 0)
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
                                    |> EightBall.playerShot [ EightBall.cueStruck (Time.millisToPosix 0) ]
                                    |> andKeepShooting [ EightBall.cueStruck (Time.millisToPosix 1) ]
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
                                    |> EightBall.currentTarget
                                    |> Expect.equal EightBall.Solids

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
