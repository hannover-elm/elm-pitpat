module PoolTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Pool
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
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot []
                        in
                        case nextAction of
                            Pool.NextShot pool ->
                                pool
                                    |> Pool.currentScore
                                    |> Expect.equal
                                        { player1 = 0
                                        , player2 = 0
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be Pool.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "player 1 hit one ball, score is 1-0"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot [ Pool.cueHitBall (Time.millisToPosix 1) Pool.oneBall ]
                        in
                        case nextAction of
                            Pool.NextShot pool ->
                                pool
                                    |> Pool.currentScore
                                    |> Expect.equal
                                        { player1 = 1
                                        , player2 = 0
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be Pool.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "player 2 hit one ball, score is 0-1"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot [ Pool.cueHitBall (Time.millisToPosix 1) Pool.oneBall ]
                                    |> andKeepShooting [ Pool.cueStruck (Time.millisToPosix 1) ]
                                    |> andKeepShooting [ Pool.cueHitBall (Time.millisToPosix 1) Pool.oneBall ]
                        in
                        case nextAction of
                            Pool.NextShot pool ->
                                pool
                                    |> Pool.currentScore
                                    |> Expect.equal
                                        { player1 = 0
                                        , player2 = 1
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be Pool.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "player 2 hit two balls and wins, score is 0-2"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot [ Pool.cueHitBall (Time.millisToPosix 1) Pool.oneBall ]
                                    |> andKeepShooting [ Pool.cueStruck (Time.millisToPosix 2) ]
                                    |> andKeepShooting [ Pool.cueHitBall (Time.millisToPosix 3) Pool.oneBall ]
                                    |> andKeepShooting [ Pool.cueHitBall (Time.millisToPosix 4) Pool.oneBall ]
                        in
                        case nextAction of
                            Pool.GameOver pool _ ->
                                pool
                                    |> Pool.currentScore
                                    |> Expect.equal
                                        { player1 = 0
                                        , player2 = 2
                                        }

                            other ->
                                Expect.fail <|
                                    "Should be Pool.GameOver, but found this instead:\n"
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
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot []
                        in
                        case nextAction of
                            Pool.NextShot pool ->
                                pool
                                    |> Pool.currentPlayer
                                    |> Expect.equal 0

                            other ->
                                Expect.fail <|
                                    "Should be Pool.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue hits nothing, next players turn"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot
                                        [ Pool.cueStruck (Time.millisToPosix 0)
                                        ]
                        in
                        case nextAction of
                            Pool.NextShot pool ->
                                pool
                                    |> Pool.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be Pool.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue hits nothing, and next player hits nothing, back to first"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot [ Pool.cueStruck (Time.millisToPosix 0) ]
                                    |> andKeepShooting [ Pool.cueStruck (Time.millisToPosix 1) ]
                        in
                        case nextAction of
                            Pool.NextShot pool ->
                                pool
                                    |> Pool.currentPlayer
                                    |> Expect.equal 0

                            other ->
                                Expect.fail <|
                                    "Should be Pool.NextShot, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue into another ball twice, player wins!"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot
                                        [ Pool.cueHitBall (Time.millisToPosix 1) Pool.oneBall
                                        , Pool.cueHitBall (Time.millisToPosix 2) Pool.oneBall
                                        ]
                        in
                        case nextAction of
                            Pool.GameOver pool result ->
                                Expect.all
                                    [ \( pool_, _ ) ->
                                        pool_
                                            |> Pool.currentPlayer
                                            |> Expect.equal 0
                                    , \( _, result_ ) ->
                                        result_.winner
                                            |> Expect.equal 0
                                    ]
                                    ( pool, result )

                            other ->
                                Expect.fail <|
                                    "Should be Pool.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                , test "after player shoots cue into another ball, but then misses on next shot, next player must shoot twice to win"
                    (\_ ->
                        let
                            nextAction =
                                Pool.start
                                    |> Pool.rack (Time.millisToPosix 0)
                                    |> Pool.ballPlacedInKitchen (Time.millisToPosix 0)
                                    |> Pool.playerShot [ Pool.cueHitBall (Time.millisToPosix 1) Pool.oneBall ]
                                    |> andKeepShooting [ Pool.cueStruck (Time.millisToPosix 2) ]
                                    |> andKeepShooting [ Pool.cueHitBall (Time.millisToPosix 3) Pool.twoBall ]
                                    |> andKeepShooting [ Pool.cueHitBall (Time.millisToPosix 5) Pool.threeBall ]
                        in
                        case nextAction of
                            Pool.GameOver pool _ ->
                                pool
                                    |> Pool.currentPlayer
                                    |> Expect.equal 1

                            other ->
                                Expect.fail <|
                                    "Should be Pool.GameOver, but found this instead:\n"
                                        ++ Debug.toString other
                    )
                ]
            ]
        ]


andKeepShooting : List ( Time.Posix, Pool.ShotEvent ) -> Pool.WhatHappened -> Pool.WhatHappened
andKeepShooting shotEvents ruling =
    case ruling of
        Pool.NextShot pool ->
            Pool.playerShot shotEvents pool

        _ ->
            ruling