module Game exposing (Model, Msg, initial, subscriptions, update, view)

import Acceleration
import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Bodies exposing (Data, Id(..))
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cylinder3d
import Dict exposing (Dict)
import Direction3d
import Duration exposing (Duration, seconds)
import Force
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode
import Length exposing (Length, Meters)
import Physics.Body as Body
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World as World exposing (World)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d
import Scene3d
import Scene3d.Light
import Scene3d.Material as Material
import SketchPlane3d
import Speed
import Vector2d
import Vector3d
import Viewpoint3d


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { world : World Data
    , dimensions : ( Quantity Float Pixels, Quantity Float Pixels )
    , distance : Length
    , azimuth : Angle
    , elevation : Angle
    , mouseAction : MouseAction
    , state : State
    }


type MouseAction
    = Orbiting
        { from : Point2d Pixels ScreenCoordinates
        , to : Point2d Pixels ScreenCoordinates
        }
    | HoveringCue
        { point : Point2d Pixels ScreenCoordinates
        , raycastResult : World.RaycastResult Data
        }
    | Aiming
        { from : Point2d Pixels ScreenCoordinates
        , to : Point2d Pixels ScreenCoordinates
        , raycastResult : World.RaycastResult Data
        , duration : Duration
        }
    | Still


type State
    = Playing
    | Simulating


type Msg
    = Tick
    | Resize Int Int
    | MouseWheel Float
    | MouseDown (Point2d Pixels ScreenCoordinates)
    | MouseUp
    | MouseMove (Point2d Pixels ScreenCoordinates)


initial : Dict Int (Material.Texture Color) -> Material.Texture Float -> ( Float, Float ) -> Model
initial ballTextures roughnessTexture ( width, height ) =
    { world =
        Bodies.balls roughnessTexture ballTextures
            |> List.foldl World.add initialWorld
    , dimensions = ( Pixels.float width, Pixels.float height )
    , distance = Length.meters 4
    , azimuth = Angle.degrees -25
    , elevation = Angle.degrees 30
    , mouseAction = Still
    , state = Playing
    }


initialWorld : World Data
initialWorld =
    World.empty
        |> World.withGravity
            (Acceleration.metersPerSecondSquared 9.80665)
            Direction3d.negativeZ
        |> World.add Bodies.tableSurface
        |> World.add Bodies.tableWalls
        |> World.add Bodies.floor
        |> World.add Bodies.cueBall


camera : Length -> Angle -> Angle -> MouseAction -> World Data -> Camera3d Meters WorldCoordinates
camera distance azimuth elevation mouseAction world =
    let
        { x, y } =
            case mouseAction of
                Orbiting { from, to } ->
                    Vector2d.toPixels (Vector2d.from from to)

                _ ->
                    Vector2d.toPixels Vector2d.zero

        newAzimuth =
            azimuth |> Quantity.minus (Angle.degrees x)

        newElevation =
            elevation
                |> Quantity.plus (Angle.degrees y)
                |> Quantity.clamp
                    (Angle.degrees 6)
                    (Angle.degrees 90)

        focalPoint =
            Point3d.interpolateFrom
                (cuePosition world)
                Point3d.origin
                ((Length.inMeters distance - 0.5) / (5 - 0.5))
    in
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = focalPoint
                , groundPlane = SketchPlane3d.xy
                , azimuth = newAzimuth
                , elevation = newElevation
                , distance = distance
                }
        , verticalFieldOfView = Angle.degrees 24
        }


cuePosition : World Data -> Point3d Meters WorldCoordinates
cuePosition world =
    World.bodies world
        |> List.filter (\b -> (Body.data b).id == CueBall)
        |> List.head
        |> Maybe.map (\b -> Point3d.placeIn (Body.frame b) (Body.centerOfMass b))
        |> Maybe.withDefault Point3d.origin


ray : Model -> Point2d Pixels ScreenCoordinates -> Axis3d Meters WorldCoordinates
ray { dimensions, distance, azimuth, elevation, mouseAction, world } =
    Camera3d.ray
        (camera distance azimuth elevation mouseAction world)
        (Rectangle2d.with
            { x1 = pixels 0
            , y1 = Tuple.second dimensions
            , x2 = Tuple.first dimensions
            , y2 = pixels 0
            }
        )


view : Model -> Html Msg
view { world, dimensions, distance, azimuth, elevation, mouseAction } =
    let
        dimensionsInt =
            Tuple.mapBoth Quantity.round Quantity.round dimensions

        sunlight =
            Scene3d.Light.directional (Scene3d.Light.castsShadows True)
                { direction = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
                , intensity = Illuminance.lux 10000
                , chromaticity = Scene3d.Light.daylight
                }

        environmentalLighting =
            Scene3d.Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Scene3d.Light.daylight
                , intensityAbove = Illuminance.lux 3000
                , intensityBelow = Illuminance.lux 0
                }

        entities =
            World.bodies world
                |> List.map
                    (\body ->
                        Scene3d.placeIn
                            (Body.frame body)
                            (Body.data body).entity
                    )

        entitiesWithCue =
            case mouseAction of
                Aiming aiming ->
                    cueEntity aiming :: entities

                HoveringCue { point, raycastResult } ->
                    cueEntity
                        { duration = Quantity.zero
                        , from = point
                        , to = point
                        , raycastResult = raycastResult
                        }
                        :: entities

                _ ->
                    entities
    in
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Events.preventDefaultOn "wheel"
            (Json.Decode.map
                (\deltaY -> ( MouseWheel deltaY, True ))
                (Json.Decode.field "deltaY" Json.Decode.float)
            )
        ]
        [ Scene3d.custom
            { dimensions = dimensionsInt
            , antialiasing = Scene3d.noAntialiasing
            , camera = camera distance azimuth elevation mouseAction world
            , entities = entitiesWithCue
            , lights = Scene3d.twoLights environmentalLighting sunlight
            , exposure = Scene3d.exposureValue 13
            , whiteBalance = Scene3d.Light.daylight
            , clipDepth = Length.meters 0.1
            , background = Scene3d.backgroundColor Color.black
            , toneMapping = Scene3d.noToneMapping
            }
        ]


axisFromMouseAction :
    { from : Point2d Pixels ScreenCoordinates
    , to : Point2d Pixels ScreenCoordinates
    , raycastResult : World.RaycastResult Data
    , duration : Duration
    }
    -> Axis3d Meters WorldCoordinates
axisFromMouseAction { from, to, raycastResult } =
    let
        { x, y } =
            Vector2d.toPixels (Vector2d.from from to)

        frame =
            Body.frame raycastResult.body

        normal =
            Direction3d.placeIn frame raycastResult.normal

        point =
            Point3d.placeIn frame raycastResult.point

        azimuth =
            Direction3d.azimuthIn SketchPlane3d.xy normal

        elevation =
            Direction3d.elevationFrom SketchPlane3d.xy normal

        newAzimuth =
            azimuth |> Quantity.plus (Angle.degrees x)

        newElevation =
            elevation |> Quantity.minus (Angle.degrees y)
    in
    Axis3d.through
        point
        (Direction3d.fromAzimuthInAndElevationFrom SketchPlane3d.xy newAzimuth newElevation)


cueEntity :
    { from : Point2d Pixels ScreenCoordinates
    , to : Point2d Pixels ScreenCoordinates
    , raycastResult : World.RaycastResult Data
    , duration : Duration
    }
    -> Scene3d.Entity WorldCoordinates
cueEntity mouseAction =
    let
        axis =
            axisFromMouseAction mouseAction

        maybeCylinder =
            Cylinder3d.from
                (Point3d.along axis (Length.centimeters 2))
                (Point3d.along axis (Length.centimeters (2 + 150)))
                (Length.millimeters 6)
    in
    case maybeCylinder of
        Just cylinder ->
            Scene3d.cylinderWithShadow
                (Material.nonmetal
                    { baseColor = Color.rgb255 255 255 255
                    , roughness = 0.6
                    }
                )
                cylinder

        Nothing ->
            Scene3d.nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize Resize
        , if model.state == Simulating then
            Browser.Events.onAnimationFrame (\_ -> Tick)

          else
            Sub.none
        , Browser.Events.onMouseDown (decodeMouse MouseDown)
        , Browser.Events.onMouseMove (decodeMouse MouseMove)
        , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
        ]


ballsStoppedMoving : World Data -> Bool
ballsStoppedMoving world =
    List.all
        (\body ->
            case (Body.data body).id of
                CueBall ->
                    Body.velocity body
                        |> Vector3d.length
                        |> Quantity.lessThan (Speed.metersPerSecond 0.0005)

                Numbered _ ->
                    Body.velocity body
                        |> Vector3d.length
                        |> Quantity.lessThan (Speed.metersPerSecond 0.0005)

                _ ->
                    True
        )
        (World.bodies world)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            case model.state of
                Simulating ->
                    if ballsStoppedMoving model.world then
                        { model | state = Playing }

                    else
                        { model
                            | world =
                                -- Simulate at shorter interval to prevent tunneling
                                model.world
                                    |> World.simulate (seconds (1 / 120))
                                    |> World.simulate (seconds (1 / 120))
                        }

                Playing ->
                    model

        Resize width height ->
            { model | dimensions = ( Pixels.float (toFloat width), Pixels.float (toFloat height) ) }

        MouseWheel deltaY ->
            { model
                | distance =
                    model.distance
                        |> Quantity.minus (Length.meters (deltaY * 0.01))
                        |> Quantity.clamp (Length.meters 0.5) (Length.meters 5)
            }

        MouseDown mouse ->
            case model.state of
                Playing ->
                    case World.raycast (ray model mouse) model.world of
                        Just raycastResult ->
                            case (Body.data raycastResult.body).id of
                                CueBall ->
                                    { model
                                        | mouseAction =
                                            Aiming
                                                { raycastResult = raycastResult
                                                , from = mouse
                                                , to = mouse
                                                , duration = Duration.seconds 5
                                                }
                                    }

                                _ ->
                                    { model | mouseAction = Orbiting { from = mouse, to = mouse } }

                        Nothing ->
                            { model | mouseAction = Orbiting { from = mouse, to = mouse } }

                Simulating ->
                    { model | mouseAction = Orbiting { from = mouse, to = mouse } }

        MouseMove mouse ->
            case model.mouseAction of
                Orbiting { from } ->
                    { model | mouseAction = Orbiting { from = from, to = mouse } }

                Aiming aiming ->
                    { model | mouseAction = Aiming { aiming | to = mouse } }

                _ ->
                    if model.state == Playing then
                        case World.raycast (ray model mouse) model.world of
                            Just raycastResult ->
                                case (Body.data raycastResult.body).id of
                                    CueBall ->
                                        { model
                                            | mouseAction =
                                                HoveringCue
                                                    { point = mouse
                                                    , raycastResult = raycastResult
                                                    }
                                        }

                                    _ ->
                                        model

                            Nothing ->
                                model

                    else
                        model

        MouseUp ->
            case model.mouseAction of
                Orbiting { from, to } ->
                    let
                        { x, y } =
                            Vector2d.toPixels (Vector2d.from from to)

                        newAzimuth =
                            model.azimuth |> Quantity.minus (Angle.degrees x)

                        newElevation =
                            model.elevation
                                |> Quantity.plus (Angle.degrees y)
                                |> Quantity.clamp
                                    (Angle.degrees 6)
                                    (Angle.degrees 90)
                    in
                    { model
                        | mouseAction = Still
                        , azimuth = newAzimuth
                        , elevation = newElevation
                    }

                Aiming mouseAction ->
                    { model
                        | state = Simulating
                        , world =
                            World.update
                                (\b ->
                                    if (Body.data b).id == CueBall then
                                        let
                                            axis =
                                                axisFromMouseAction mouseAction
                                        in
                                        Body.applyImpulse
                                            (Quantity.times (Duration.milliseconds 16) (Force.newtons 50))
                                            (Axis3d.reverse axis
                                                |> Axis3d.direction
                                            )
                                            (Axis3d.originPoint axis)
                                            b

                                    else
                                        b
                                )
                                model.world
                        , mouseAction = Still
                    }

                _ ->
                    model


decodeMouse : (Point2d Pixels ScreenCoordinates -> Msg) -> Json.Decode.Decoder Msg
decodeMouse msg =
    Json.Decode.map2 (\x y -> msg (Point2d.pixels x y))
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)