module Bodies exposing
    ( Data
    , Id(..)
    , areaBallInHand
    , areaBallInHandEntity
    , areaBehindTheHeadString
    , areaBehindTheHeadStringEntity
    , cueBall
    , floor
    , radius
    , tableSurface
    , tableWalls
    )

import Angle
import Axis3d
import Block3d
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d
import Length exposing (Meters, meters, millimeters)
import Mass
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material exposing (Material)
import Physics.Shape
import Point2d
import Point3d
import Quantity exposing (Quantity(..))
import Rectangle2d
import Rectangle3d exposing (Rectangle3d)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import Sphere3d exposing (Sphere3d)
import Vector3d


type Id
    = Floor
    | CueBall
    | Table
    | Walls


type alias Data =
    { entity : Entity BodyCoordinates
    , id : Id
    }


areaBallInHand : Rectangle3d Meters WorldCoordinates
areaBallInHand =
    Rectangle3d.on
        SketchPlane3d.xy
        (Rectangle2d.from
            (Point2d.meters
                -(sizes.halfWidth - sizes.wallThickness - sizes.ballRadius)
                -(sizes.halfLength - sizes.wallThickness - sizes.ballRadius)
            )
            (Point2d.meters
                (sizes.halfWidth - sizes.wallThickness - sizes.ballRadius)
                (sizes.halfLength - sizes.wallThickness - sizes.ballRadius)
            )
        )
        |> Rectangle3d.translateIn Direction3d.z (Length.millimeters 1)


areaBallInHandEntity : Entity WorldCoordinates
areaBallInHandEntity =
    case Rectangle3d.vertices areaBallInHand of
        [ v1, v2, v3, v4 ] ->
            Scene3d.quad
                (Material.nonmetal
                    { baseColor = Color.rgb255 80 80 0
                    , roughness = 1
                    }
                )
                v1
                v2
                v3
                v4

        _ ->
            Scene3d.nothing


areaBehindTheHeadString : Rectangle3d Meters WorldCoordinates
areaBehindTheHeadString =
    Rectangle3d.on
        SketchPlane3d.xy
        (Rectangle2d.from
            (Point2d.meters
                -(sizes.halfWidth - sizes.wallThickness - sizes.ballRadius)
                -(sizes.halfLength - sizes.wallThickness - sizes.ballRadius)
            )
            (Point2d.meters
                (sizes.halfWidth - sizes.wallThickness - sizes.ballRadius)
                -((sizes.halfLength - sizes.wallThickness) / 2)
            )
        )
        |> Rectangle3d.translateIn Direction3d.z (Length.millimeters 1)


areaBehindTheHeadStringEntity : Entity WorldCoordinates
areaBehindTheHeadStringEntity =
    case Rectangle3d.vertices areaBehindTheHeadString of
        [ v1, v2, v3, v4 ] ->
            Scene3d.quad
                (Material.nonmetal
                    { baseColor = Color.rgb255 80 80 0
                    , roughness = 1
                    }
                )
                v1
                v2
                v3
                v4

        _ ->
            Scene3d.nothing


radius : Float
radius =
    38 / 2


ballSphere : Sphere3d Meters BodyCoordinates
ballSphere =
    Sphere3d.atPoint
        (Point3d.millimeters 0 0 0)
        (millimeters radius)


cueBall : Body Data
cueBall =
    Body.sphere ballSphere
        { id = CueBall
        , entity =
            Scene3d.sphereWithShadow
                (Material.matte (Color.rgb255 255 255 255))
                ballSphere
        }
        |> Body.withMaterial ballMaterial
        |> Body.withDamping ballDamping
        |> Body.withBehavior (Body.dynamic (Mass.grams 170))
        |> Body.translateBy (Vector3d.millimeters 0 0 radius)


ballDamping : { linear : Float, angular : Float }
ballDamping =
    { linear = 0.4, angular = 0.4 }


ballMaterial : Material
ballMaterial =
    Physics.Material.custom
        { friction = 0.06
        , bounciness = 0.6
        }


floor : Body Data
floor =
    Body.plane
        { id = Floor
        , entity =
            Scene3d.quad
                (Material.matte (Color.rgb255 255 255 255))
                (Point3d.meters -sizes.floorHalfSize -sizes.floorHalfSize 0)
                (Point3d.meters sizes.floorHalfSize -sizes.floorHalfSize 0)
                (Point3d.meters sizes.floorHalfSize sizes.floorHalfSize 0)
                (Point3d.meters -sizes.floorHalfSize sizes.floorHalfSize 0)
        }
        |> Body.moveTo (Point3d.meters 0 0 -sizes.height)


sizes :
    { halfWidth : Float
    , halfLength : Float
    , wallThickness : Float
    , wallHeight : Float
    , height : Float
    , thickness : Float
    , floorHalfSize : Float
    , ballRadius : Float
    }
sizes =
    let
        wallThickness =
            0.02
    in
    { halfWidth = 0.4 / 2
    , halfLength = 2.5 / 2
    , wallThickness = wallThickness
    , wallHeight = 0.03
    , height = 0.45 -- distance from the floor until the top of the table
    , thickness = 0.03 -- the height of table top
    , floorHalfSize = 15
    , ballRadius = 57.15 / 2000
    }


tableSurface : Body Data
tableSurface =
    let
        blocks =
            [ Block3d.from
                (Point3d.meters -sizes.halfWidth -sizes.halfLength 0)
                (Point3d.meters sizes.halfWidth sizes.halfLength -sizes.thickness)
            ]
    in
    Body.compound (List.map Physics.Shape.block blocks)
        { id = Table
        , entity =
            List.map
                (Scene3d.blockWithShadow
                    (Material.nonmetal
                        { baseColor = Color.rgb255 10 80 0
                        , roughness = 1
                        }
                    )
                )
                blocks
                |> Scene3d.group
        }
        |> Body.withMaterial
            (Physics.Material.custom
                { friction = 0.8
                , bounciness = 0
                }
            )


tableWalls : Body Data
tableWalls =
    let
        blocks =
            [ Block3d.from
                (Point3d.meters -sizes.halfWidth -sizes.halfLength sizes.wallHeight)
                (Point3d.meters (-sizes.halfWidth + sizes.wallThickness) sizes.halfLength 0)
            , Block3d.from
                (Point3d.meters (sizes.halfWidth - sizes.wallThickness) -sizes.halfLength 0)
                (Point3d.meters sizes.halfWidth sizes.halfLength sizes.wallHeight)
            , Block3d.from
                (Point3d.meters sizes.halfWidth (-sizes.halfLength + sizes.wallThickness) 0)
                (Point3d.meters -sizes.halfWidth -sizes.halfLength sizes.wallHeight)
            , Block3d.from
                (Point3d.meters sizes.halfWidth (sizes.halfLength - sizes.wallThickness) 0)
                (Point3d.meters -sizes.halfWidth sizes.halfLength sizes.wallHeight)
            ]

        shapes =
            blocks
                |> List.map Physics.Shape.block

        entities =
            blocks
                |> List.map
                    (Scene3d.blockWithShadow
                        (Material.nonmetal
                            { baseColor = Color.rgb255 10 80 0
                            , roughness = 0.9
                            }
                        )
                    )
    in
    Body.compound shapes
        { id = Walls
        , entity = Scene3d.group entities
        }
        |> Body.withMaterial
            (Physics.Material.custom
                { friction = 0.3
                , bounciness = 0.6
                }
            )
