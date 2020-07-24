module Types.Json exposing
    ( Leaf(..)
    , Node(..)
    , Tree
    , addLocal
    , changeToList
    , changeToObject
    , example
    , view
    )

import Html exposing (Html)
import Html.Attributes as Attributes
import Types.Tree as Tree


type alias Tree =
    Tree.Tree Node Leaf


type Leaf
    = StringLiteral
    | BooleanLiteral
    | NumberLiteral


type Node
    = ObjectTree
    | FieldTree
    | ListTree


example : Tree
example =
    Tree.Node ObjectTree
        [ Tree.Node FieldTree [ Tree.Leaf StringLiteral "name", Tree.Leaf StringLiteral "allan" ]
        , Tree.Node FieldTree [ Tree.Leaf StringLiteral "age", Tree.Leaf NumberLiteral "39" ]
        ]
        |> Tree.Cursor


view : Tree -> Html msg
view tree =
    let
        simpleList listClass elementClass separator elements =
            List.map List.singleton elements
                |> List.map (Html.li [ Attributes.class elementClass ])
                |> List.intersperse (punctuation separator)
                |> Html.ul [ Attributes.class listClass ]

        punctuation s =
            Html.span [ Attributes.class "punctuation" ] [ Html.text s ]

        surround left right element =
            Html.div
                [ Attributes.class "surround" ]
                [ Html.span [ Attributes.class "surround" ] [ Html.text left ]
                , element
                , Html.span [ Attributes.class "surround" ] [ Html.text right ]
                ]
    in
    case tree of
        Tree.Cursor subTree ->
            Html.div
                [ Attributes.class "cursor" ]
                [ view subTree ]

        Tree.Leaf StringLiteral s ->
            Html.div
                [ Attributes.class "string-literal" ]
                [ Html.text "\""
                , Html.text s
                , Html.text "\""
                ]

        Tree.Leaf BooleanLiteral s ->
            Html.div
                [ Attributes.class "boolean-literal" ]
                [ s |> Html.text ]

        Tree.Leaf NumberLiteral s ->
            Html.div
                [ Attributes.class "number-literal" ]
                [ Html.text s ]

        Tree.Node kind fields ->
            let
                subFields =
                    List.map view fields
            in
            case kind of
                ObjectTree ->
                    subFields
                        |> simpleList "object" "object-field" ","
                        |> surround "{" "}"

                FieldTree ->
                    subFields
                        |> simpleList "field" "field-part" ":"

                ListTree ->
                    subFields
                        |> simpleList "list" "list-element" ","
                        |> surround "[" "]"


addLocal : Tree -> Tree
addLocal tree =
    case tree of
        Tree.Cursor subTree ->
            addLocal subTree
                |> Tree.Cursor

        Tree.Leaf _ _ ->
            tree

        Tree.Node kind subTrees ->
            let
                new =
                    case kind of
                        ObjectTree ->
                            Tree.Node FieldTree [ Tree.Leaf StringLiteral "", Tree.Leaf StringLiteral "" ]

                        FieldTree ->
                            -- This shouldn't really happen you should always have exactly two
                            Tree.Leaf StringLiteral ""

                        ListTree ->
                            Tree.Leaf StringLiteral ""

                add l =
                    case l of
                        [] ->
                            []

                        (Tree.Cursor t) :: rest ->
                            Tree.Cursor t :: new :: add rest

                        first :: rest ->
                            first :: add rest
            in
            subTrees
                |> List.map addLocal
                |> add
                |> Tree.Node kind


changeToList : Tree -> Tree
changeToList tree =
    case tree of
        Tree.Cursor subTree ->
            case subTree of
                Tree.Cursor _ ->
                    -- we can merge the two cursors
                    changeToList subTree

                Tree.Node ListTree subTrees ->
                    -- We're already a list tree so nothing to change here, but recurse in case there are
                    -- further cursors below
                    List.map changeToList subTrees
                        |> Tree.Node ListTree
                        |> Tree.Cursor

                Tree.Node FieldTree (left :: right) ->
                    -- We cannot change a field tree to be a list, and similar to below we cannot change
                    -- the left side of a field tree, but we might have cursors in the value part.
                    (left :: List.map changeToList right)
                        |> Tree.Node FieldTree
                        |> Tree.Cursor

                Tree.Node FieldTree [] ->
                    -- If you somehow manage to produce this, then the user is probably trying to get out of
                    -- this error state so let's just do what seems reasonable here:
                    Tree.Node FieldTree [ Tree.Leaf StringLiteral "", Tree.Leaf StringLiteral "" ]
                        |> Tree.Cursor

                Tree.Leaf leaf value ->
                    -- So in this case the leaf becomes the value of a singleton list
                    [ Tree.Leaf leaf value ]
                        |> Tree.Node ListTree
                        |> Tree.Cursor

                Tree.Node ObjectTree subTrees ->
                    let
                        convertField element =
                            case element of
                                Tree.Node FieldTree [ _, value ] ->
                                    value

                                Tree.Cursor (Tree.Node FieldTree [ _, value ]) ->
                                    value

                                _ ->
                                    element
                    in
                    List.map changeToList subTrees
                        |> List.map convertField
                        |> Tree.Node ListTree
                        |> Tree.Cursor

        Tree.Leaf _ _ ->
            tree

        Tree.Node FieldTree (left :: right) ->
            -- We cannot change the left part of a field tree into an object
            (left :: List.map changeToList right)
                |> Tree.Node FieldTree

        Tree.Node kind subTrees ->
            subTrees
                |> List.map changeToList
                |> Tree.Node kind


changeToObject : Tree -> Tree
changeToObject tree =
    case tree of
        Tree.Cursor subTree ->
            case subTree of
                Tree.Cursor _ ->
                    -- we can merge the two cursors
                    changeToObject subTree

                Tree.Node ObjectTree subTrees ->
                    -- We're already an object tree so nothing to change here, but recurse in case there are
                    -- further cursors below
                    List.map changeToObject subTrees
                        |> Tree.Node ObjectTree
                        |> Tree.Cursor

                Tree.Node FieldTree (left :: right) ->
                    -- We cannot change a field tree to be an object, and similar to below we cannot change
                    -- the left side of a field tree, but we might have cursors in the value part.
                    (left :: List.map changeToObject right)
                        |> Tree.Node FieldTree
                        |> Tree.Cursor

                Tree.Node FieldTree [] ->
                    -- If you somehow manage to produce this, then the user is probably trying to get out of
                    -- this error state so let's just do what seems reasonable here:
                    Tree.Node FieldTree [ Tree.Leaf StringLiteral "", Tree.Leaf StringLiteral "" ]
                        |> Tree.Cursor

                Tree.Leaf leaf value ->
                    -- So in this case the leaf becomes the value of a single field of an ojbect
                    [ Tree.Node FieldTree [ Tree.Leaf StringLiteral "", Tree.Leaf leaf value ] ]
                        |> Tree.Node ObjectTree
                        |> Tree.Cursor

                Tree.Node ListTree subTrees ->
                    let
                        makeField element =
                            Tree.Node FieldTree [ Tree.Leaf StringLiteral "", element ]
                    in
                    List.map changeToObject subTrees
                        |> List.map makeField
                        |> Tree.Node ObjectTree
                        |> Tree.Cursor

        Tree.Leaf _ _ ->
            tree

        Tree.Node FieldTree (left :: right) ->
            -- We cannot change the left part of a field tree into an object
            (left :: List.map changeToObject right)
                |> Tree.Node FieldTree

        Tree.Node kind subTrees ->
            subTrees
                |> List.map changeToObject
                |> Tree.Node kind
