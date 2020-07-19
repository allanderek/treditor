module Types.Tree exposing
    ( Tree(..)
    , goDown
    , goIn
    , goOut
    , goUp
    , insert
    )

import Types.Key as Key exposing (Key)


type Tree node leaf
    = Cursor (Tree node leaf)
    | Leaf leaf String
    | Node node (List (Tree node leaf))


insert : Key -> Tree node leaf -> Tree node leaf
insert key tree =
    case tree of
        Cursor (Leaf leaf s) ->
            let
                newS =
                    case key of
                        Key.Character c ->
                            String.fromChar c
                                |> String.append s

                        Key.Control _ ->
                            s
            in
            Leaf leaf newS
                |> Cursor

        Leaf _ _ ->
            tree

        Node node subTrees ->
            List.map (insert key) subTrees
                |> Node node

        Cursor subTree ->
            insert key subTree
                |> Cursor


goUp : Tree node leaf -> Tree node leaf
goUp tree =
    case tree of
        Cursor subTree ->
            goUp subTree
                |> Cursor

        Leaf _ _ ->
            tree

        Node node subTrees ->
            let
                moveUp l =
                    case l of
                        [] ->
                            []

                        (Cursor t) :: others ->
                            Cursor t :: moveUp others

                        first :: (Cursor t) :: others ->
                            Cursor first :: t :: moveUp others

                        first :: others ->
                            first :: moveUp others
            in
            subTrees
                |> List.map goUp
                |> moveUp
                |> Node node


goDown : Tree node leaf -> Tree node leaf
goDown tree =
    case tree of
        Cursor subTree ->
            goDown subTree
                |> Cursor

        Leaf _ _ ->
            tree

        Node node subTrees ->
            let
                moveDown l =
                    case l of
                        [] ->
                            []

                        (Cursor t) :: others ->
                            -- Note we recurs first in case there are multiple cursors successively
                            case moveDown others of
                                [] ->
                                    [ Cursor t ]

                                (Cursor first) :: rest ->
                                    -- If we have recursed and the first is *still* a cursor that means it's
                                    -- cursors all the way from here to the bottom, so we're not moving
                                    Cursor t :: Cursor first :: rest

                                first :: rest ->
                                    t :: Cursor first :: rest

                        first :: rest ->
                            first :: moveDown rest
            in
            subTrees
                |> List.map goDown
                |> moveDown
                |> Node node


goIn : Tree node leaf -> Tree node leaf
goIn tree =
    case tree of
        Cursor (Node kind (firstChild :: others)) ->
            Node kind (Cursor firstChild :: others)

        Cursor _ ->
            tree

        Leaf _ _ ->
            tree

        Node kind subTrees ->
            List.map goIn subTrees
                |> Node kind


isCursor : Tree node leaf -> Bool
isCursor tree =
    case tree of
        Cursor _ ->
            True

        Node _ _ ->
            False

        Leaf _ _ ->
            False


removeCursor : Tree node leaf -> Tree node leaf
removeCursor tree =
    -- Note, this only removes it from the top level if it is there, it does not recursively remove any
    -- cursors contained within the tree.
    case tree of
        Cursor subTree ->
            subTree

        Node _ _ ->
            tree

        Leaf _ _ ->
            tree


goOut : Tree node leaf -> Tree node leaf
goOut tree =
    case tree of
        Node kind subTrees ->
            let
                newTree =
                    List.map removeCursor subTrees
                        |> List.map goOut
                        |> Node kind
            in
            -- Note: obviously we are checking the *old* subTrees, the new subTrees
            -- may have cursor that has already been moved out.
            case List.any isCursor subTrees of
                True ->
                    Cursor newTree

                False ->
                    newTree

        Cursor _ ->
            -- Why not remove it? We remove it explicitly in the case above, here it could be the outermost
            -- tree so we don't wish to remove it from there.
            tree

        Leaf _ _ ->
            tree
