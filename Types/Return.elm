module Types.Return exposing
    ( noCommand
    , withCommand
    )


noCommand : model -> ( model, Cmd msg )
noCommand model =
    ( model, Cmd.none )


withCommand : Cmd msg -> model -> ( model, Cmd msg )
withCommand command model =
    ( model, command )
