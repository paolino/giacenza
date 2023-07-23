module Configuration where

import Protolude
import Turtle
    ( Parser
    , arg
    , argInt
    , argPath
    , argText
    , subcommand
    )
import Types (NumberFormatKnown, parseNumberFormat)

parseOptions
    :: Parser
        ( Either
            (Text, Int, Text)
            (FilePath, Text, Text, NumberFormatKnown)
        )
parseOptions = fmap Left parserServe <|> fmap Right parser

parser :: Parser (FilePath, Text, Text, NumberFormatKnown)
parser =
    (,,,)
        <$> argPath "csv" "The csv file"
        <*> argText "date-name" "The date field name"
        <*> argText "amount-name" "The amount field name"
        <*> arg
            (either (const Nothing) pure . parseNumberFormat)
            "number-format"
            "The number format, european or american"

parserServe :: Parser (Text, Int, Text)
parserServe =
    subcommand "serve" "Start the web server"
        $ (,,)
            <$> argText "host" "The host to bind to"
            <*> argInt "port" "The port to bind to"
            <*> argText "prefix" "The prefix to add to the http calls"
