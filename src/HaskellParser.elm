module HaskellParser exposing (func, Func(..))

import Parser exposing (Parser, (|.), (|=), succeed, symbol, keyword, ignore, zeroOrMore)
import Char
import Parser.LanguageKit exposing (variable, sequence, Trailing(..))
import Set


type alias Name =
    String


type alias Body =
    String


type alias Args =
    List String


type Func
    = Func Name Args Body


func : Parser Func
func =
    succeed Func
        |= lowVar
        |. spaces
        |= args
        |. keyword "="
        |. spaces
        |= lowVar


args : Parser (List String)
args =
    sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = spaces
        , item = lowVar
        , trailing = Forbidden
        }


lowVar : Parser String
lowVar =
    variable Char.isLower isVarChar keywords


capVar : Parser String
capVar =
    variable Char.isUpper isVarChar keywords


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'


keywords : Set.Set String
keywords =
    Set.fromList [ "let", "in", "case", "of" ]


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')
