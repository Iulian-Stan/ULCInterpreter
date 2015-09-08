{-
A parsing module, adapted from
"Haskell: The Craft of Functional Programming", 2nd ed, S. Thompson.
-}

module Parser where

{-
"Data.Char" is needed for "isLetter" and "isDigit".
-}

import Data.Char

{-
The type of parsing functions.
They take a list of objects of type "a" e.g., characters, and return a list
of successful parses. Each parse is represented as a pair between the extracted
object, of type "b" e.g., integer, and the list of objects designating
the remaining input.

Examples, where "bracket" and "number" are parsers:
* bracket	"(xyz)"	= [('(', "xyz)")]			- one successful parse
* number	23		= [(2, "3"), (23, "")]		- two successful parses
* bracket	234		= []						- no successful parse 
-}

type Parser a b = [a] -> [(b, [a])]

{-
"none" never parses anything.
-}

none :: Parser a b
none input = []

{-
"success" parses an explicitly given value, without consuming any input.

Examples:
* success 23 "abc" = [(23, "abc")]
-}

success :: b -> Parser a b
success value input = [(value, input)]

{-
"token" recognizes the token "t".

Examples:
* token 'a' "abc" = [('a', "bc")]
* token 'a' "bbc" = []
-}

token :: Eq a => a -> Parser a a
token _ [] = []
token token (first : rest)
	| token == first	= [(token, rest)]
	| otherwise			= []

{-
"spot" recognizes a token satisfying a given property.

"token" can be defined starting from "spot" as below (any of the two options):
* token t	= spot (== t)
* token		= spot . (==)

Examples:
* spot isLetter "abc" = [('a', "bc")]
* spot isLetter "123" = []
-}

spot :: (a -> Bool) -> Parser a a
spot property [] = []
spot property (first : rest)
	| property first	= [(first, rest)]
	| otherwise			= []
	
{-
"alt" represents an alternative between two parsers.
The parsing succeeds if any of the two parsers is able to parse.
It simply concatenates the lists of parses obtained by the two parsers.

By employing the infix notation, `alt`, as in the examples below,
any number of parsers can be legibly chained.

Examples:
* ((token 'a') `alt` (token 'b') `alt` (spot isDigit)) "abc" = [('a', "bc")]
* ((token 'a') `alt` (token 'b') `alt` (spot isDigit)) "123" = [('1', "23")]
-}

alt :: Parser a b -> Parser a b -> Parser a b
alt parser1 parser2 input = parser1 input ++ parser2 input

{-
">*>" represents the concatenation of two parsers.
The parsing succeeds if the first parser is able to parse,
starting at the beginning of the input, and the second one
is able to parse, starting from the remainder of the input.

It is defined as a right-associative infix operator.

Examples:
* ((token 'a') >*> (token 'b') >*> (spot isDigit)) "ab2p"
	= [(('a', ('b', '2')), "p")]
* ((token 'a') >*> (token 'b') >*> (spot isDigit)) "abp" = []
-}

infixr 5 >*>
(>*>) :: Parser a b -> Parser a c -> Parser a (b, c)
(>*>) parser1 parser2 input = [ ((result1, result2), remainder2) |
									(result1, remainder1) <- parser1 input,
									(result2, remainder2) <- parser2 remainder1 ]
									
{-
"transform" changes the result of another parser.

Examples:
* ((spot isLetter) `transform` toUpper) "abc" = [('A', "bc")]
-}

transform :: Parser a b -> (b -> c) -> Parser a c
transform parser change input = [ (change result, remainder) |
									(result, remainder) <- parser input ]
									
{-
"list" parses lists of objects, given a parser for a single object.

Examples:
* (list (spot isLetter)) "abc123"
	= [("", "abc123"), ("a", "bc123"), ("ab", "c123"), ("abc", "123")]
-}

list :: Parser a b -> Parser a [b]
list parser = (success []) `alt` ((parser >*> list parser) `transform` (uncurry (:)))

{-
"neList" parses non-empty lists of objects, given a parser for a single object.

Examples:
* (neList (spot isLetter)) "abc123"
	= [("a", "bc123"), ("ab", "c123"), ("abc", "123")]
-}

neList :: Parser a b -> Parser a [b]
neList parser = (parser >*> list parser) `transform` (uncurry (:))

{-
"maxList" parses the longest, possibly empty, list of objects,
given a parser for a single object.

Examples:
* (maxList (spot isLetter)) "abc123"	= [("abc", "123")]
* (maxList (spot isLetter)) "123"		= [("", "123")]
-}

maxList :: Parser a b -> Parser a [b]
maxList parser input = [last (list parser input)]

{-
"maxNeList" parses the longest non-empty list of objects,
given a parser for a single object.

Examples:
* (maxNeList (spot isLetter)) "abc123"	= [("abc", "123")]
* (maxNeList (spot isLetter)) "123"		= []
-}

maxNeList :: Parser a b -> Parser a [b]
maxNeList parser = (parser >*> maxList parser) `transform` (uncurry (:))

{-
"spotWhile0" parses the longest, possibly empty, list of objects,
given a property that needs to be satisfied by the objects.

Examples:
* (spotWhile0 isLetter) "abc123"	= [("abc", "123")]
* (spotWhile0 isLetter) "123"		= [("", "123")]
-}

spotWhile0 :: (a -> Bool) -> Parser a [a]
spotWhile0 property = maxList (spot property)

{-
"spotWhile1" parses the longest non-empty list of objects,
given a property that needs to be satisfied by the objects.

Examples:
* (spotWhile1 isLetter) "abc123"	= [("abc", "123")]
* (spotWhile1 isLetter) "123"		= []
-}

spotWhile1 :: (a -> Bool) -> Parser a [a]
spotWhile1 property = maxNeList (spot property)

{-
"parse" tries to parse the entire input, using a given parser.

The "where" section only looks at entries for which the entire input
has been consumed.

Examples:
* parse (spotWhile1 isLetter) "abc" = "abc"
* parse (spotWhile1 isLetter) "abc1" = <error>
-}

parse :: Parser a b -> [a] -> b
parse parser input =
	case results of
		[]	-> error "Syntax error"
		_	-> head results
	where
		results = [ result | (result, []) <- parser input ]