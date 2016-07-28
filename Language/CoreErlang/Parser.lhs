\begin{code}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CoreErlang.Parser
-- Copyright   :  (c) Henrique Ferreiro García 2008
--                (c) David Castro Pérez 2008
--                (c) Eric Bailey 2016
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alex Kropivny <alex.kropivny@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- CoreErlang parser.
-- <http://www.it.uu.se/research/group/hipe/cerl/>

-----------------------------------------------------------------------------
module Language.CoreErlang.Parser (
  -- * Lexical definitions
  sign, digit,
  uppercase, lowercase, inputchar, control, space, namechar,
  escape,
  octaldigit, octal, ctrlchar, escapechar,
  -- * Terminals
  integer, float, atom, echar, estring, variableName,
  -- * Non-terminals
  annotatedModule, module_, moduleHeader, exports,  attributes, moduleAttribute,
  funDef, funName, fun, constant, literal, nil, variables,
  ecase, clause, guard, letrec, app, modCall, primOpCall, etry,
  receive, timeout, eseq, ecatch,
  -- * Static semantics
  -- ** Annotations
  annotation, annotated,
  -- ** Module definitions
  parseModule,
  -- * Parse Error (from -- "Text.Parsec")
  ParseError
  ) where

import           Language.CoreErlang.Syntax

import           Prelude                                hiding (exp)

import           Control.Monad                          (liftM)
import           Data.Char                              (chr, isControl)
import           Numeric                                (readOct)

import           Text.Parsec.Char                       (char, lower, noneOf,
                                                         oneOf, satisfy, upper)
import qualified Text.Parsec.Char                       as PChar
import           Text.ParserCombinators.Parsec          (ParseError, Parser,
                                                         choice, count, eof,
                                                         many, many1, option,
                                                         parse, try, (<|>))
\end{code}
% -- import           Text.ParserCombinators.Parsec.Expr
\begin{code}
import           Text.ParserCombinators.Parsec.Language
import           Text.ParserCombinators.Parsec.Token    (TokenParser,
                                                         makeTokenParser)
import qualified Text.ParserCombinators.Parsec.Token    as Token
\end{code}

\section{Grammar}

\subsection{Lexical definitions}

\begin{align}
  sign      &::= + | - \\
  digit     &::= 0 | 1 | ... | 9 \\
  uppercase &::= \texttt{A}\ |\ ...\ |\ \texttt{Z}\
              |\ \texttt{\textbackslash{}u00c0}\ |\ ...\
              |\ \texttt{\textbackslash{}u00d6}\
              |\ \texttt{\textbackslash{}u00d8}\ |\ ...\
              |\ \texttt{\textbackslash{}u00de} \\
  lowercase &::= \texttt{a}\ |\ ...\ |\ \texttt{z}\
              |\ \texttt{\textbackslash{}u00df}\ |\ ...\
              |\ \texttt{\textbackslash{}u00f6}\
              |\ \texttt{\textbackslash{}u00f8}\ |\ ...\
              |\ \texttt{\textbackslash{}u00ff} \\
  inputchar &::= \text{any character except \texttt{CR} and \texttt{LF}} \\
  control   &::= \texttt{\textbackslash{}u0000}\ |\ ...\
              |\ \texttt{\textbackslash{}u001f} \\
  space     &::= \texttt{\textbackslash{}u0020} \\
  namechar  &::= uppercase\ |\ lowercase\ |\ digit\ |\ \texttt{@}\
              |\ \texttt{\_} \\
  escape    &::= \texttt{\textbackslash}\ (octal\
              |\ (\texttt{\textasciicircum}\ ctrlchar)\ |\ escapechar) \\
  octaldigit &::= \texttt{0}\ |\ \texttt{1}\ |\ ...\ |\ \texttt{7} \\
  octal      &::= octaldigit (octal digit octaldigit?)? \\
  ctrlchar   &::= \texttt{\textbackslash{}u0040}\ |\ ...\
               |\ \texttt{\textbackslash{}u005f} \\
  escapechar &::= \texttt{b}\ |\ \texttt{d}\ |\ \texttt{e}\ |\ \texttt{f}\
               |\ \texttt{n}\ |\ \texttt{r}\ |\ \texttt{s}\ |\ \texttt{t}\
               |\ \texttt{v}\ |\ \texttt{"}\ |\ \texttt{'}\
               |\ \texttt{\textbackslash}
\end{align}

\begin{code}
sign, digit :: Parser Char
sign  = oneOf "+-"
digit = PChar.digit
\end{code}

\begin{code}
uppercase, lowercase, inputchar, control, space, namechar :: Parser Char
uppercase = upper
lowercase = lower
inputchar = noneOf "\n\r"
control   = satisfy isControl
space     = char ' '
namechar  = uppercase <|> lowercase <|> digit <|> oneOf "@_"
\end{code}

\begin{code}
escape :: Parser Char
escape    = char '\\' >> (octal <|> ctrl <|> escapechar)
  where
    ctrl :: Parser Char
    ctrl = char '^' >> ctrlchar
\end{code}

\begin{code}
octaldigit, octal, ctrlchar, escapechar :: Parser Char
octaldigit = oneOf "01234567"
octal      = do chars <- tryOctal
                let [(o, _)] = readOct chars
                return (chr o)
  where
    tryOctal :: Parser [Char]
    tryOctal = choice [ try (count 3 octaldigit)
                      , try (count 2 octaldigit)
                      , try (count 1 octaldigit)
                      ]
ctrlchar   = satisfy (`elem` ['\x0040'..'\x005f'])
escapechar = oneOf "bdefnrstv\"\'\\"
\end{code}


\section{Terminals}

\begin{code}
-- | > Integer (i):
-- >    sign? digit+
integer :: Parser Integer
integer = do i <- positive <|> negative <|> decimal
             whiteSpace -- TODO: buff
             return $ i
  where
    positive :: Parser Integer
    positive = char '+' >> decimal
    negative :: Parser Integer
    negative = char '-' >> decimal >>= return . negate
\end{code}

\begin{code}
-- | > Float:
-- >    sign? digit+ . digit+ ((E | e) sign? digit+)?
float :: Parser Double
float = Token.float lexer
\end{code}

\begin{code}
atom :: Parser Atom
atom = do _ <- char '\''
--          ((inputchar except control and \ and ')|escape)*
--          inputchar = noneOf "\n\r"
          a <- many (noneOf "\n\r\\\'")
          _ <- char '\''
          whiteSpace -- TODO: buff
          return $ Atom a
\end{code}

\begin{code}
echar :: Parser Literal
-- char = $((inputchar except control and space and \)|escape)
echar = do _ <- char '$'
           c <- noneOf "\n\r\\ " <|> escape
           whiteSpace -- TODO: buff
           return $ LChar c
\end{code}

\begin{code}
estring :: Parser Literal
-- string = "((inputchar except control and \\ and \"")|escape)*"
estring = do _ <- char '"'
             s <- many $ noneOf "\n\r\\\""
             _ <- char '"'
             return $ LString s
\end{code}

\begin{code}
variableName :: Parser VarName
-- variable = (uppercase | (_ namechar)) namechar*
variableName = identifier
\end{code}


\section{Non-terminals}

\begin{code}
annotatedModule :: Parser (Ann Module)
annotatedModule = annotated module_

module_ :: Parser Module
module_ = do reserved "module"
             name    <- atom
             header  <- moduleHeader
             fundefs <- many funDef
             reserved "end"
             return $ Module name header fundefs

moduleHeader :: Parser ModHeader
moduleHeader = do funs  <- exports
                  attrs <- attributes
                  return $ ModHeader funs attrs

exports :: Parser [FunName]
exports = brackets $ commaSep funName

attributes :: Parser Attributes
attributes = reserved "attributes" *> brackets (commaSep moduleAttribute)

moduleAttribute :: Parser ModAttribute
moduleAttribute = do a <- atom
                     _ <- symbol "="
                     c <- constant
                     return (a,c)

funDef :: Parser FunDef
funDef = do name <- annotated funName
            _    <- symbol "="
            body <- annotated fun
            return $ FunDef name body

funName :: Parser FunName
funName = do a <- atom
             _ <- char '/'
             i <- decimal
             whiteSpace -- TODO: buff
             return (a,i)

fun :: Parser Exp
fun = do reserved "fun"
         vars <- parens $ commaSep (annotated variableName)
         _    <- symbol "->"
         expr <- expression
         return $ Fun vars expr

constant :: Parser Const
constant = liftM CLit   (try literal)    <|>
           liftM CTuple (tuple constant) <|>
           liftM CList  (elist constant)

literal :: Parser Literal
literal = try (liftM LFloat float) <|> liftM LInt integer <|>
          liftM LAtom atom <|> nil <|> echar <|> estring

nil :: Parser Literal
nil = brackets (return LNil)

pattern :: Parser Pat
pattern = liftM PAlias (try   alias)   <|> liftM PVar    variableName      <|>
          liftM PLit   (try   literal) <|> liftM PTuple  (tuple   pattern) <|>
          liftM PList  (elist pattern) <|> liftM PBinary (ebinary pattern)

alias :: Parser Alias
alias = do v <- variableName
           _ <- symbol "="
           p <- pattern
           return $ Alias v p

patterns :: Parser Pats
patterns = liftM Pat pattern <|> liftM Pats (angles $ commaSep pattern)

expression :: Parser Exps
expression =  try (liftM Exps (annotated $ angles $ commaSep (annotated sexpression))) <|>
              liftM Exp (annotated sexpression)

sexpression :: Parser Exp
sexpression = app <|> ecatch <|> ecase <|> elet <|>
              liftM FunName (try funName) {- because of atom -} <|>
              fun <|> letrec <|> liftM Binary (ebinary expression) <|>
              liftM List (try $ elist expression) {- because of nil -} <|>
              liftM Lit literal <|> modCall <|> primOpCall <|> receive <|>
              eseq <|> etry <|> liftM Tuple (tuple expression) <|>
              liftM Var variableName

tuple :: Parser a -> Parser [a]
tuple = braces . commaSep

elist :: Parser a -> Parser (List a)
elist a = brackets $ list a

list :: Parser a -> Parser (List a)
list x = do xs <- commaSep1 x
            option (L xs) (do _ <- symbol "|"
                              t <- x
                              return $ LL xs t)

ebinary :: Parser a -> Parser [BitString a]
ebinary p = do _  <- symbol "#"
               bs <- braces (commaSep (bitstring p))
               _  <- symbol "#"
               return bs

bitstring :: Parser a -> Parser (BitString a)
bitstring p = do _  <- symbol "#"
                 e0 <- angles p
                 es <- parens (commaSep expression)
                 return $ BitString e0 es

elet :: Parser Exp
elet = do reserved "let"
          vars <- variables
          _    <- symbol "="
          e1   <- expression
          _    <- symbol "in"
          e2   <- expression
          return $ Let (vars,e1) e2

variables :: Parser [VarName]
variables =
  do { v <- variableName; return [v] } <|> angles (commaSep variableName)

ecase :: Parser Exp
ecase = do reserved "case"
           exp <- expression
           reserved "of"
           alts <- many1 (annotated clause)
           reserved "end"
           return $ Case exp alts

clause :: Parser Clause
clause = do pat <- patterns
            g   <- guard
            _   <- symbol "->"
            exp <- expression
            return $ Clause pat g exp

guard :: Parser Guard
guard = do reserved "when"
           e <- expression
           return $ Guard e

letrec :: Parser Exp
letrec = do reserved "letrec"
            defs <- many funDef
            reserved "in"
            e <- expression
            return $ Letrec defs e

app :: Parser Exp
app = do reserved "apply"
         e1 <- expression
         eN <- parens $ commaSep expression
         return $ App e1 eN

modCall :: Parser Exp
modCall = do reserved "call"
             e1 <- expression
             _  <- symbol ":"
             e2 <- expression
             eN <- parens $ commaSep expression
             return $ ModCall (e1, e2) eN

primOpCall :: Parser Exp
primOpCall = do reserved "primop"
                a <- atom
                e <- parens $ commaSep expression
                return $ PrimOp a e

etry :: Parser Exp
etry = do reserved "try"
          e1 <- expression
          reserved "of"
          v1 <- variables
          _  <- symbol "->"
          e2 <- expression
          reserved "catch"
          v2 <- variables
          _  <- symbol "->"
          _  <- expression
          return $ Try e1 (v1,e1) (v2,e2)

receive :: Parser Exp
receive = do reserved "receive"
             alts <- many $ annotated clause
             to <- timeout
             return $ Rec alts to

timeout :: Parser Timeout
timeout = do reserved "after"
             e1 <- expression
             _  <- symbol "->"
             e2 <- expression
             return $ Timeout  e1 e2

eseq :: Parser Exp
eseq =  do reserved "do"
           e1 <- expression
           e2 <- expression
           return $ Seq e1 e2

ecatch :: Parser Exp
ecatch = do reserved "catch"
            e <- expression
            return $ Catch e

annotation :: Parser [Const]
annotation = do _  <- symbol "-|"
                cs <- brackets $ many constant
                return cs

annotated :: Parser a -> Parser (Ann a)
annotated p = parens (Ann <$> p <*> annotation) <|> Constr <$> p

lexer :: TokenParser ()
lexer = makeTokenParser
            (emptyDef {
             --    commentStart = "",
             --    commentEnd = "",
                   commentLine = "%%",
             --    nestedComments = True,
                   identStart = upper <|> char '_',
                   identLetter = namechar
             --    opStart,
             --    opLetter,
             --    reservedNames,
             --    reservedOpNames,
             --    caseSensitive = True,
               })


angles, braces, brackets :: Parser a -> Parser a
angles     = Token.angles   lexer
braces     = Token.braces   lexer
brackets   = Token.brackets lexer

commaSep, commaSep1 :: Parser a -> Parser [a]
commaSep   = Token.commaSep  lexer
commaSep1  = Token.commaSep1 lexer

decimal :: Parser Integer
decimal = Token.decimal lexer
\end{code}

\begin{comment}
-- float :: Parser Double
-- float      = Token.float lexer
\end{comment}

\begin{code}
identifier :: Parser String
identifier = Token.identifier lexer
\end{code}

\begin{comment}
-- natural :: Parser Integer
-- natural    = Token.natural lexer
\end{comment}

\begin{code}
parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer
\end{code}

\begin{comment}
-- reservedOp :: String -> Parser ()
-- reservedOp = Token.reservedOp lexer
\end{comment}

\begin{code}
symbol :: String -> Parser String
symbol = Token.symbol lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
\end{code}

\begin{comment}
-- runLex :: Show a => Parser a -> String -> IO ()
-- runLex p file = do input <- readFile file
--                    parseTest (do whiteSpace
--                                  x <- p
--                                  eof
--                                  return x) input
--                    return ()
\end{comment}

\begin{code}
-- | Parse of a string, which should contain a complete CoreErlang module
parseModule :: String -> Either ParseError (Ann Module)
parseModule input = parse (do whiteSpace
                              x <- annotatedModule
                              eof
                              return x) "" input
\end{code}
