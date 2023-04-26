// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"


# 9 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | COMMA
  | EOF
  | EQ
  | NEQ
  | LT
  | GT
  | LTEQ
  | GTEQ
  | AND
  | OR
  | TRUE
  | FALSE
  | PLUS
  | MINUS
  | STAR
  | LPAR
  | RPAR
  | HAT
  | DIV
  | MOD
  | SEMICOL
  | WRITE
  | READ
  | LET
  | DEF
  | FUNC
  | IN
  | IF
  | THEN
  | ELSE
  | NAME of (string)
  | INT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_COMMA
    | TOKEN_EOF
    | TOKEN_EQ
    | TOKEN_NEQ
    | TOKEN_LT
    | TOKEN_GT
    | TOKEN_LTEQ
    | TOKEN_GTEQ
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_STAR
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_HAT
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_SEMICOL
    | TOKEN_WRITE
    | TOKEN_READ
    | TOKEN_LET
    | TOKEN_DEF
    | TOKEN_FUNC
    | TOKEN_IN
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_NAME
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_FDS
    | NONTERM_funcDef
    | NONTERM_NO_NAME_OR_MORE
    | NONTERM_NAME_OR_MORE
    | NONTERM_EXPS
    | NONTERM_exp

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | COMMA  -> 0 
  | EOF  -> 1 
  | EQ  -> 2 
  | NEQ  -> 3 
  | LT  -> 4 
  | GT  -> 5 
  | LTEQ  -> 6 
  | GTEQ  -> 7 
  | AND  -> 8 
  | OR  -> 9 
  | TRUE  -> 10 
  | FALSE  -> 11 
  | PLUS  -> 12 
  | MINUS  -> 13 
  | STAR  -> 14 
  | LPAR  -> 15 
  | RPAR  -> 16 
  | HAT  -> 17 
  | DIV  -> 18 
  | MOD  -> 19 
  | SEMICOL  -> 20 
  | WRITE  -> 21 
  | READ  -> 22 
  | LET  -> 23 
  | DEF  -> 24 
  | FUNC  -> 25 
  | IN  -> 26 
  | IF  -> 27 
  | THEN  -> 28 
  | ELSE  -> 29 
  | NAME _ -> 30 
  | INT _ -> 31 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_COMMA 
  | 1 -> TOKEN_EOF 
  | 2 -> TOKEN_EQ 
  | 3 -> TOKEN_NEQ 
  | 4 -> TOKEN_LT 
  | 5 -> TOKEN_GT 
  | 6 -> TOKEN_LTEQ 
  | 7 -> TOKEN_GTEQ 
  | 8 -> TOKEN_AND 
  | 9 -> TOKEN_OR 
  | 10 -> TOKEN_TRUE 
  | 11 -> TOKEN_FALSE 
  | 12 -> TOKEN_PLUS 
  | 13 -> TOKEN_MINUS 
  | 14 -> TOKEN_STAR 
  | 15 -> TOKEN_LPAR 
  | 16 -> TOKEN_RPAR 
  | 17 -> TOKEN_HAT 
  | 18 -> TOKEN_DIV 
  | 19 -> TOKEN_MOD 
  | 20 -> TOKEN_SEMICOL 
  | 21 -> TOKEN_WRITE 
  | 22 -> TOKEN_READ 
  | 23 -> TOKEN_LET 
  | 24 -> TOKEN_DEF 
  | 25 -> TOKEN_FUNC 
  | 26 -> TOKEN_IN 
  | 27 -> TOKEN_IF 
  | 28 -> TOKEN_THEN 
  | 29 -> TOKEN_ELSE 
  | 30 -> TOKEN_NAME 
  | 31 -> TOKEN_INT 
  | 34 -> TOKEN_end_of_input
  | 32 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_FDS 
    | 3 -> NONTERM_FDS 
    | 4 -> NONTERM_funcDef 
    | 5 -> NONTERM_NO_NAME_OR_MORE 
    | 6 -> NONTERM_NO_NAME_OR_MORE 
    | 7 -> NONTERM_NAME_OR_MORE 
    | 8 -> NONTERM_NAME_OR_MORE 
    | 9 -> NONTERM_EXPS 
    | 10 -> NONTERM_EXPS 
    | 11 -> NONTERM_EXPS 
    | 12 -> NONTERM_exp 
    | 13 -> NONTERM_exp 
    | 14 -> NONTERM_exp 
    | 15 -> NONTERM_exp 
    | 16 -> NONTERM_exp 
    | 17 -> NONTERM_exp 
    | 18 -> NONTERM_exp 
    | 19 -> NONTERM_exp 
    | 20 -> NONTERM_exp 
    | 21 -> NONTERM_exp 
    | 22 -> NONTERM_exp 
    | 23 -> NONTERM_exp 
    | 24 -> NONTERM_exp 
    | 25 -> NONTERM_exp 
    | 26 -> NONTERM_exp 
    | 27 -> NONTERM_exp 
    | 28 -> NONTERM_exp 
    | 29 -> NONTERM_exp 
    | 30 -> NONTERM_exp 
    | 31 -> NONTERM_exp 
    | 32 -> NONTERM_exp 
    | 33 -> NONTERM_exp 
    | 34 -> NONTERM_exp 
    | 35 -> NONTERM_exp 
    | 36 -> NONTERM_exp 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 34 
let _fsyacc_tagOfErrorTerminal = 32

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | COMMA  -> "COMMA" 
  | EOF  -> "EOF" 
  | EQ  -> "EQ" 
  | NEQ  -> "NEQ" 
  | LT  -> "LT" 
  | GT  -> "GT" 
  | LTEQ  -> "LTEQ" 
  | GTEQ  -> "GTEQ" 
  | AND  -> "AND" 
  | OR  -> "OR" 
  | TRUE  -> "TRUE" 
  | FALSE  -> "FALSE" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | STAR  -> "STAR" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | HAT  -> "HAT" 
  | DIV  -> "DIV" 
  | MOD  -> "MOD" 
  | SEMICOL  -> "SEMICOL" 
  | WRITE  -> "WRITE" 
  | READ  -> "READ" 
  | LET  -> "LET" 
  | DEF  -> "DEF" 
  | FUNC  -> "FUNC" 
  | IN  -> "IN" 
  | IF  -> "IF" 
  | THEN  -> "THEN" 
  | ELSE  -> "ELSE" 
  | NAME _ -> "NAME" 
  | INT _ -> "INT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | COMMA  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | NEQ  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | LTEQ  -> (null : System.Object) 
  | GTEQ  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | STAR  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | HAT  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | MOD  -> (null : System.Object) 
  | SEMICOL  -> (null : System.Object) 
  | WRITE  -> (null : System.Object) 
  | READ  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | DEF  -> (null : System.Object) 
  | FUNC  -> (null : System.Object) 
  | IN  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 2us; 65535us; 0us; 2us; 5us; 6us; 2us; 65535us; 0us; 5us; 5us; 5us; 1us; 65535us; 9us; 10us; 2us; 65535us; 9us; 15us; 17us; 18us; 2us; 65535us; 20us; 21us; 67us; 68us; 26us; 65535us; 2us; 3us; 12us; 13us; 20us; 19us; 25us; 26us; 30us; 31us; 32us; 33us; 34us; 35us; 54us; 36us; 55us; 37us; 56us; 38us; 57us; 39us; 58us; 40us; 59us; 41us; 60us; 42us; 61us; 43us; 63us; 44us; 67us; 19us; 70us; 45us; 71us; 46us; 72us; 47us; 73us; 48us; 74us; 49us; 75us; 50us; 76us; 51us; 77us; 52us; 78us; 53us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 6us; 9us; 11us; 14us; 17us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 15us; 1us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 1us; 1us; 1us; 3us; 1us; 3us; 1us; 4us; 1us; 4us; 1us; 4us; 1us; 4us; 1us; 4us; 1us; 4us; 15us; 4us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 1us; 4us; 1us; 6us; 2us; 7us; 8us; 1us; 8us; 1us; 8us; 16us; 10us; 11us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 1us; 11us; 1us; 11us; 1us; 12us; 1us; 13us; 1us; 14us; 1us; 15us; 15us; 15us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 1us; 15us; 1us; 16us; 1us; 16us; 1us; 16us; 15us; 16us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 1us; 16us; 15us; 16us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 1us; 17us; 15us; 17us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 23us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 23us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 23us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 24us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 27us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 30us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 31us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 32us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 33us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 34us; 35us; 15us; 18us; 19us; 20us; 21us; 22us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 34us; 35us; 35us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 21us; 1us; 22us; 1us; 23us; 1us; 23us; 1us; 23us; 1us; 24us; 1us; 24us; 1us; 24us; 1us; 25us; 2us; 26us; 36us; 1us; 26us; 1us; 26us; 1us; 26us; 1us; 27us; 1us; 28us; 1us; 29us; 1us; 30us; 1us; 31us; 1us; 32us; 1us; 33us; 1us; 34us; 1us; 35us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 22us; 24us; 26us; 28us; 30us; 32us; 34us; 36us; 38us; 40us; 56us; 58us; 60us; 63us; 65us; 67us; 84us; 86us; 88us; 90us; 92us; 94us; 96us; 112us; 114us; 116us; 118us; 120us; 136us; 138us; 154us; 156us; 172us; 188us; 204us; 220us; 236us; 252us; 268us; 284us; 300us; 316us; 332us; 348us; 364us; 380us; 396us; 412us; 428us; 444us; 460us; 462us; 464us; 466us; 468us; 470us; 472us; 474us; 476us; 478us; 480us; 482us; 484us; 487us; 489us; 491us; 493us; 495us; 497us; 499us; 501us; 503us; 505us; 507us; 509us; |]
let _fsyacc_action_rows = 79
let _fsyacc_actionTableElements = [|1us; 16386us; 25us; 7us; 0us; 49152us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 15us; 32768us; 1us; 4us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 0us; 16385us; 1us; 16386us; 25us; 7us; 0us; 16387us; 1us; 32768us; 30us; 8us; 1us; 32768us; 15us; 9us; 1us; 16389us; 30us; 16us; 1us; 32768us; 16us; 11us; 1us; 32768us; 24us; 12us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 15us; 32768us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 20us; 14us; 0us; 16388us; 0us; 16390us; 1us; 16391us; 0us; 17us; 1us; 32768us; 30us; 16us; 0us; 16392us; 15us; 16394us; 0us; 20us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 10us; 16393us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 0us; 16395us; 0us; 16396us; 0us; 16397us; 0us; 16398us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 15us; 32768us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 16us; 27us; 17us; 57us; 18us; 58us; 19us; 71us; 0us; 16399us; 1us; 32768us; 30us; 29us; 1us; 32768us; 24us; 30us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 15us; 32768us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 26us; 32us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 14us; 16400us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 11us; 16401us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 11us; 16402us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 11us; 16403us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 8us; 16404us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 17us; 57us; 8us; 16405us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 17us; 57us; 8us; 16406us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 17us; 57us; 15us; 32768us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 28us; 60us; 15us; 32768us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 29us; 61us; 14us; 16407us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 15us; 32768us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 16us; 64us; 17us; 57us; 18us; 58us; 19us; 71us; 14us; 16411us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 8us; 16412us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 17us; 57us; 13us; 16413us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 14us; 16414us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 14us; 16415us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 14us; 16416us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 14us; 16417us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 14us; 16418us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 14us; 16419us; 2us; 72us; 3us; 70us; 4us; 73us; 5us; 75us; 6us; 74us; 7us; 76us; 8us; 77us; 9us; 78us; 12us; 54us; 13us; 55us; 14us; 56us; 17us; 57us; 18us; 58us; 19us; 71us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 1us; 32768us; 15us; 63us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 0us; 16408us; 0us; 16409us; 1us; 16420us; 15us; 67us; 10us; 16393us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 1us; 32768us; 16us; 69us; 0us; 16410us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; 10us; 32768us; 10us; 23us; 11us; 24us; 13us; 34us; 15us; 25us; 21us; 62us; 22us; 65us; 23us; 28us; 27us; 59us; 30us; 66us; 31us; 22us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 14us; 30us; 31us; 33us; 34us; 36us; 38us; 40us; 42us; 44us; 55us; 71us; 72us; 73us; 75us; 77us; 78us; 94us; 105us; 106us; 107us; 108us; 109us; 120us; 136us; 137us; 139us; 141us; 152us; 168us; 179us; 194us; 205us; 217us; 229us; 241us; 250us; 259us; 268us; 284us; 300us; 315us; 331us; 346us; 355us; 369us; 384us; 399us; 414us; 429us; 444us; 459us; 470us; 481us; 492us; 503us; 514us; 525us; 536us; 547us; 549us; 560us; 561us; 562us; 564us; 575us; 577us; 578us; 589us; 600us; 611us; 622us; 633us; 644us; 655us; 666us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 3us; 0us; 2us; 8us; 0us; 1us; 1us; 3us; 0us; 1us; 3us; 1us; 1us; 1us; 3us; 6us; 2us; 3us; 3us; 3us; 3us; 3us; 6us; 4us; 1us; 4us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 4us; 4us; 5us; 5us; 6us; 6us; 6us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 65535us; 16385us; 65535us; 16387us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16388us; 16390us; 65535us; 65535us; 16392us; 65535us; 65535us; 16395us; 16396us; 16397us; 16398us; 65535us; 65535us; 16399us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16408us; 16409us; 65535us; 65535us; 65535us; 16410us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |]
let _fsyacc_reductions ()  =    [| 
# 293 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> (Syntax.funcDef list * Syntax.exp) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startstart));
# 302 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.funcDef list in
            let _2 = parseState.GetInput(2) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                                                         _1, _2 
                   )
# 38 "Parser.fsy"
                 : (Syntax.funcDef list * Syntax.exp)));
# 314 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "Parser.fsy"
                                                      [] 
                   )
# 41 "Parser.fsy"
                 : Syntax.funcDef list));
# 324 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.funcDef in
            let _2 = parseState.GetInput(2) :?> Syntax.funcDef list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "Parser.fsy"
                                                      _1::_2 
                   )
# 42 "Parser.fsy"
                 : Syntax.funcDef list));
# 336 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> Syntax.varName list in
            let _7 = parseState.GetInput(7) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "Parser.fsy"
                                                                                   Syntax.funcDef(_2, (_4, _7)) 
                   )
# 46 "Parser.fsy"
                 : Syntax.funcDef));
# 349 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Parser.fsy"
                                                     [] 
                   )
# 49 "Parser.fsy"
                 : Syntax.varName list));
# 359 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.varName list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "Parser.fsy"
                                                     _1 
                   )
# 50 "Parser.fsy"
                 : Syntax.varName list));
# 370 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "Parser.fsy"
                                                     [_1] 
                   )
# 53 "Parser.fsy"
                 : Syntax.varName list));
# 381 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _3 = parseState.GetInput(3) :?> Syntax.varName list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "Parser.fsy"
                                                     _1::_3 
                   )
# 54 "Parser.fsy"
                 : Syntax.varName list));
# 393 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                                     [] 
                   )
# 57 "Parser.fsy"
                 : Syntax.exp list));
# 403 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Parser.fsy"
                                                     [_1] 
                   )
# 58 "Parser.fsy"
                 : Syntax.exp list));
# 414 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "Parser.fsy"
                                                     _1::_3 
                   )
# 59 "Parser.fsy"
                 : Syntax.exp list));
# 426 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "Parser.fsy"
                                                     Syntax.INT _1 
                   )
# 62 "Parser.fsy"
                 : Syntax.exp));
# 437 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "Parser.fsy"
                                                     Syntax.TRUE
                   )
# 63 "Parser.fsy"
                 : Syntax.exp));
# 447 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "Parser.fsy"
                                                     Syntax.FALSE
                   )
# 64 "Parser.fsy"
                 : Syntax.exp));
# 457 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "Parser.fsy"
                                                     _2 
                   )
# 65 "Parser.fsy"
                 : Syntax.exp));
# 468 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> Syntax.exp in
            let _6 = parseState.GetInput(6) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "Parser.fsy"
                                                     Syntax.LET(_2, _4, _6) 
                   )
# 66 "Parser.fsy"
                 : Syntax.exp));
# 481 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "Parser.fsy"
                                                     Syntax.NEG(_2) 
                   )
# 67 "Parser.fsy"
                 : Syntax.exp));
# 492 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "Parser.fsy"
                                                     Syntax.ADD(_1, _3) 
                   )
# 68 "Parser.fsy"
                 : Syntax.exp));
# 504 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "Parser.fsy"
                                                     Syntax.SUB(_1, _3) 
                   )
# 69 "Parser.fsy"
                 : Syntax.exp));
# 516 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "Parser.fsy"
                                                     Syntax.MUL(_1, _3) 
                   )
# 70 "Parser.fsy"
                 : Syntax.exp));
# 528 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "Parser.fsy"
                                                     Syntax.EXP(_1, _3) 
                   )
# 71 "Parser.fsy"
                 : Syntax.exp));
# 540 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "Parser.fsy"
                                                     Syntax.DIV(_1, _3) 
                   )
# 72 "Parser.fsy"
                 : Syntax.exp));
# 552 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> Syntax.exp in
            let _4 = parseState.GetInput(4) :?> Syntax.exp in
            let _6 = parseState.GetInput(6) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 73 "Parser.fsy"
                                                     Syntax.IF (_2, _4, _6) 
                   )
# 73 "Parser.fsy"
                 : Syntax.exp));
# 565 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "Parser.fsy"
                                                     Syntax.WRITE(_3) 
                   )
# 74 "Parser.fsy"
                 : Syntax.exp));
# 576 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "Parser.fsy"
                                                     Syntax.READ 
                   )
# 75 "Parser.fsy"
                 : Syntax.exp));
# 586 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _3 = parseState.GetInput(3) :?> Syntax.exp list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "Parser.fsy"
                                                     Syntax.CALL(_1, _3) 
                   )
# 76 "Parser.fsy"
                 : Syntax.exp));
# 598 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "Parser.fsy"
                                                     Syntax.NEQ(_1, _3) 
                   )
# 77 "Parser.fsy"
                 : Syntax.exp));
# 610 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "Parser.fsy"
                                                     Syntax.MOD(_1, _3) 
                   )
# 78 "Parser.fsy"
                 : Syntax.exp));
# 622 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "Parser.fsy"
                                                     Syntax.EQ (_1, _3) 
                   )
# 79 "Parser.fsy"
                 : Syntax.exp));
# 634 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "Parser.fsy"
                                                     Syntax.LT(_1, _3) 
                   )
# 80 "Parser.fsy"
                 : Syntax.exp));
# 646 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "Parser.fsy"
                                                     Syntax.LTEQ(_1, _3) 
                   )
# 81 "Parser.fsy"
                 : Syntax.exp));
# 658 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "Parser.fsy"
                                                     Syntax.GT(_1, _3) 
                   )
# 82 "Parser.fsy"
                 : Syntax.exp));
# 670 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 83 "Parser.fsy"
                                                     Syntax.GTEQ(_1, _3) 
                   )
# 83 "Parser.fsy"
                 : Syntax.exp));
# 682 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 84 "Parser.fsy"
                                                     Syntax.AND(_1, _3) 
                   )
# 84 "Parser.fsy"
                 : Syntax.exp));
# 694 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 85 "Parser.fsy"
                                                     Syntax.OR(_1, _3) 
                   )
# 85 "Parser.fsy"
                 : Syntax.exp));
# 706 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "Parser.fsy"
                                                     Syntax.VAR _1 
                   )
# 86 "Parser.fsy"
                 : Syntax.exp));
|]
# 718 "Parser.fs"
let tables : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 35;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = tables.Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : (Syntax.funcDef list * Syntax.exp) =
    engine lexer lexbuf 0 :?> _
