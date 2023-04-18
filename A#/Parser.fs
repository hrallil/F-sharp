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
  | PLUS  -> 10 
  | MINUS  -> 11 
  | STAR  -> 12 
  | LPAR  -> 13 
  | RPAR  -> 14 
  | HAT  -> 15 
  | DIV  -> 16 
  | MOD  -> 17 
  | SEMICOL  -> 18 
  | WRITE  -> 19 
  | READ  -> 20 
  | LET  -> 21 
  | DEF  -> 22 
  | FUNC  -> 23 
  | IN  -> 24 
  | IF  -> 25 
  | THEN  -> 26 
  | ELSE  -> 27 
  | NAME _ -> 28 
  | INT _ -> 29 

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
  | 10 -> TOKEN_PLUS 
  | 11 -> TOKEN_MINUS 
  | 12 -> TOKEN_STAR 
  | 13 -> TOKEN_LPAR 
  | 14 -> TOKEN_RPAR 
  | 15 -> TOKEN_HAT 
  | 16 -> TOKEN_DIV 
  | 17 -> TOKEN_MOD 
  | 18 -> TOKEN_SEMICOL 
  | 19 -> TOKEN_WRITE 
  | 20 -> TOKEN_READ 
  | 21 -> TOKEN_LET 
  | 22 -> TOKEN_DEF 
  | 23 -> TOKEN_FUNC 
  | 24 -> TOKEN_IN 
  | 25 -> TOKEN_IF 
  | 26 -> TOKEN_THEN 
  | 27 -> TOKEN_ELSE 
  | 28 -> TOKEN_NAME 
  | 29 -> TOKEN_INT 
  | 32 -> TOKEN_end_of_input
  | 30 -> TOKEN_error
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
    | 11 -> NONTERM_exp 
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
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 32 
let _fsyacc_tagOfErrorTerminal = 30

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
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 2us; 65535us; 0us; 2us; 5us; 6us; 2us; 65535us; 0us; 5us; 5us; 5us; 1us; 65535us; 9us; 10us; 2us; 65535us; 9us; 15us; 17us; 18us; 2us; 65535us; 19us; 20us; 63us; 64us; 25us; 65535us; 2us; 3us; 12us; 13us; 19us; 19us; 22us; 23us; 27us; 28us; 29us; 30us; 49us; 31us; 50us; 32us; 51us; 33us; 52us; 34us; 53us; 35us; 54us; 36us; 55us; 37us; 56us; 38us; 57us; 39us; 59us; 40us; 63us; 19us; 66us; 41us; 67us; 42us; 68us; 43us; 69us; 44us; 70us; 45us; 71us; 46us; 72us; 47us; 73us; 48us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 6us; 9us; 11us; 14us; 17us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 15us; 1us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 1us; 1us; 1us; 3us; 1us; 3us; 1us; 4us; 1us; 4us; 1us; 4us; 1us; 4us; 1us; 4us; 1us; 4us; 15us; 4us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 1us; 4us; 1us; 6us; 2us; 7us; 8us; 1us; 8us; 1us; 8us; 16us; 9us; 10us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 1us; 10us; 1us; 11us; 1us; 12us; 15us; 12us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 1us; 12us; 1us; 13us; 1us; 13us; 1us; 13us; 15us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 1us; 13us; 15us; 13us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 20us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 21us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 26us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 27us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 28us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 29us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 30us; 31us; 15us; 14us; 15us; 16us; 17us; 18us; 19us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 31us; 1us; 14us; 1us; 15us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 20us; 1us; 20us; 1us; 21us; 1us; 21us; 1us; 21us; 1us; 22us; 2us; 23us; 32us; 1us; 23us; 1us; 23us; 1us; 23us; 1us; 24us; 1us; 25us; 1us; 26us; 1us; 27us; 1us; 28us; 1us; 29us; 1us; 30us; 1us; 31us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 22us; 24us; 26us; 28us; 30us; 32us; 34us; 36us; 38us; 40us; 56us; 58us; 60us; 63us; 65us; 67us; 84us; 86us; 88us; 90us; 106us; 108us; 110us; 112us; 114us; 130us; 132us; 148us; 164us; 180us; 196us; 212us; 228us; 244us; 260us; 276us; 292us; 308us; 324us; 340us; 356us; 372us; 388us; 404us; 420us; 436us; 438us; 440us; 442us; 444us; 446us; 448us; 450us; 452us; 454us; 456us; 458us; 460us; 462us; 465us; 467us; 469us; 471us; 473us; 475us; 477us; 479us; 481us; 483us; 485us; |]
let _fsyacc_action_rows = 74
let _fsyacc_actionTableElements = [|1us; 16386us; 23us; 7us; 0us; 49152us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 15us; 32768us; 1us; 4us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 0us; 16385us; 1us; 16386us; 23us; 7us; 0us; 16387us; 1us; 32768us; 28us; 8us; 1us; 32768us; 13us; 9us; 1us; 16389us; 28us; 16us; 1us; 32768us; 14us; 11us; 1us; 32768us; 22us; 12us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 15us; 32768us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 18us; 14us; 0us; 16388us; 0us; 16390us; 1us; 16391us; 0us; 17us; 1us; 32768us; 28us; 16us; 0us; 16392us; 21us; 16393us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 13us; 22us; 15us; 52us; 16us; 54us; 17us; 67us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 0us; 16394us; 0us; 16395us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 15us; 32768us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 14us; 24us; 15us; 52us; 16us; 54us; 17us; 67us; 0us; 16396us; 1us; 32768us; 28us; 26us; 1us; 32768us; 22us; 27us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 15us; 32768us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 24us; 29us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 14us; 16397us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 12us; 16398us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 12us; 16399us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 11us; 16400us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 15us; 52us; 16us; 54us; 17us; 67us; 11us; 16401us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 15us; 52us; 16us; 54us; 17us; 67us; 14us; 16402us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 9us; 16403us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 17us; 67us; 15us; 32768us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 26us; 56us; 15us; 32768us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 27us; 57us; 14us; 16404us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 15us; 32768us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 14us; 60us; 15us; 52us; 16us; 54us; 17us; 67us; 14us; 16408us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 14us; 16409us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 14us; 16410us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 14us; 16411us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 14us; 16412us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 14us; 16413us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 14us; 16414us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 14us; 16415us; 2us; 53us; 3us; 66us; 4us; 68us; 5us; 70us; 6us; 69us; 7us; 71us; 8us; 72us; 9us; 73us; 10us; 49us; 11us; 50us; 12us; 51us; 15us; 52us; 16us; 54us; 17us; 67us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 1us; 32768us; 13us; 59us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 0us; 16405us; 0us; 16406us; 1us; 16416us; 13us; 63us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 1us; 32768us; 14us; 65us; 0us; 16407us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; 7us; 32768us; 13us; 22us; 19us; 58us; 20us; 61us; 21us; 25us; 25us; 55us; 28us; 62us; 29us; 21us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 11us; 27us; 28us; 30us; 31us; 33us; 35us; 37us; 39us; 41us; 49us; 65us; 66us; 67us; 69us; 71us; 72us; 94us; 95us; 96us; 104us; 120us; 121us; 123us; 125us; 133us; 149us; 157us; 172us; 185us; 198us; 210us; 222us; 237us; 247us; 263us; 279us; 294us; 310us; 325us; 340us; 355us; 370us; 385us; 400us; 415us; 430us; 438us; 446us; 454us; 462us; 470us; 478us; 486us; 494us; 502us; 504us; 512us; 513us; 514us; 516us; 524us; 526us; 527us; 535us; 543us; 551us; 559us; 567us; 575us; 583us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 3us; 0us; 2us; 8us; 0us; 1us; 1us; 3us; 1us; 2us; 1us; 3us; 6us; 3us; 3us; 3us; 3us; 3us; 3us; 6us; 4us; 1us; 4us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 4us; 4us; 5us; 5us; 6us; 6us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 65535us; 16385us; 65535us; 16387us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16388us; 16390us; 65535us; 65535us; 16392us; 65535us; 16394us; 16395us; 65535us; 65535us; 16396us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16405us; 16406us; 65535us; 65535us; 65535us; 16407us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |]
let _fsyacc_reductions ()  =    [| 
# 277 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> (Syntax.funcDef list * Syntax.exp) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startstart));
# 286 "Parser.fs"
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
# 298 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "Parser.fsy"
                                                      [] 
                   )
# 41 "Parser.fsy"
                 : Syntax.funcDef list));
# 308 "Parser.fs"
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
# 320 "Parser.fs"
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
# 333 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Parser.fsy"
                                                     [] 
                   )
# 49 "Parser.fsy"
                 : Syntax.varName list));
# 343 "Parser.fs"
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
# 354 "Parser.fs"
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
# 365 "Parser.fs"
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
# 377 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                                     [_1] 
                   )
# 57 "Parser.fsy"
                 : Syntax.exp list));
# 388 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _2 = parseState.GetInput(2) :?> Syntax.exp list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Parser.fsy"
                                                     _1::_2 
                   )
# 58 "Parser.fsy"
                 : Syntax.exp list));
# 400 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "Parser.fsy"
                                                     Syntax.INT _1 
                   )
# 61 "Parser.fsy"
                 : Syntax.exp));
# 411 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "Parser.fsy"
                                                     _2 
                   )
# 62 "Parser.fsy"
                 : Syntax.exp));
# 422 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> Syntax.exp in
            let _6 = parseState.GetInput(6) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "Parser.fsy"
                                                     Syntax.LET(_2, _4, _6) 
                   )
# 63 "Parser.fsy"
                 : Syntax.exp));
# 435 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "Parser.fsy"
                                                     Syntax.ADD(_1, _3) 
                   )
# 64 "Parser.fsy"
                 : Syntax.exp));
# 447 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "Parser.fsy"
                                                     Syntax.SUB(_1, _3) 
                   )
# 65 "Parser.fsy"
                 : Syntax.exp));
# 459 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "Parser.fsy"
                                                     Syntax.MUL(_1, _3) 
                   )
# 66 "Parser.fsy"
                 : Syntax.exp));
# 471 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "Parser.fsy"
                                                     Syntax.EXP(_1, _3) 
                   )
# 67 "Parser.fsy"
                 : Syntax.exp));
# 483 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "Parser.fsy"
                                                     Syntax.EQ (_1, _3) 
                   )
# 68 "Parser.fsy"
                 : Syntax.exp));
# 495 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "Parser.fsy"
                                                     Syntax.DIV(_1, _3) 
                   )
# 69 "Parser.fsy"
                 : Syntax.exp));
# 507 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> Syntax.exp in
            let _4 = parseState.GetInput(4) :?> Syntax.exp in
            let _6 = parseState.GetInput(6) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "Parser.fsy"
                                                     Syntax.IF (_2, _4, _6) 
                   )
# 70 "Parser.fsy"
                 : Syntax.exp));
# 520 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "Parser.fsy"
                                                     Syntax.WRITE(_3) 
                   )
# 71 "Parser.fsy"
                 : Syntax.exp));
# 531 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "Parser.fsy"
                                                     Syntax.READ 
                   )
# 72 "Parser.fsy"
                 : Syntax.exp));
# 541 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _3 = parseState.GetInput(3) :?> Syntax.exp list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 73 "Parser.fsy"
                                                     Syntax.CALL(_1, _3) 
                   )
# 73 "Parser.fsy"
                 : Syntax.exp));
# 553 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "Parser.fsy"
                                                     Syntax.NEQ(_1, _3) 
                   )
# 75 "Parser.fsy"
                 : Syntax.exp));
# 565 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "Parser.fsy"
                                                     Syntax.MOD(_1, _3) 
                   )
# 76 "Parser.fsy"
                 : Syntax.exp));
# 577 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "Parser.fsy"
                                                     Syntax.LT(_1, _3) 
                   )
# 77 "Parser.fsy"
                 : Syntax.exp));
# 589 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "Parser.fsy"
                                                     Syntax.LTEQ(_1, _3) 
                   )
# 78 "Parser.fsy"
                 : Syntax.exp));
# 601 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "Parser.fsy"
                                                     Syntax.GT(_1, _3) 
                   )
# 79 "Parser.fsy"
                 : Syntax.exp));
# 613 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "Parser.fsy"
                                                     Syntax.GTEQ(_1, _3) 
                   )
# 80 "Parser.fsy"
                 : Syntax.exp));
# 625 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "Parser.fsy"
                                                     Syntax.AND(_1, _3) 
                   )
# 81 "Parser.fsy"
                 : Syntax.exp));
# 637 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Syntax.exp in
            let _3 = parseState.GetInput(3) :?> Syntax.exp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "Parser.fsy"
                                                     Syntax.OR(_1, _3) 
                   )
# 82 "Parser.fsy"
                 : Syntax.exp));
# 649 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 83 "Parser.fsy"
                                                     Syntax.VAR _1 
                   )
# 83 "Parser.fsy"
                 : Syntax.exp));
|]
# 661 "Parser.fs"
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
    numTerminals = 33;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = tables.Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : (Syntax.funcDef list * Syntax.exp) =
    engine lexer lexbuf 0 :?> _
