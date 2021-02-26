module Ass7

open System
open FParsec

type aExp =
    | N of int              (* Integer literal *)
    | V of string           (* Variable reference *)
        
    | WL                    (* Word length *)
    | PV of aExp            (* Point value lookup at word index *)
        
    | Add of aExp * aExp    (* Addition *)
    | Sub of aExp * aExp    (* Subtraction *)
    | Mul of aExp * aExp    (* Multiplication *)
    | Div of aExp * aExp    (* Division *)
    | Mod of aExp * aExp    (* Modulo *)

    | CharToInt of cExp     (* Cast to integer *)

and cExp =
    | C  of char             (* Character literal *)
    | CV of aExp             (* Character lookup at word index *)
       
    | ToUpper of cExp        (* Convert character to upper case *)
    | ToLower of cExp        (* Convert character to lower case *)
       
    | IntToChar of aExp      (* Cast to character *)

type bExp =             
    | TT                   (* True *)
    | FF                   (* False *)
        
    | AEq of aExp * aExp   (* Numeric equality *)
    | ALt of aExp * aExp   (* Numeric less than *)
        
    | Not of bExp          (* Boolean not *)
    | Conj of bExp * bExp  (* Boolean conjunction *)
        
    | IsVowel of cExp      (* Check for vowel *)
    | IsConsonant of cExp  (* Check for constant *)

type stm =                
    | Declare of string       (* NEW: Variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* Nop *)
    | Seq of stm * stm        (* Sequential composition *)
    | ITE of bExp * stm * stm (* If-Then-Else statement *)
    | While of bExp * stm     (* While statement *)


module StateMonad =
    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string    
        | StackEmpty       

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    type word = (char * int) list
    type squareFun = word -> int -> int -> int


    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = 
        S (fun s -> match s.vars with | _::rest -> Success((), {s with vars = rest}))

    let wordLength : SM<int> = 
        S (fun s -> Success(s.word.Length, s))

    let characterValue (pos : int) : SM<char> = 
        S (fun s -> if pos < s.word.Length && pos >= 0 then Success(fst s.word.[pos], s) else Failure(IndexOutOfBounds pos))

    let pointValue (pos : int) : SM<int> = 
        S (fun s -> if pos < s.word.Length && pos >= 0 then Success(snd s.word.[pos], s) else Failure(IndexOutOfBounds pos))

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = 
        S (fun state -> 
            if (Set.contains var state.reserved) then
                Failure(ReservedName var)
            else 
                match state.vars with
                | x::xs -> if (Map.containsKey var x) then Failure(VarExists var) else Success((), {state with vars = Map.add var 0 x :: xs})
                | []    -> Failure(StackEmpty))

    let update (var : string) (value : int) : SM<unit> = 
        S (fun state ->
            let rec aux s (s1: Map<string, int> list) : Result<'a * State, Error> =
                match s with
                | x::xs -> 
                    if Map.containsKey var x then
                        Success((), {state with vars = (s1 @ Map.add var value x :: xs)})
                    else
                        aux (xs) (s1 @ [x])
                | []    ->
                    Failure(VarNotFound var)
            aux state.vars []
        )


    let add (a: SM<int>) (b: SM<int>) : SM<int> = 
        a >>= (fun x -> b >>= (fun y -> ret (x+y)))

    let sub (a: SM<int>) (b: SM<int>) : SM<int> = 
        a >>= (fun x -> b >>= (fun y -> ret (x-y)))

    let mul (a: SM<int>) (b: SM<int>) : SM<int> = 
        a >>= (fun x -> b >>= (fun y -> ret (x*y)))

    let div a b = 
        b >>= (fun y ->
            if y = 0 then
                fail DivisionByZero
            else
                a >>= (fun x ->
                    ret (x/y)))

    let modulo a b = 
        b >>= (fun y ->
            if y = 0 then
                fail DivisionByZero
            else
                a >>= (fun x ->
                    ret (x % y)))


    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V v -> lookup v
        | WL -> wordLength
        | PV pv -> arithEval pv >>= (fun x -> pointValue x)
        | Add (a,b) -> add (arithEval a) (arithEval b)
        | Sub (a,b) -> sub (arithEval a) (arithEval b)
        | Mul (a,b) -> mul (arithEval a) (arithEval b)
        | Div (a,b) -> div (arithEval a) (arithEval b)
        | Mod (a,b) -> modulo (arithEval a) (arithEval b)
        | CharToInt c -> charEval c >>= (fun x -> ret (int(x)))
    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | CV cv -> arithEval cv >>= (fun x -> characterValue x)
        | ToUpper c -> charEval c >>= (fun x -> ret (System.Char.ToUpper x))
        | ToLower c -> charEval c >>= (fun x -> ret (System.Char.ToLower x))
        | IntToChar a -> arithEval a >>= (fun x -> ret (char(x)))

    let isVowel c =
        match System.Char.ToLower c with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> true
        | _ -> false

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a,b) -> arithEval a >>= (fun x -> arithEval b >>= (fun y -> ret (x = y)))
        | ALt (a,b) -> arithEval a >>= (fun x -> arithEval b >>= (fun y -> ret (x < y)))
        | Not b -> boolEval b >>= (fun x -> ret (not x))
        | Conj (a,b) -> boolEval a >>= (fun x -> boolEval b >>= (fun y -> ret(x && y)))
        | IsVowel c -> charEval c >>= (fun x -> ret (isVowel x))
        | IsConsonant c -> charEval c >>= (fun x -> ret (not (isVowel x)))


    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare var -> declare var
        | Ass (var,value) -> arithEval value >>= (fun x -> update var x)
        | Skip -> ret ()
        | Seq (s1,s2) -> stmntEval s1 >>>= stmntEval s2
        | ITE (b,s1,s2) -> boolEval b >>= (fun x -> if x then stmntEval s1 else stmntEval s2)
        | While (b,s) -> boolEval b >>= (fun x -> if x then stmntEval s >>>= stmntEval(While(b,s)) else stmntEval(Skip))


    let getResult (stmnt: stm) (x: int) (y: int) = 
        stmntEval stmnt >>>= lookup "_result_" |>
        evalSM (mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"])


    let stmntToSquareFun (stm: stm) : squareFun =
        fun w pos acc ->
            match stmntEval stm >>>= lookup "_result_" |> evalSM (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"]) with
            | Success x -> x
            | Failure _ -> failwith "Something went terribly wrong :)"



module ImpParser =

    let getParserResult s (pr : ParserResult<'a, 'b>) =
        match pr with
        | Success (t, _, _)   -> 
            t
        | Failure (err, _, _) -> 
            let errorStr = sprintf "Failed to parse %s\n\nError:\n%A" s err
            failwith errorStr

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)
        
    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)

    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)                (* numeric inequality *)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)  (* numeric smaller than or equal to *)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)



    let pIntToChar = pstring "intToChar"
    let pPointValue = pstring "pointValue"
    let pCharToInt = pstring "charToInt"
    let pToUpper = pstring "toUpper"
    let pToLower = pstring "toLower"
    let pCharValue = pstring "charValue"
    let pTrue = pstring "true"
    let pFalse = pstring "false"
    let pif = pstring "if"
    let pthen = pstring "then"
    let pelse = pstring "else"
    let pwhile = pstring "while"
    let pdo = pstring "do"

    let pAnyChar = anyChar
    let letterChar = asciiLetter
    let alphaNumeric = asciiLetter <|> digit
    let charListToStr charList = String(List.toArray charList) |> string
    let pint = pint32
    let choice ps = ps |> Seq.map attempt |> choice

    let (<|>) p1 p2 = attempt p1 <|> attempt p2

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2

    let (>*>.) p1 p2 = p1 .>>. spaces >>. p2

    let (.>*>) p1 p2 = p1 .>> spaces .>> p2


    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let curlybracketise p = pchar '{' >*>. p .>*> pchar '}'

    let pid = (letterChar <|> pchar '_') .>>. many (alphaNumeric <|> pchar '_') |>> (fun x -> string (fst x) + (snd x |> List.toArray |> String))

    let unop op a = op .>*> spaces >*>. a
     
    let binop op a b = a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp, unit>()
    let ProdParse, pref = createParserForwardedToRef<aExp, unit>()
    let AtomParse, aref = createParserForwardedToRef<aExp, unit>()
    let CharParse, cref = createParserForwardedToRef<cExp, unit>()


    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    do pref := choice [DivParse; MulParse; ModParse; AtomParse]

    let NParse   = pint |>> N <?> "Int"
    let VParse   = pid |>> V <?> "Var"
    let NegParse = unop (pchar '-') pint |>> (fun x -> Mul (N -1, N x)) <?> "Neg"
    let PVParse = unop pPointValue (parenthesise TermParse) |>> PV <?> "PV"
    let CharToIntParse = unop pCharToInt CharParse |>> CharToInt <?> "CharToInt"
    let ParParse = parenthesise TermParse
    do aref := choice [CharToIntParse; PVParse; NegParse; NParse; VParse; ParParse]

    let AexpParse = TermParse 


    let CParse = between (pchar ''') pAnyChar (pchar ''') |>> C <?> "C"
    let CharValueParse = unop pCharValue TermParse |>> CV <?> "CharValue"
    let LowerParse = unop pToLower CharParse |>> ToLower <?> "ToLower"
    let UpperParse = unop pToUpper CharParse |>> ToUpper <?> "ToUpper"
    let IntToCharParse = unop pIntToChar TermParse |>> IntToChar <?> "IntToChar"
    let CParParse = parenthesise CharParse
    do cref := choice [CharValueParse; IntToCharParse; CParse; LowerParse; UpperParse; CParParse]

    let CexpParse = CharParse


    let BTermParse, btref = createParserForwardedToRef<bExp, unit>()
    let BProdParse, bpref = createParserForwardedToRef<bExp, unit>()
    let BAtomParse, baref = createParserForwardedToRef<bExp, unit>()

    let AndParse = binop (pstring @"/\") BProdParse BTermParse |>> Conj <?> "AndParse"
    let OrParse = binop (pstring @"\/") BProdParse BTermParse |>> (fun (x, y) -> Not(Conj(Not x, Not y))) <?> "OrParse"
    do btref := choice [AndParse; OrParse; BProdParse]

    let EqParse = binop (pchar '=') TermParse TermParse |>> AEq <?> "EqParse"
    let NotEqParse = binop (pstring "<>") TermParse TermParse |>> (fun (x, y) -> Not(AEq(x, y))) <?> "NotEqParse"
    let LtParse = binop (pstring "<") TermParse TermParse |>> ALt <?> "LtParse"
    let LteParse = binop (pstring "<=") TermParse TermParse |>> (fun (x, y) -> Not(Conj(Not(ALt(x, y)), Not(AEq(x, y))))) <?> "LteParse"
    let GtParse = binop (pstring ">") TermParse TermParse |>> (fun (x, y) -> Not(Not(Conj(Not(ALt(x, y)), Not(AEq(x, y)))))) <?> "GtParse"
    let GteParse = binop (pstring ">=") TermParse TermParse |>> (fun (x, y) -> Not(ALt(x, y))) <?> "GteParse"
    do bpref := choice [EqParse; NotEqParse; LtParse; LteParse; GtParse; GteParse; BAtomParse]

    let TrueParse = pTrue |>> (fun _ -> TT) <?> "TrueParse"
    let FalseParse = pFalse |>> (fun _ -> FF) <?> "FalseParse"
    let NotParse = unop (pchar '~') BTermParse |>> Not <?> "NotParse"
    let BParParse = parenthesise BTermParse
    do baref := choice [NotParse; TrueParse; FalseParse; BParParse]

    let BexpParse = BTermParse


    let STermParse, stref = createParserForwardedToRef<stm, unit>()
    let SProdStmParse, spref = createParserForwardedToRef<stm, unit>()
    
    let SeqParse = SProdStmParse .>*> (pstring ";") .>*>. STermParse |>> Seq <?> "Sequential"
    do stref := choice [SeqParse; SProdStmParse]

    let VarParse = binop (pstring ":=") pid AexpParse |>> Ass <?> "Ass"
    let DeclareParse = pstring "declare" >*>. pid |>> Declare <?> "Declare"
    let ITEParse = pif >*>. parenthesise BexpParse .>*> pthen .>*>. curlybracketise STermParse .>*> pelse .>*>. curlybracketise STermParse |>> (fun ((x, y), z) -> ITE(x, y, z)) <?> "ITE"
    let IfParse = pif >*>. parenthesise BexpParse .>*> pthen .>*>. curlybracketise STermParse |>> (fun (x, y) -> ITE(x, y, Skip)) <?> "If"
    let WhileParse = pwhile >*>. parenthesise BexpParse .>*> pdo .>*>. curlybracketise STermParse |>> (fun (x, y) -> While(x, y)) <?> "While"
    do spref := choice [VarParse; DeclareParse; ITEParse; IfParse; WhileParse]

    let stmParse = STermParse

    let runTextParser f s =
        match runParserOnString f () "" s with
        | Success (k, _, _) -> k
        | Failure (err, _, _) -> failwith ""
