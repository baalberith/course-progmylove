% I**** K***
% zadanie 2 wersja 2
% interpreter Funola


% --- gramatyka ------------------------------------------------------------- %

% program --> expression
% expression --> "if" expression "then" expression "else" expression
% expression --> "let" identifier "=" expression ";" expression
% expression --> "\" identifier "->" expression
% expression --> boolExpression
% boolExpression --> orExpression
% orExpression --> orExpression "or" andExpression
% orExpression --> andExpression
% andExpression --> andExpression "and" simpleExpression
% andExpression --> simpleExpression
% simpleExpression --> arthmeticExpression listOperator listExpression
% simpleExpression --> arthmeticExpression relOperator arthmeticExpression
% simpleExpression --> arthmeticExpression
% listExpression --> arthmeticExpression listOperator listExpression
% listExpression --> arthmeticExpression
% arthmeticExpression --> arthmeticExpression addOperator minusSummand
% arthmeticExpression --> minusSummand
% minusSummand --> "-" summand
% minusSummand --> summand
% summand --> summand mulOperator factor
% summand --> factor
% factor --> application
% application --> application atomExpression
% application --> atomExpression
% atomExpression --> "(" expression ")"
% atomExpression --> prefixOperator
% atomExpression --> number
% atomExpression --> identifier
% atomExpression --> "[" expressionsSequence "]"
% atomExpression --> "[]"
% expressionsSequence --> expression "," expressionsSequence
% expressionsSequence --> expression

% relOperator --> "=" | "/=" | "<" | "<=" | ">" | ">="
% listOperator --> ":" | "++"
% addOperator --> "+" | "-"
% mulOperator --> "*" | "div" | "mod"
% prefixOperator --> "not" | "head" | "tail" | "null"

% identifier -->  identifier specialLetter
% identifier -->  letter
% number --> number digit
% number --> digit
% specialLetter --> letter | digit | "_" | "'"
% digit --> "0" | ... | "9"
% letter --> "A" | ... | "Z" | "a" | ... | "z"


% --- lekser ---------------------------------------------------------------- %

whiteSpace -->
    "--", !,
    comment.
whiteSpace -->
    [Space], 
    { code_type(Space, space) }, !, 
    whiteSpace.
whiteSpace -->
    [].

comment -->
    "\n", !,
    whiteSpace.
comment --> 
    [_],
    comment.


digit(Digit) -->
    [Digit], 
    { code_type(Digit, digit) }.

digits([Digit|Digits]) -->
    digit(Digit), !,
    digits(Digits).
digits([]) -->
    [].

number(Digit, Number) -->
    digits(Digits),
    { number_chars(Number, [Digit|Digits]) }.


letter(Letter) -->
    [Letter],
    { code_type(Letter, alpha) }.

specialLetter(Letter) -->
    [Letter],
    { code_type(Letter, alnum);
    Letter = 0'_; Letter = 0'' }.

specialLetters([Letter|Letters]) -->
    specialLetter(Letter), !,
    specialLetters(Letters).
specialLetters([]) -->
    [].

identifier(Letter, Identifier) -->
    specialLetters(Letters),
    { atom_chars(Identifier, [Letter|Letters]) }.


token(tokLambda) --> "\\", !.
token(tokArrow) --> "->", !.
token(tokLParen) --> "(", !.
token(tokRParen) --> ")", !.
token(tokLSquare) --> "[", !.
token(tokRSquare) --> "]", !.
token(tokComma) --> ",", !.
token(tokSColon) --> ";", !.
token(tokCons) --> ":", !.
token(tokAppend) --> "++", !.
token(tokLtEq) --> "<=", !.
token(tokLt) --> "<", !.
token(tokGtEq) --> ">=", !.
token(tokGt) --> ">", !.
token(tokEq) --> "=", !.
token(tokNotEq) --> "/=", !.
token(tokPlus) --> "+", !.
token(tokMinus) --> "-", !.
token(tokMul) --> "*", !.

token(tokNum(Number)) --> 
    digit(Digit), !,
    number(Digit, Number).
token(Token) -->
    letter(Letter), !,
    identifier(Letter, Identifier),
    { member(
        (Identifier, Token), 
          [ (let, tokLet)
          , (if, tokIf)
          , (then, tokThen)
          , (else, tokElse)
          , (and, tokAnd)
          , (or, tokOr)
          , (not, tokNot)
          , (div, tokDiv)
          , (mod, tokMod)
          , (head, tokHead)
          , (tail, tokTail)
          , (null, tokNull)]
        ) -> true; 
      Token = tokVar(Identifier)
    }.


lexer([Token|Tokens]) -->
    whiteSpace,
    token(Token), !,
    lexer(Tokens).  
lexer([]) -->
    whiteSpace,
    [].


% --- parser ---------------------------------------------------------------- %

program(Prog) -->
    expression(Expr),
    { Prog = Expr }.


expression(if(Pred, ExprT, ExprF)) -->
    [tokIf], !,
    expression(Pred),
    [tokThen],
    expression(ExprT),
    [tokElse],
    expression(ExprF).

expression(let(Var, Expr1, Expr2)) -->
    [tokLet], !,
    [tokVar(Var)],
    [tokEq],
    expression(Expr1),
    [tokSColon],
    expression(Expr2).

expression(lambda(Var, Expr)) -->
    [tokLambda], !,
    [tokVar(Var)],
    [tokArrow],
    expression(Expr).

expression(Expr) -->
    boolExpression(BoolExpr),
    { Expr = BoolExpr }.


boolExpression(BoolExpr) -->
    orExpression(OrExpr),
    { BoolExpr = OrExpr }.


orExpression(OrExpr) -->
    andExpression(AndExpr), !,
    orExpression(AndExpr, OrExpr).

orExpression(AndAcc, OrExpr) -->
    [tokOr], !,
    andExpression(AndExpr),
    orExpression(or(AndAcc, AndExpr), OrExpr).
orExpression(Acc, Acc) -->
    [].


andExpression(AndExpr) -->
    simpleExpression(SimpExpr), 
    andExpression(SimpExpr, AndExpr).

andExpression(SimpAcc, AndExpr) -->
    [tokAnd], !, 
    simpleExpression(SimpExpr),
    andExpression(and(SimpAcc, SimpExpr), AndExpr).
andExpression(Acc, Acc) -->
    [].


simpleExpression(SimpExpr) -->
    arthmeticExpression(ArthExpr),
    listOperator(Op), !, 
    listExpression(ListExpr),
    { SimpExpr =.. [Op, ArthExpr, ListExpr] }.

simpleExpression(SimpExpr) -->
    arthmeticExpression(Expr1), 
    relOperator(Op), !, 
    arthmeticExpression(Expr2),
    { SimpExpr =.. [Op, Expr1, Expr2] }.

simpleExpression(SimpExpr) -->
    arthmeticExpression(ArthExpr),
    { SimpExpr = ArthExpr }.


listExpression(ListExpr) -->
    arthmeticExpression(ArthExpr),
    listOperator(Op), !, 
    listExpression(List),
    { ListExpr =.. [Op, ArthExpr, List] }.

listExpression(ListExpr) -->
    arthmeticExpression(ArthExpr),
    { ListExpr = ArthExpr }.


arthmeticExpression(ArthExpr) -->
    minusSummand(Summand), 
    arthmeticExpression(Summand, ArthExpr).

arthmeticExpression(SummandAcc, ArthExpr) -->
    addOperator(Op), !, 
    minusSummand(Summand),
    { Acc =.. [Op, SummandAcc, Summand] }, 
    arthmeticExpression(Acc, ArthExpr).
arthmeticExpression(Acc, Acc) -->
    [].


minusSummand(const(-1) * Summand) -->
    [tokMinus], !,
    summand(Summand).
minusSummand(Summand) -->
    summand(Summand).

summand(Summand) -->
    factor(Factor), 
    summand(Factor, Summand).

summand(FacAcc, Summand) -->
    mulOperator(Op), !, 
    factor(Factor),
    { Acc =.. [Op, FacAcc, Factor] }, 
    summand(Acc, Summand).
summand(Acc, Acc) -->
    [].


factor(Factor) -->
    application(App), 
    { Factor = App }.


application(App) -->
    atomExpression(AtomExpr),
    application(AtomExpr, App).

application(AtomAcc, App) -->
    atomExpression(AtomExpr), !,
    application(apply(AtomAcc, AtomExpr), App).
application(Acc, Acc) -->
    [].


atomExpression(AtomExpr) -->
    [tokLParen], !, 
    expression(Expr),
    { AtomExpr = Expr },
    [tokRParen].

atomExpression(AtomExpr) -->
    prefixOperator(Op), !,
    { AtomExpr = Op }.

atomExpression(AtomExpr) -->
    num(Num), !,
    { AtomExpr = Num }.

atomExpression(AtomExpr) -->
    ident(Ident), !,
    { AtomExpr = Ident }.

atomExpression(AtomExpr) -->
    [tokLSquare],
    expressionsSequence(ExprSeq), !,
    { AtomExpr = ExprSeq },
    [tokRSquare].

atomExpression([]) -->
    [tokLSquare],
    [tokRSquare].


expressionsSequence(Expr : Seq) -->
    expression(Expr),
    [tokComma], !, 
    expressionsSequence(Seq).

expressionsSequence(Expr : []) -->
    expression(Expr).
    

num(const(Num)) -->
    [tokNum(Num)].

ident(id(Ident)) -->
    [tokVar(Ident)].


:- op(400, yfx, div).
:- op(700, xfy, ++).
:- op(700, xfy, :).
:- op(700, xfx, <=).
:- op(700, xfx, /=).
:- op(1000, yfx, and).
:- op(1100, yfx, or).


relOperator(=) -->
    [tokEq], !.
relOperator(/=) -->
    [tokNotEq], !.
relOperator(<) -->
    [tokLt], !.
relOperator(<=) -->
    [tokLtEq], !.
relOperator(>) -->
    [tokGt], !.
relOperator(>=) -->
    [tokGtEq].


listOperator(:) -->
    [tokCons], !.
listOperator(++) -->
    [tokAppend].


addOperator(+) -->
    [tokPlus], !.
addOperator(-) -->
    [tokMinus].


mulOperator(*) -->
    [tokMul], !.
mulOperator(div) -->
    [tokDiv], !.
mulOperator(mod) -->
    [tokMod].


prefixOperator(not) -->
    [tokNot], !.
prefixOperator(head) -->
    [tokHead], !.
prefixOperator(tail) -->
    [tokTail], !.
prefixOperator(null) -->
    [tokNull].


% --- ewaluator ------------------------------------------------------------- %

% lookup pobiera z pamięci Mem wyrażenie Expr o nazwie Id

lookup(Id, Mem, Expr) :-
    member((Id, Expr), Mem).

% update dodaje do pamięci wyrażenie Expr związane z nazwą Id

update([], Id, Expr, [(Id, Expr)]) :- !.
update([(Id, _)|T], Id, Expr, [(Id, Expr)|T]) :- !.
update([H|T1], Id, Expr, [H|T2]) :-
    update(T1, Id, Expr, T2).


% ewaluuje wyrażenie Expr w pamięci Mem do wartości Val sprawdzając 
% jednocześnie, czy podany typ Type jest zgodny jej typem

evalTyped(Type, Expr, Mem, Val) :- 
    eval(Expr, Mem, value(Type, Val)), !.
evalTyped(Type, _, _, _) :- 
    print('Błąd wykonania: Niezgodność typów. Oczekiwano '), 
    print(Type), print('.'), nl, fail.


% liczba i zmienna

eval(const(Num), _, value(int, Num)) :- !.
eval(id(Id), Mem, Val) :- !,
    lookup(Id, Mem, Expr), 
    eval(Expr, Mem, Val).


% policzona już wartość (w argumentach funkcji)

eval(evaluated(Val), _, Val) :- !.


% operatory not, null, head, tail

eval(not, Mem, value(fun, Val)) :- !,
    Val = function(arg, Mem, apply(not, id(arg))).
eval(null, Mem, value(fun, Val)) :- !,
    Val = function(arg, Mem, apply(null, id(arg))).
eval(head, Mem, value(fun, Val)) :- !,
    Val = function(arg, Mem, apply(head, id(arg))).
eval(tail, Mem, value(fun, Val)) :- !,
    Val = function(arg, Mem, apply(tail, id(arg))).


% applikacja operatorów not, null, head, tail

eval(apply(not, Pred), Mem, value(bool, Val)) :- !,
    evalTyped(bool, Pred, Mem, PredVal),
    (PredVal == true -> 
        Val = false;
        Val = true
    ).
eval(apply(null, List), Mem, value(bool, Val)) :- !,
    evalTyped(list, List, Mem, ListVal),
    (ListVal = [] -> 
        Val = true; 
        Val = false
    ).
eval(apply(head, List), Mem, Val) :- !,
    evalTyped(list, List, Mem, ListVal),
    (ListVal = [Val|_] -> 
        true;
        print('Błąd wykonania: Lista pusta.'), nl, fail
    ).
eval(apply(tail, List), Mem, value(list, Val)) :- !,
    evalTyped(list, List, Mem, ListVal),
    (ListVal = [_|Val] ->
        true;
        print('Błąd wykonania: Lista pusta.'), nl, fail
    ).


% wyrażenia listowe

eval([], _, value(list, [])) :- !.
eval(H : T, Mem, value(list, [HVal|TVal])) :- !,
    eval(H, Mem, HVal), 
    evalTyped(list, T, Mem, TVal).
eval(L1 ++ L2, Mem, value(list, Val)) :- !,
    evalTyped(list, L1, Mem, L1Val), 
    evalTyped(list, L2, Mem, L2Val), 
    append(L1Val, L2Val, Val).


% wyrażenia arytmetyczne

eval(E1 div E2, Mem, value(int, Val)) :- !,
    evalTyped(int, E2, Mem, N2),
    (N2 == 0 ->
        print('Błąd wykonania: Dzielenie przez zero.'), nl, fail;
        evalTyped(int, E1, Mem, N1),
        Val is N1 // N2
    ).
eval(E1 mod E2, Mem, value(int, Val)) :- !,
    evalTyped(int, E2, Mem, N2),
    (N2 == 0 ->
        print('Błąd wykonania: Dzielenie przez zero.'), nl, fail;
        evalTyped(int, E1, Mem, N1),
        Val is N1 mod N2
    ).
eval(ArthExpr, Mem, value(int, Val)) :-
    ArthExpr =.. [Op, E1, E2],
    member(Op, [+, -, *]), !,
    evalTyped(int, E1, Mem, N1),
    evalTyped(int, E2, Mem, N2),
    Expr =.. [Op, N1, N2],
    Val is Expr.


% wyrażenia logiczne

eval(E1 = E2, Mem, value(bool, Val)) :- !,
    evalTyped(int, E1, Mem, N1),
    evalTyped(int, E2, Mem, N2),
    (N1 == N2 ->
        Val = true;
        Val = false
    ).    
eval(E1 /= E2, Mem, value(bool, Val)) :- !,
    evalTyped(int, E1, Mem, N1),
    evalTyped(int, E2, Mem, N2),
    (N1 \= N2 ->
        Val = true;
        Val = false
    ).    
eval(E1 <= E2, Mem, value(bool, Val)) :- !,
    evalTyped(int, E1, Mem, N1),
    evalTyped(int, E2, Mem, N2),
    (N1 =< N2 ->
        Val = true;
        Val = false
    ).
eval(BoolExpr, Mem, value(bool, Val)) :-
    BoolExpr =.. [Op, E1, E2],
    member(Op, [<, >, >=]), !,
    evalTyped(int, E1, Mem, N1),
    evalTyped(int, E2, Mem, N2),
    (call(Op, N1, N2) ->
        Val = true;
        Val = false
    ).

eval(E1 and E2, Mem, value(bool, Val)) :- !, 
    evalTyped(bool, E1, Mem, P1),
    evalTyped(bool, E2, Mem, P2),
    ((P1 == true, P2 == true) ->
        Val = true;
        Val = false
    ).
eval(E1 or E2, Mem, value(bool, Val)) :- !,
    evalTyped(bool, E1, Mem, P1),
    evalTyped(bool, E2, Mem, P2),
    ((P1 == true; P2 == true) ->
        Val = true;
        Val = false
    ).


% jeżeli predykat Pred jest spełniony, to obliczamy wartość ExprT, 
% w przeciwnym wypadku obliczamy wartość ExprF

eval(if(Pred, ExprT, ExprF), Mem, Val) :- !,
    evalTyped(bool, Pred, Mem, PredVal),
    (PredVal = true -> 
        eval(ExprT, Mem, Val); 
        eval(ExprF, Mem, Val)
    ).


% wyrażenie Expr1 związujemy ze zmienną Var, w tak zaktualizowanej
% pamięci obliczamy wartość Expr2

eval(let(Var, Expr1, Expr2), Mem, Val) :-
    update(Mem, Var, Expr1, Mem2),
    eval(Expr2, Mem2, Val), !.


% jako wartość wyrażenia zwracamy domknięcie funkcji o parametrze Var,
% ciele Body w kontekście Mem

eval(lambda(Var, Body), Mem, value(fun, Val)) :- !,
    Val = function(Var, Mem, Body).


% obliczmy funkcję do jej domknięcia, obliczamy wartość argumentu ArgVal,
% związujemy z parametrem Var wartość argumentu ArgVal,
% w tak zmienionym kontekście obliczmy wartość ciała Body

eval(apply(Fun, Arg), Mem, Val) :-
    (evalTyped(fun, Fun, Mem, function(Var, Context, Body)) -> 
        eval(Arg, Mem, ArgVal),
        update(Context, Var, evaluated(ArgVal), Context2),
        eval(Body, Context2, Val); 
        print('Błąd wykonania: Aplikacja nie do funkcji.'), nl, fail
    ), !.
    

% --- interpreter ----------------------------------------------------------- %

% printValue wypisuje obliczoną wartość

printValue(value(int, N)) :- !,
    print(N).
printValue(value(bool, B)) :- !,
    print(B).
printValue(value(fun, _)) :- !,
    print('<function>').
printValue(value(list, L)):-
    print('['),
    printList(L).

printList([]) :- !,
    print(']').
printList([H]) :- !,
    printValue(H), !, 
    printList([]).
printList([H|T]) :-
    printValue(H), 
    print(', '), 
    printList(T).


% parse tworzy listę tokenów, a potem abstrakcyjne drzewo rozbioru

parse(Program, Absynt) :-
    phrase(lexer(Tokens), Program),
    phrase(program(Absynt), Tokens).


% run parsuje, a potem ewaluuje program w początkowo pustej pamięci,
% pamięć jest tablicą asocjacyjną par (identyfikator, obliczone wyrażenie)

run(Program, Result) :-
    parse(Program, Absynt),
    Memory = [],
    eval(Absynt, Memory, Result).


loadFile(File, Program) :-
    open(File, read, In),
    get_char(In, Char),
    loadProgram(Char, In, Program),
    close(In).
 
loadProgram(end_of_file, _, []) :- !.
loadProgram(Char, In, [Code|Rest]) :-
    char_code(Char, Code),
    get_char(In, Char2),
    loadProgram(Char2, In, Rest).


% main wczytuje treść programu o podanej nazwie, wykonuje go 
% i wypisuje wynik lub komunikat o błędzie wykonania

main(File):-
    loadFile(File, Program),
    run(Program, Value),
    printValue(Value).
