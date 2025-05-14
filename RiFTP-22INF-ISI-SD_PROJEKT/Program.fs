open System

// Definicja typu danych dla wyrażeń algebraicznych
type Expr =
    | Const of float
    | Var of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Pow of Expr * float // Potęga
    | Sin of Expr // Sinus
    | Cos of Expr // Cosinus
    | Ln of Expr // Logarytm naturalny

// Funkcja do upraszczania wyrażeń algebraicznych
let rec simplify (expr: Expr) =
    match expr with
    | Add(Const a, Const b) -> Const(a + b)
    | Sub(Const a, Const b) -> Const(a - b)
    | Mul(Const a, Const b) -> Const(a * b)
    | Div(Const a, Const b) -> Const(a / b)
    | Add(a, Const 0.0) -> simplify a
    | Add(Const 0.0, a) -> simplify a
    | Sub(a, Const 0.0) -> simplify a
    | Mul(_, Const 0.0) -> Const 0.0
    | Mul(Const 0.0, _) -> Const 0.0
    | Mul(a, Const 1.0) -> simplify a
    | Mul(Const 1.0, a) -> simplify a
    | Div(a, Const 1.0) -> simplify a
    | Div(a, Const b) when b <> 0.0 -> simplify a
    | Pow(_, 0.0) -> Const 1.0
    | Pow(a, 1.0) -> simplify a
    | Sin(Const a) -> Const (sin a)
    | Cos(Const a) -> Const (cos a)
    | Ln(Const a) when a > 0.0 -> Const (log a)
    | _ -> expr

// Funkcja do wykonania operacji algebraicznych
let rec evaluate (expr: Expr) (x: float) =
    match expr with
    | Const c -> c
    | Var v -> x // Wartość zmiennej x
    | Add(a, b) -> evaluate a x + evaluate b x
    | Sub(a, b) -> evaluate a x - evaluate b x
    | Mul(a, b) -> evaluate a x * evaluate b x
    | Div(a, b) -> evaluate a x / evaluate b x
    | Pow(a, b) -> evaluate a x ** b
    | Sin(a) -> sin (evaluate a x)
    | Cos(a) -> cos (evaluate a x)
    | Ln(a) -> log (evaluate a x)

// Funkcja do obliczania pochodnej symbolicznej
let rec derivative (expr: Expr) (variable: string) =
    match expr with
    | Const _ -> Const 0.0
    | Var v when v = variable -> Const 1.0
    | Var _ -> Const 0.0
    | Add(a, b) -> Add(derivative a variable, derivative b variable)
    | Sub(a, b) -> Sub(derivative a variable, derivative b variable)
    | Mul(a, b) -> Add(Mul(derivative a variable, b), Mul(a, derivative b variable))
    | Div(a, b) -> Div(Sub(Mul(derivative a variable, b), Mul(a, derivative b variable)), Pow(b, 2.0))
    | Pow(a, b) -> Mul(Const b, Mul(Pow(a, b - 1.0), derivative a variable))
    | Sin(a) -> Mul(Cos(a), derivative a variable)
    | Cos(a) -> Mul(Const (-1.0), Mul(Sin(a), derivative a variable))
    | Ln(a) -> Div(derivative a variable, a)

// Funkcja do konwersji wyrażenia na czytelny format tekstowy
let rec exprToString (expr: Expr) =
    match expr with
    | Var v -> v
    | Const c -> sprintf "%f" c
    | Add(a, b) -> sprintf "%s + %s" (exprToString a) (exprToString b)
    | Sub(a, b) -> sprintf "%s - %s" (exprToString a) (exprToString b)
    | Mul(a, b) -> sprintf "%s * %s" (exprToString a) (exprToString b)
    | Div(a, b) -> sprintf "%s / %s" (exprToString a) (exprToString b)
    | Pow(a, b) -> sprintf "%s ^ %f" (exprToString a) b
    | Sin(a) -> sprintf "sin(%s)" (exprToString a)
    | Cos(a) -> sprintf "cos(%s)" (exprToString a)
    | Ln(a) -> sprintf "ln(%s)" (exprToString a)

// Funkcja do odczytywania wyrażenia od użytkownika
let rec readExpr (input: string) =
    let tokens = input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let rec parseTokens tokens =
        match tokens with
        | [|a; op; b|] ->
            match op with
            | "+" -> Add(parseToken a, parseToken b)
            | "-" -> Sub(parseToken a, parseToken b)
            | "*" -> Mul(parseToken a, parseToken b)
            | "/" -> Div(parseToken a, parseToken b)
            | "^" -> Pow(parseToken a, float b)
            | _ -> failwith "Nieznany operator"
        | _ -> failwith "Nieprawidłowe wyrażenie"
    and parseToken token =
        match token with
        | "x" -> Var "x"
        | _ -> Const (float token)
    parseTokens tokens

[<EntryPoint>]
let main argv =
    printfn "Wybierz operację: "
    printfn "1. Upraszczanie wyrażenia"
    printfn "2. Obliczanie wartości wyrażenia"
    printfn "3. Obliczanie pochodnej wyrażenia"
    let choice = Console.ReadLine()

    printfn "Podaj wyrażenie (przykład: 2 * x + 3):"
    let input = Console.ReadLine()
    let expr = readExpr input

    match choice with
    | "1" ->
        let simplifiedExpr = simplify expr
        printfn "Uproszczone wyrażenie: %s" (exprToString simplifiedExpr)
    | "2" ->
        printfn "Podaj wartość zmiennej x:"
        let xValue = float (Console.ReadLine())
        let result = evaluate expr xValue
        printfn "Wynik dla x = %f: %f" xValue result
    | "3" ->
        let derivativeExpr = derivative expr "x"
        printfn "Pochodna wyrażenia: %s" (exprToString derivativeExpr)
    | _ -> printfn "Nieznana operacja"

    0 // kod zakończenia programu
