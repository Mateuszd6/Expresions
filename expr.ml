type expr =
  | CONST of float
  | VAR
  | NEGATE of expr
  | LOG of expr
  | SUM of expr * expr
  | SUBSTRACT of expr * expr
  | MULTIPLY of expr * expr
  | DIVIDE of expr * expr

let negate = function
  | CONST constant -> CONST(-. constant)
  | expr1 -> NEGATE(expr1)

let ln expr1 = LOG(expr1)

let add expr1 expr2 =
  match expr1, expr2 with
  | _, CONST 0. -> expr1
  | CONST 0., _ -> expr2
  | CONST val1, CONST val2 -> CONST (val1 +. val2)
  | _, _ -> SUM(expr1, expr2)

let substract expr1 expr2 =
  match expr1, expr2 with
  | _, CONST 0. -> expr1
  | CONST 0., _ -> NEGATE(expr2)
  | CONST(val1), CONST(val2) -> CONST(val1 -. val2)
  | _, _ -> SUBSTRACT(expr1, expr2)

let multiply expr1 expr2 =
  match expr1, expr2 with
  | _, CONST 0. -> CONST 0.
  | CONST 0., _ -> CONST 0.
  | _, CONST 1. -> expr1
  | CONST 1., _ -> expr2
  | CONST val1, CONST val2 -> CONST(val1 *. val2)
  | _, _ -> MULTIPLY(expr1, expr2)

let divide expr1 expr2 =
  match expr1, expr2 with
  | _, CONST 0. -> raise Division_by_zero
  | CONST 0., _ -> CONST 0.
  | _, CONST 1. -> expr1
  | CONST val1, CONST val2 -> CONST (val1 /. val2)
  | _, _ -> DIVIDE(expr1, expr2)

let rec derivative = function
  | CONST _ -> CONST 0.
  | VAR -> CONST 1.
  | NEGATE expr -> derivative expr |> negate
  | LOG expr -> divide (derivative expr) expr
  | SUM (expr1, expr2) -> add (derivative expr1) (derivative expr2)
  | SUBSTRACT (expr1, expr2) -> substract (derivative expr1)
                                  (derivative expr2)
  | MULTIPLY (expr1, expr2) -> add
                                 (multiply (derivative expr1) expr2)
                                 (multiply (derivative expr2) expr1)
  | DIVIDE (expr1, expr2) -> (divide (substract
                                        (multiply (derivative expr1) expr2)
                                        (multiply (derivative expr2) expr1))
                                (multiply expr2 expr2))

let rec getValue value = function
  | CONST constant -> constant
  | VAR -> value
  | NEGATE expr -> -. getValue value expr
  | LOG expr -> getValue value expr |> log
  | SUM (expr1, expr2) -> (getValue value expr1) +. (getValue value expr2)
  | SUBSTRACT (expr1, expr2) -> (getValue value expr1) -. (getValue value expr2)
  | MULTIPLY (expr1, expr2) -> (getValue value expr1) *. (getValue value expr2)
  | DIVIDE (expr1, expr2) -> (getValue value expr1) /. (getValue value expr2)

let rec string_of_expr  = function
  | CONST constant -> string_of_float constant
  | VAR -> "x"
  | NEGATE expr -> "-(" ^ (string_of_expr expr) ^ ")"
  | LOG expr -> "log(" ^ (string_of_expr expr)
  | SUM (expr1, expr2) -> "(" ^ (string_of_expr expr1) ^ " + " ^ (string_of_expr expr2) ^ ")"
  | SUBSTRACT (expr1, expr2) -> "(" ^ (string_of_expr expr1) ^ " - " ^ (string_of_expr expr2) ^ ")"
  | MULTIPLY (expr1, expr2) -> "(" ^ (string_of_expr expr1) ^ " * " ^ (string_of_expr expr2) ^ ")"
  | DIVIDE (expr1, expr2) -> "(" ^ (string_of_expr expr1) ^ " / " ^ (string_of_expr expr2) ^ ")"
