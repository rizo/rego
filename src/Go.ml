

type type' = Empty_interface

type identifier = string

type parameter = {
  identifiers: identifier list;
  ellipsis: bool;
  type': type'
}

type result =
  | Parameters of parameter list
  | Type of type'

type signature = {
  parameters: parameter list;
  return: result option
}

type rel_op = [ `eq | `not_eq | `lt | `le | `gt | `ge ]
type add_op = [ `sum | `difference | `bit_or | `bit_xor ]
type mul_op = [ `product | `quotient | `remainder | `left_shift | `right_shift | `bit_and | `bit_and_not ]
type unary_op = [ `sum | `difference | `not | `bit_xor | `product | `bit_and | `receive ]
type binary_op  = [ `or_ | `and_ | rel_op | add_op | mul_op ]

type primary_expression = [`xxx]

type unary_expression =
  | Primary of primary_expression
  | Unary_op of unary_op * unary_expression

type expression =
  | Unary of unary_expression
  | Binary of binary_op * expression * expression

type simple_statement =
  | Empty
  | Expression
  | Send
  | IncDec
  | Assignment
  | ShortVarDecl

type statement =
  | Declaration
  | Labeled
  | Simple of simple_statement
  | Go
  | Return of expression list
  | Break
  | Continue
  | Goto
  | Fallthrough
  | Block
  | If
  | Switch
  | Select
  | For
  | Defer

type block = statement list

type function_declaration = {
  name: identifier;
  signature: signature;
  body: block option
}

type method_declaration = {
  receiver: parameter list;
  name: identifier;
  signature: signature;
  body: block option
}

type declaration =
  | Const
  | Type
  | Var

type top_level_declaration =
  | Declaration of declaration
  | Function of function_declaration
  | Method of method_declaration


