type var = string Source.phrase

type symbolic = symbolic' Source.phrase
and symbolic' = SymbolicVar of Types.value_type * string

type definition = definition' Source.phrase
and definition' =
  | Textual of Ast.module_
  | Encoded of string * string
  | Quoted of string * string

type action = action' Source.phrase
and action' =
  | Invoke of var option * Ast.name * Ast.literal list
  | Get of var option * Ast.name

type symbolicaction = symbolicaction' Source.phrase
and symbolicaction' =
  | SymbolicInvoke of var option * Ast.name * symbolic list

type assertion = assertion' Source.phrase
and assertion' =
  | AssertMalformed of definition * string
  | AssertInvalid of definition * string
  | AssertUnlinkable of definition * string
  | AssertUninstantiable of definition * string
  | AssertReturn of action * Ast.literal list
  | AssertReturnCanonicalNaN of action
  | AssertReturnArithmeticNaN of action
  | AssertTrap of action * string
  | AssertExhaustion of action * string

type symbolicassertion = symbolicassertion' Source.phrase
and symbolicassertion' =
  | SymbolicAssertReturn of symbolicaction * Ast.literal list

type command = command' Source.phrase
and command' =
  | Module of var option * definition
  | Register of Ast.name * var option
  | Action of action
  | Assertion of assertion
  | SymbolicAssertion of symbolicassertion
  | Meta of meta

and meta = meta' Source.phrase
and meta' =
  | Input of var option * string
  | Output of var option * string option
  | Script of var option * script

and script = command list

exception Syntax of Source.region * string
