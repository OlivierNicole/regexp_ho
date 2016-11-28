module type Regex = sig
  type t
  type chr
  type str
  macro matches : t -> str -> bool expr
  static zero : t
  static one : t
  macro char : character -> t
  macro plus : t -> t -> t
  macro (+.+) : t -> t -> t
  macro disjunction : t list -> t
  macro maybe : t -> t
  macro cat : t -> t -> t
  macro ( *.* ) : t -> t -> t
  macro concat : t list -> t
end
