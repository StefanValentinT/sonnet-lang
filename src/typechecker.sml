structure TypeChecker =
struct

  datatype typ =
    Bool
  | Fun of typ * typ
  | Record of (string * typ) list
  | Bottom
  | Top

end
