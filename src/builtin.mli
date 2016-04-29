val funcs : Prim.t Core.Std.String.Map.t

val list_methods : (Prim.t Blist.t -> Prim.t list -> Prim.t) Core.Std.String.Map.t

val dict_methods : (Prim.t Bdict.t -> Prim.t list -> Prim.t) Core.Std.String.Map.t

val str_methods : (string -> Prim.t list -> Prim.t) Core.Std.String.Map.t
