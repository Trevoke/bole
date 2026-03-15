(** Table schema: column definitions and primary key.

    Binary format:
    [column_count: 2B BE] then per column: [name_len: 2B BE] [name] [type: 1B]
    [pk_count: 2B BE] then per pk column: [name_len: 2B BE] [name]
    Type bytes: 0x01 = Int64, 0x02 = Str *)

type column_type = Int64 | Str

type t = {
  columns : (string * column_type) list;
  primary_key : string list;
}

val create : columns:(string * column_type) list -> primary_key:string list -> t
(** [create ~columns ~primary_key] creates a schema.
    @raise Invalid_argument if any primary key column is not in the column list. *)

val encode : t -> string
val decode : string -> t

val key_columns : t -> (string * column_type) list
(** Returns the primary key columns in declaration order. *)

val value_columns : t -> (string * column_type) list
(** Returns the non-primary-key columns in declaration order. *)
