# CLI Executable Design

## What we're building

A `bole` command-line tool that exposes the database API as git-style subcommands, with file-backed persistence in a `.bole/` directory. Native binary, no runtime dependencies, distributable cross-platform.

## Design decisions

- **Git-style `.bole/` directory:** HEAD file, refs/heads/ for branches, objects/ with 2-char prefix subdirectories for chunks. Familiar pattern, debuggable with standard tools.
- **Wrap in-memory Store with file persistence:** `Store.create ~path` adds file read/write behind the existing Hashtbl. No functors, no new module signature. In-memory Hashtbl acts as cache. Existing tests unchanged.
- **Keep Db pure, persistence in Repo module:** `Repo` handles loading/saving `.bole/` directory state. Db stays filesystem-unaware and testable.
- **cmdliner for CLI parsing:** Already in dependency tree (via alcotest), added as explicit non-test dependency. Gives us subcommands, help, completion.
- **Cross-platform via native OCaml:** No runtime deps. Use `Filename.concat` and `Sys.mkdir` for path handling. CI matrix for Linux/macOS/Windows.

## Directory layout

```
.bole/
  HEAD              # current branch name, e.g. "main\n"
  refs/
    heads/
      main          # commit hash hex + newline
      feature       # commit hash hex + newline
  objects/
    ab/
      cd1234...     # raw chunk bytes, named by hex hash
```

## Project structure changes

```
bin/
  dune              # (executable (name main) (public_name bole) (libraries bole cmdliner))
  main.ml           # CLI entry point
lib/
  repo.mli          # new — .bole/ directory ↔ Db.t bridge
  repo.ml           # new
  store.mli         # modified — create gains optional ~path
  store.ml          # modified — file-backed persistence
```

## Store changes

```ocaml
type t = {
  tbl : string Tbl.t;
  mutable gets : int;
  mutable puts : int;
  path : string option;  (* .bole/objects directory, or None for in-memory *)
}

val create : ?path:string -> unit -> t
```

When `path` is provided:
- `put`: write to Hashtbl AND file `<path>/<hex[0..1]>/<hex[2..]>`
- `get`: check Hashtbl, on miss read file, cache in Hashtbl
- `mem`: check Hashtbl, on miss check file exists

When `path` is None: pure in-memory, unchanged behavior.

## Repo module

```ocaml
val init : string -> unit
(* Create .bole/ directory structure at given path *)

val load : string -> Db.t
(* Load Db.t from .bole/ directory *)

val save : string -> Db.t -> unit
(* Save HEAD and branch refs to .bole/ directory *)
```

`Db` gains accessors for Repo to use: `current_branch`, `branches`, `of_parts`.

## CLI commands

```
bole init                              # create .bole/ in current directory
bole put <table> <key> <value>         # insert/update
bole get <table> <key>                 # lookup, print to stdout
bole delete <table> <key>              # remove
bole commit -m <message>               # snapshot
bole log                               # commit history
bole branch <name>                     # create and switch to branch
bole switch <name>                     # switch to existing branch
bole diff <commit1> <commit2> <table>  # show differences
bole merge <branch>                    # three-way merge into current
```

Each command: find `.bole/` → `Repo.load` → `Db` operation → `Repo.save` → print output.

Output format: plain text. `get` prints value. `diff` prints `+ key value`, `- key value`, `~ key old new`. `merge` prints conflicts. `log` prints hash and message.

## Distribution

- GitHub Actions CI: Linux, macOS, Windows matrix
- GitHub Releases: upload native binaries per platform per tag
- Later: Homebrew tap, opam package
