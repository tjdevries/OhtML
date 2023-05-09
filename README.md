# Ohtml

Ohtml (oatmeal) is a project focused on making the web fun, safe, and fast.

## Goals

Roughly speaking (copied from a recent tweet of mine):

I'm exploring building a framework around htmx inside of ocaml, with some goals roughly approximating:
- Generate (via ppx) all the CRUD operations on your data types
- Generate all the CRUD routes on your data types
- Typesafe htmx usage within TyXML (so you can generate your forms with complete typesafety from DB  to generated HTML/Routes).
- Shouldn't have to write any javascript (but phase two will hopefully include Melange compilation, so you can write your JS in ocaml)
- Builtin stuff with Dream, cause it's a cool webserver (so, easy integration with Dream's handlers, forms, sessions, etc)

In some ways, you could imagine it as typesafe, fast rails with typechecked templates (but the templates are actually just functions, since it's ocaml)

## Installation (WIP)

You'll need to have:

```bash
# First, you'll need opam. Install that based on ocaml.org recommendations

$ git clone https://github.com/tjdevries/ohtml
$ cd ohtml

# Create a new opam environment to run for this project
$ opam switch create . 5.0.0

# Install opam monorepo, which we use to build the project
$ opam install opam-monorepo

# (Temporary)
$ opam pin add dune.3.8 --dev-repo

# Pull the deps and install
$ opam monorepo pull

# Run it!
$ dune exec ohtml
```
