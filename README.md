#  Edn Reader for Scala
Simple [Extensible Data notation](https://github.com/edn-format/edn) reader for scala, implemented using [Parser Combinators](https://github.com/scala/scala-parser-combinators) and returning base Scala types:
- Lists
- Vectors
- Maps
- Sets
- Scala Symbols (for both clojure Symbol and Keyword)
- simple types
- support for EDN's [builtin tagged](https://github.com/edn-format/edn#built-in-tagged-elements) elements Instant (i.e. datetime) and UUIDs

# Examples

```
Edn> (+ 2 (* 2 2))
List('+, 2, List('*, 2, 2))

Edn>(fn [x] (+ x (add 2 )))
List('fn, Vector('x), List('+, 'x, List('add, 2)))

```
