# calculator-server
The second task of "Programming Paradigms" course (CMC MSU)

## Installation

Requires [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

    stack setup
    stack build
    stack exec calculator-server-exe

## Queries
- GET /add/\<x>/\<y>
- GET /sub/\<x>/\<y>
- GET /mul/\<x>/\<y>
- GET /div/\<x>/\<y>
- GET /sqrt/\<x>
- GET /pow/\<x>/\<n>

## Example
    curl localhost:8080/add/1.1/2.3
