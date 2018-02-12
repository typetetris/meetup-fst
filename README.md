# meetup-fst
meetup command line app to manage events as an organisator -- early wip -- not usuable

## strange compilation times / reproduction manual

Resolved. It was optimisation. `cabal` defaults to something different then `-O0`.
Introduced a `release` flag to have fast compilation for development.
