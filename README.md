# meetup-fst
meetup command line app to manage events as an organisator -- early wip -- not usuable

## strange compilation times / reproduction manual

1. `stack init`
   
   If using nixos/nixpkgs: Be sure to include `--nix` on the command line, and add

       nix:
         packages: [ zlib ]
   
   to the resulting `stack.yaml`.

2. `stack build`

   If using nixos/nixpkgs: Be sure to include `--nix`.

   After all the dependencies are built and registered notice how in the final compilation/link step (whatever it is)
   it takes forever at `[3/4]` and eats a lot of ram (8 GiBi on my machine/nixos/ghc).

3. `stack exec -- ghc --make -i$(pwd)/src:$(pwd)/app -o test app/Main.hs`

   If using nixos/nixpkgs: Be sure to include `--nix`.

   Notice how it compiles all modules and does the link step in like no time and no ram.

If you know, what is going on, please explain it to me.
