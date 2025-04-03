{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  pythonEnv = python3.withPackages (ps: with ps; [
    jupyter
    notebook
  ]);
  
  # Create a Haskell environment with IHaskell and its dependencies
  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    ihaskell
  ]);
in
mkShell {
  buildInputs = [
    pythonEnv
    haskellEnv
    zeromq
    pkg-config
    # These are still needed for development
    cabal-install
    stack
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${lib.makeLibraryPath [zeromq]}:$LD_LIBRARY_PATH
    # Install IHaskell kernel if it's not already installed
    if ! jupyter kernelspec list | grep -q "ihaskell"; then
      ihaskell install
    fi
  '';
}
