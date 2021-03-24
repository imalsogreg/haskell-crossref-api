{ pkgs ? import <nixpkgs> {} }:

with pkgs;


pkgs.mkShell {
  inputsFrom = [ (import ./. { inherit pkgs; }).crossref.env ];
  buildDepends = [
    pkgs.stylish-haskell
  ];
}
