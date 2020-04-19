# default.nix
{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    reflex-basic-host = ./reflex-basic-host;
    potato-flow = ./potato_flow;
    todo-undo-mvc-model = ./reflex-todo-undo-mvc-model;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["todoundo" "frontend"];
    ghcjs = ["todoundo" "frontend"];
  };
})
