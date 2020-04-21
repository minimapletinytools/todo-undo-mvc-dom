# default.nix
{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    reflex-test-host = ./reflex-test-host;
    reflex-potatoes = ./reflex-potatoes;
    reflex-dynamic-containers = ./reflex-dynamic-containers;
    todo-undo-mvc-model = ./reflex-todo-undo-mvc-model;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["todoundo" "frontend"];
    ghcjs = ["todoundo" "frontend"];
  };
})
