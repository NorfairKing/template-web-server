{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
}:
let
  foo-bar-production = import (./nixos-module.nix) {
    envname = "production";
    fooBarPackages = pkgs.fooBarPackages;
  };
  port = 8080;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "foo-bar-module-test";
    machine = {
      imports = [
        foo-bar-production
      ];
      services.foo-bar.production = {
        enable = true;
        web-server = {
          enable = true;
          inherit port;
        };
      };
    };
    testScript = ''
      machine.start()
      machine.wait_for_unit("multi-user.target")

      machine.wait_for_open_port(${builtins.toString port})
    '';
  }
)
