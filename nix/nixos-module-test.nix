{ nixosTest
, foo-bar-nixos-module-factory
}:
let
  foo-bar-production = foo-bar-nixos-module-factory {
    envname = "production";
  };
  port = 8080;
in
nixosTest (
  { lib, pkgs, ... }: {
    name = "foo-bar-module-test";
    nodes = {
      server = {
        imports = [
          foo-bar-production
        ];
        services.foo-bar.production = {
          enable = true;
          web-server = {
            enable = true;
            inherit port;
            openFirewall = true;
          };
        };
      };
      client = { };
    };
    testScript = ''
      server.start()
      client.start()
      server.wait_for_unit("multi-user.target")
      client.wait_for_unit("multi-user.target")

      server.wait_for_open_port(${builtins.toString port})
      client.succeed("curl server:${builtins.toString port}")
    '';
  }
)
