{ foo-bar-web-server
}:
{ envname
}:
{ lib
, pkgs
, config
, ...
}:
with lib;

let
  cfg = config.services.foo-bar."${envname}";
  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };
in
{
  options.services.foo-bar."${envname}" = {
    enable = mkEnableOption "Foo/Bar Service";
    web-server = mkOption {
      type = types.submodule {
        options = {
          enable = mkEnableOption "Foo/Bar WEB Server";
          config = mkOption {
            default = { };
            description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
          };
          log-level = mkOption {
            type = types.str;
            example = "Debug";
            default = "Warn";
            description = "The log level to use";
          };
          hosts = mkOption {
            type = types.listOf (types.str);
            example = "foo-bar.cs-syd.eu";
            description = "The host to serve web requests on";
          };
          openFirewall = mkOption {
            type = types.bool;
            default = false;
            description = "Whether to open the specified ports in the firewall";
          };
          port = mkOption {
            type = types.int;
            example = 8001;
            description = "The port to serve web requests on";
          };
        };
      };
      default = null;
    };
  };
  config =
    let
      working-dir = "/www/foo-bar/${envname}/";
      # The docs server
      web-server-working-dir = working-dir + "web-server/";
      attrOrNull = name: value: optionalAttrs (!builtins.isNull value) { "${name}" = value; };
      web-server-config = with cfg.web-server; mergeListRecursively [
        (attrOrNull "port" port)
        (attrOrNull "log-level" log-level)
        cfg.web-server.config
      ];
      web-server-config-file = (pkgs.formats.yaml { }).generate "foo-bar-web-server-config" web-server-config;
      # The api server
      web-server-service =
        with cfg.web-server;
        optionalAttrs enable {
          "foo-bar-web-server-${envname}" = {
            description = "FooBar Web Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment = {
              "FOO_BAR_WEB_SERVER_CONFIG_FILE" = "${web-server-config-file}";
            };
            script = ''
              mkdir -p "${web-server-working-dir}"
              cd ${web-server-working-dir};
              ${foo-bar-web-server}/bin/foo-bar-web-server
            '';
            serviceConfig = {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
            unitConfig = {
              StartLimitIntervalSec = 0;
              # ensure Restart=always is always honoured
            };
          };
        };
      web-server-host =
        with cfg.web-server;

        optionalAttrs (enable && hosts != [ ]) {
          "${head hosts}" = {
            enableACME = true;
            forceSSL = true;
            locations."/".proxyPass = "http://localhost:${builtins.toString port}";
            serverAliases = tail hosts;
          };
        };
    in
    mkIf cfg.enable {
      systemd.services = mergeListRecursively [
        web-server-service
      ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional ((cfg.web-server.enable or false) && cfg.web-server.openFirewall) cfg.web-server.port)
      ];
      services.nginx.virtualHosts = mergeListRecursively [
        web-server-host
      ];
    };
}
