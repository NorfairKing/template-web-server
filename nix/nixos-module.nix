{ envname, fooBarPackages ? (import ./pkgs.nix { }).fooBarPackages }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.foo-bar."${envname}";
  concatAttrs = attrList: fold (x: y: x // y) { } attrList;
in
{
  options.services.foo-bar."${envname}" =
    {
      enable = mkEnableOption "Foo/Bar Service";
      web-server =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Foo/Bar API Server";
                  log-level =
                    mkOption {
                      type = types.str;
                      example = "Debug";
                      default = "Warn";
                      description = "The log level to use";
                    };
                  hosts =
                    mkOption {
                      type = types.listOf (types.str);
                      example = "foo-bar.cs-syd.eu";
                      description = "The host to serve web requests on";
                    };
                  port =
                    mkOption {
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
      web-server-database-file = web-server-working-dir + "foo-bar-server-database.sqlite3";
      # The api server
      web-server-service =
        with cfg.web-server;
        optionalAttrs enable {
          "foo-bar-web-server-${envname}" = {
            description = "Foo/Bar API Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "FOO_BAR_API_SERVER_LOG_LEVEL" =
                  "${builtins.toString log-level}";
                "FOO_BAR_API_SERVER_PORT" =
                  "${builtins.toString port}";
                "FOO_BAR_API_SERVER_DATABASE" = web-server-database-file;
              };
            script =
              ''
                mkdir -p "${web-server-working-dir}"
                cd ${web-server-working-dir};
                ${fooBarPackages.foo-bar-web-server}/bin/foo-bar-web-server
              '';
            serviceConfig =
              {
                Restart = "always";
                RestartSec = 1;
                Nice = 15;
              };
            unitConfig =
              {
                StartLimitIntervalSec = 0;
                # ensure Restart=always is always honoured
              };
          };
        };
      web-server-host =
        with cfg.web-server;

        optionalAttrs (enable && hosts != [ ]) {
          "${head hosts}" =
            {
              enableACME = true;
              forceSSL = true;
              locations."/" = {
                proxyPass = "http://localhost:${builtins.toString port}";
                # Just to make sure we don't run into 413 errors on big syncs
                extraConfig = ''
                  client_max_body_size 0;
                '';
              };
              serverAliases = tail hosts;
            };
        };

      # Local backup
      local-backup-service =
        optionalAttrs (cfg.web-server.enable or false) (
          optionalAttrs (cfg.web-server.local-backup.enable or false) (
            with cfg.web-server.local-backup;
            {
              "foo-bar-web-server-local-backup-${envname}" = {
                description = "Backup foo-bar-web-server database locally for ${envname}";
                wantedBy = [ ];
                script =
                  ''
                    mkdir -p ${backup-dir}
                    file="${backup-dir}/''$(date +%F_%T).db"
                    ${pkgs.sqlite}/bin/sqlite3 ${web-server-database-file} ".backup ''${file}"
                  '';
                serviceConfig = {
                  WorkingDirectory = working-dir;
                  Type = "oneshot";
                };
              };
            }
          )
        );
      local-backup-timer =
        optionalAttrs (cfg.web-server.enable or false) (
          optionalAttrs (cfg.web-server.local-backup.enable or false) (
            with cfg.web-server.local-backup;
            {
              "foo-bar-web-server-local-backup-${envname}" = {
                description = "Backup foo-bar-web-server database locally for ${envname} every twelve hours.";
                wantedBy = [ "timers.target" ];
                timerConfig = {
                  OnCalendar = "00/12:00";
                  Persistent = true;
                };
              };
            }
          )
        );
    in
    mkIf cfg.enable {
      systemd.services =
        concatAttrs [
          web-server-service
          local-backup-service
        ];
      systemd.timers =
        concatAttrs [
          local-backup-timer
        ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional cfg.web-server.enable cfg.web-server.port)
      ];
      services.nginx.virtualHosts =
        concatAttrs [
          web-server-host
        ];
    };
}
