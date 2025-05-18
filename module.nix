{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.artiflakery;

  routesFile = pkgs.writeText "artiflakery-routes.txt" (
    concatStringsSep "\n" (
      mapAttrsToList (
        routePath: routeOpts: "${routePath} ${routeOpts.flakeref} ${concatStringsSep " " routeOpts.access}"
      ) cfg.routes
    )
    + "\n"
  );

in
{
  options.services.artiflakery = {
    enable = mkEnableOption "Artiflakery service";

    package = mkOption {
      type = types.package;
      default = pkgs.artiflakery;
      description = "The artiflakery package to use.";
    };

    authFile = mkOption {
      type = types.path;
      description = ''
        Path to the authentication file containing secrets.
        This file must be created manually by the user.
      '';
      example = "/var/lib/artiflakery/auth.txt";
    };

    routes = mkOption {
      type = types.attrsOf (
        types.submodule {
          options = {
            flakeref = mkOption {
              type = types.str;
              description = "Complete flake reference URL";
              example = "git+ssh://git@github.com/foo/bar";
            };

            access = mkOption {
              type = types.listOf types.str;
              description = "Access levels for this route (can include multiple)";
              example = [
                "admins"
                "friends"
              ];
            };
          };
        }
      );
      default = { };
      description = "Routes configuration";
      example = literalExpression ''
        {
          "hello/world/" = {
            flakeref = "git+ssh://git@github.com/hello/world";
            access = ["admins" "public"];
          };
        }
      '';
    };
  };

  config = mkIf cfg.enable {
    users.users = {
      artiflakery = {
        isSystemUser = true;
        group = "artiflakery";
        description = "ArtifLakery service user";
        home = "/var/lib/artiflakery";
        createHome = true;
      };
    };

    users.groups.artiflakery = { };

    nix.settings.allowed-users = [ "artiflakery" ];

    systemd.services.artiflakery = {
      description = "ArtifLakery Service";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      path = [ pkgs.nix ];

      serviceConfig = {
        User = "artiflakery";
        Group = "artiflakery";
        ExecStart = "${cfg.package}/bin/artiflakery-exe --auth ${cfg.authFile} --routes ${routesFile}";
        Restart = "on-failure";
        RestartSec = "5s";

        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        ProtectHostname = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        NoNewPrivileges = true;
        WorkingDirectory = "/var/lib/artiflakery";

        ReadOnlyPaths = "${cfg.authFile}";
      };
    };
  };
}
