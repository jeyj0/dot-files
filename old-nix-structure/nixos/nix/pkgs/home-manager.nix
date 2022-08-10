{ pkgs }:
{ user, userHome, hostName, home-manager-path, config-path }:
assert builtins.typeOf user == "string";
assert builtins.typeOf userHome == "string";
assert builtins.typeOf hostName == "string";
assert builtins.typeOf home-manager-path == "string";
assert builtins.typeOf config-path == "string";
    (
      pkgs.callPackage
        (/. + home-manager-path + "/home-manager") { path = "${home-manager-path}"; }
    ).overrideAttrs (old: {
      nativeBuildInputs = [ pkgs.makeWrapper ];
      buildCommand =
        let
          home-manager-bootstrap = pkgs.writeTextFile {
            name = "home-manager-bootstrap.nix";
            text = ''
              { config, pkgs, ... }:
              {
                # Home Manager needs a bit of information about you and the
                # paths it should manage.
                home.username = "${user}";
                home.homeDirectory = "${userHome}";
                home.sessionVariables.HOSTNAME = "${hostName}";
                imports = [ ${config-path} ];
              }
            '';
          }; in
        ''
          ${old.buildCommand}
          wrapProgram $out/bin/home-manager --set HOME_MANAGER_CONFIG "${home-manager-bootstrap}"
        '';
    })
