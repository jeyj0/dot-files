{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.gnome = {
    enable = mkEnableOption "gnome";
  };

  config = mkIf config.jeyj0.gnome.enable {
    # Enable the GNOME Desktop Environment.
    services.xserver.displayManager.gdm.enable = true;
    services.xserver.desktopManager.gnome.enable = true;

    # themeing
    dconf.settings = {
      "/org/gnome/desktop/wm/keybindings/switch-applications" = [];
      "/org/gnome/desktop/wm/keybindings/switch-windows" = ["<Alt>Tab"];
      "/org/gnome/desktop/wm/keybindings/switch-windows-backward" = ["<Shift><Alt>Tab"];

      "org/gnome/shell" = {
        disable-user-extensions = false;

        enabled-extensions = [
          "user-theme@gnome-shell-extensions.gcampax.github.com"
        ];

        "org/gnome/shell/extensions/user-theme" = {
          name = "palenight";
        };
      };
    };
    home.packages = with pkgs; [
      gnome-extensions.user-themes
      palenight-theme
    ];
  };
}
