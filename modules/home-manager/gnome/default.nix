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
    jeyj0.xfconf.enable = true;
    # themeing
    dconf.settings = {
      #"/org/gnome/desktop/wm/keybindings/switch-applications" = "";#[];
      #"/org/gnome/desktop/wm/keybindings/switch-windows" = "<Alt>Tab";#["<Alt>Tab"];
      #"/org/gnome/desktop/wm/keybindings/switch-windows-backward" = "<Shift><Alt>Tab";#["<Shift><Alt>Tab"];
      "org/gnome/desktop/wm/keybindings" = {
        switch-applications = [];
        switch-windows = ["<Alt>Tab"];
        switch-windows-backward = ["<Shift><Alt>Tab"];
      };

      "org/gnome/shell" = {
        disable-user-extensions = false;

        enabled-extensions = [
          "user-theme@gnome-shell-extensions.gcampax.github.com"
          "launch-new-instance@gnome-shell-extensions.gcampax.github.com"
        ];
      };
      "org/gnome/shell/extensions/user-theme" = {
        name = "Tokyonight-Moon-BL";
      };
    };
    home.packages = with pkgs.unstable; [
      gnomeExtensions.user-themes
      palenight-theme
      tokyo-night-gtk
    ];
  };
}
