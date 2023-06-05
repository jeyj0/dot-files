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
      # use `dconf watch /` and change what you want to save to figure out what to set here

      #"/org/gnome/desktop/wm/keybindings/switch-applications" = "";#[];
      #"/org/gnome/desktop/wm/keybindings/switch-windows" = "<Alt>Tab";#["<Alt>Tab"];
      #"/org/gnome/desktop/wm/keybindings/switch-windows-backward" = "<Shift><Alt>Tab";#["<Shift><Alt>Tab"];
      "org/gnome/desktop/wm/keybindings" = {
        switch-applications = [];
        switch-windows = ["<Alt>Tab"];
        switch-windows-backward = ["<Shift><Alt>Tab"];
      };

      # enable right-alt/alt-gr as compose key
      "org/gnome/desktop/input-sources".xkb-options = ["terminate:ctrl_alt_bksp" "compose:ralt"];

      "org/gnome/desktop/interface".color-scheme = "prefer-dark";

      "org/gnome/desktop/notifications".show-in-lock-screen = false;

      # turn off automatic screen brightness
      "org/gnome/settings-daeom/plugins/power".ambient-enabled = false;

      "org/gnome/desktop/peripherals/touchpad" = {
        speed = 0.33333333333333326;
        tap-to-click = true;
      };

      # alt-tab only switches windows on current workspace
      "org/gnome/shell/app-switcher".current-workspace-only = true;

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
      tokyo-night-gtk
    ];
  };
}
