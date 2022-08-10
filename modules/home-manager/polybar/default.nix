{ config, lib, pkgs, ... }:
with lib;
{
  options.jeyj0.polybar = {
    enable = mkEnableOption "polybar";
  };

  config = mkIf config.jeyj0.polybar.enable {
    services.polybar = {
      enable = true;
      package = pkgs.unstable.polybar;
      script = "polybar desktop-left & polybar desktop-right &";
      settings = {
        "bar/base" = {
          height = 32;
          modules = {
            left = "start xmonad";
            right = "cpu ram disk date";
          };
          font = ["Hack Nerd Font"];
          background = "#1d2021";
          foreground = "#fbf1c7";
          module.margin = 1;
        };
        "bar/desktop-left" = {
          "inherit" = "bar/base";
          monitor = "DP-2";
        };
        "bar/desktop-right" = {
          "inherit" = "bar/base";
          monitor = "DP-4";
        };
        "module/cpu" = {
          type = "internal/cpu";
          # format = "<label> <ramp-coreload>";
          format = "<label>";
          format-foreground = "#ecbe7b";
          label = " %percentage%%";
          label-warn = " %percentage%%";
          ramp-coreload = ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"];
        };
        "module/ram" = {
          type = "internal/memory";
          format-foreground = "#ff6c6b";
          format = " <label>";
          label = "%used%";
        };
        "module/disk" = {
          type = "internal/fs";
          mount = ["/"];
          format-mounted = "<label-mounted>";
          format-mounted-foreground = "#51afef";
          label-mounted = " %free%";
        };
        "module/date" = {
          type = "internal/date";
          format = " <label> ";
          format-foreground = "#46d9ff";
          label = "%date% %time%";
          date = "%a, %d %b";
          time = "%H:%M";
          date-alt = "%Y-%m-%d%";
          time-alt = "%H:%M";
        };
        "module/xmonad" = {
          type = "custom/script";
          exec = "${pkgs.xmonad-log}/bin/xmonad-log";
          tail = true;
        };
        "module/start" = {
          type = "custom/text";
          content = " NixOS";
          content-padding = 1;
          content-background = "#8ec07c";
          content-foreground = "#1d2021";
        };
      };
    };
  };
}

