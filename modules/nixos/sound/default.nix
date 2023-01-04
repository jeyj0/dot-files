{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.sound = {
    enable = mkEnableOption "sound";
  };

  config = mkIf config.jeyj0.sound.enable {
    # Enable sound with pipewire.
    sound.enable = true;
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;
    };
  };
}