{ userName, pkgs }:
{
  programs.firefox = {
    enable = true;
    extensions = with pkgs.firefox-addons; [
      bitwarden
      https-everywhere
      privacy-badger
      pushbullet
      simple-tab-groups
    ];
    profiles = {
      ${userName} = {};
    };
  };
  home.file.firefoxContainers = {
    source = ./containers.json;
    target = ".mozilla/firefox/${userName}/containers.json";
  };
}
