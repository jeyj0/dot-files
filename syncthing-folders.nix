[
  {
    id = "02-areas/00-ttrpgs";
    label = "00-ttrpgs";
    path = "02-areas/00-ttrpgs";
    stignore = ''
      .obsidian
    '';
    devices = [ "jeyj0-framework" ];
  }
  {
    id = "02-areas";
    label = "02-areas";
    path = "02-areas";
    stignore = ''
      /00-ttrpgs
    '';
    devices = [ "jeyj0-framework" ];
  }
  {
    id = "666-test";
    label = "666-test";
    path = "666-test";
    stignore = ''
      .obsidian
    '';
    devices = [ "jeyj0-nixos" "jeyj0-framework" ];
  }
]
