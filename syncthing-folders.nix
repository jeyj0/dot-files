[
  {
    id = "02-areas/00-ttrpgs";
    label = "00-ttrpgs";
    path = "02-areas/00-ttrpgs";
    stignore = ''
      .obsidian/workspace
      .obsidian/workspaces.json
    '';
    devices = [ "jeyj0-framework" "jeyj0-nixos" ];
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
]
