let
  desktop = "jeyj0-nixos";
  framework = "jeyj0-framework";
  obsidianIgnore = ''
    .obsidian/workspace
    .obsidian/workspace.json
    .obsidian/workspaces.json
  '';
in
[
  {
    id = "02-areas/00-ttrpgs";
    label = "00-ttrpgs";
    path = "02-areas/00-ttrpgs";
    stignore = obsidianIgnore;
    devices = [ framework desktop ];
  }
  {
    id = "02-areas";
    label = "02-areas";
    path = "02-areas";
    stignore = ''
      /00-ttrpgs
    '';
    devices = [ framework ];
  }
]
