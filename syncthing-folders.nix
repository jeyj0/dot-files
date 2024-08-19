let
  desktop = "jeyj0-nixos";
  framework = "jeyj0-framework";
  phone = "jeyj0-phone";
  pixel8 = "jeyj0-pixel8";

  baseIgnore = ''
    // MacOS
    .DS_Store

    // Linux
    .fuse_hidden*
    .nfs*
    *~

    // obsidian
    .obsidian/workspace
    .obsidian/workspace*.json
  '';
in
[
  {
    id = "01-projects";
    label = "01-projects";
    path = "01-projects";
    stignore = baseIgnore;
    devices = [ framework desktop ];
  }
  {
    id = "02-areas";
    label = "02-areas";
    path = "02-areas";
    stignore = baseIgnore + ''
      /00-ttrpgs
      /00-notes
    '';
    devices = [ framework desktop ];
  }
  {
    id = "02-areas/00-ttrpgs";
    label = "00-ttrpgs";
    path = "02-areas/00-ttrpgs";
    stignore = baseIgnore;
    devices = [ framework desktop phone pixel8 ];
  }
  {
    id = "02-areas/00-notes";
    label = "00-notes";
    path = "02-areas/00-notes";
    stignore = baseIgnore;
    devices = [ framework desktop phone pixel8 ];
  }
  {
    id = "03-resources";
    label = "03-resources";
    path = "03-resources";
    stignore = baseIgnore;
    devices = [ framework desktop ];
  }
  {
    id = "04-archive";
    label = "04-archive";
    path = "04-archive";
    stignore = baseIgnore;
    devices = [ framework desktop ];
  }
  {
    id = "05-photos";
    label = "05-photos";
    path = "05-photos";
    stignore = baseIgnore;
    devices = [ framework desktop ];
  }
  {
    id = "07-media/00-inbox/00-phone-camera";
    label = "00-phone-camera";
    path = "07-media/00-inbox/00-phone-camera";
    stignore = baseIgnore + ''
      .thumbnails
    '';
    devices = [ desktop phone ];
  }
]
