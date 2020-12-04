{ buildFirefoxXpiAddon, fetchurl, stdenv }:
  {
    "bitwarden" = buildFirefoxXpiAddon {
      pname = "bitwarden";
      version = "1.47.0";
      addonId = "{446900e4-71c2-419f-a6a7-df9c091e268b}";
      url = "https://addons.mozilla.org/firefox/downloads/file/3677817/bitwarden_free_password_manager-1.47.0-an+fx.xpi";
      sha256 = "8580571d340c60db808d974710ce296c40155ca15e528ccaf1580788eecf98ea";
      meta = with stdenv.lib;
      {
        homepage = "https://bitwarden.com";
        description = "A secure and free password manager for all of your devices.";
        license = licenses.gpl3;
        platforms = platforms.all;
        };
      };
    "https-everywhere" = buildFirefoxXpiAddon {
      pname = "https-everywhere";
      version = "2020.11.17";
      addonId = "https-everywhere@eff.org";
      url = "https://addons.mozilla.org/firefox/downloads/file/3679479/https_everywhere-2020.11.17-an+fx.xpi";
      sha256 = "a6ebcb0a05607e54e7a9fc0b5b3832eda6f13f8dce2ee802164a455919e385c9";
      meta = with stdenv.lib;
      {
        homepage = "https://www.eff.org/https-everywhere";
        description = "Encrypt the web! HTTPS Everywhere is a Firefox extension to protect your communications by enabling HTTPS encryption automatically on sites that are known to support it, even when you type URLs or follow links that omit the https: prefix.";
        platforms = platforms.all;
        };
      };
    "privacy-badger" = buildFirefoxXpiAddon {
      pname = "privacy-badger";
      version = "2020.10.7";
      addonId = "jid1-MnnxcxisBPnSXQ@jetpack";
      url = "https://addons.mozilla.org/firefox/downloads/file/3656444/privacy_badger-2020.10.7-an+fx.xpi";
      sha256 = "da9d488a9ee75347b73931a7d0136540292cd3db8aa0bfe4c273503291f7019e";
      meta = with stdenv.lib;
      {
        homepage = "https://privacybadger.org/";
        description = "Automatically learns to block invisible trackers.";
        license = licenses.gpl3;
        platforms = platforms.all;
        };
      };
    "pushbullet" = buildFirefoxXpiAddon {
      pname = "pushbullet";
      version = "362";
      addonId = "jid1-BYcQOfYfmBMd9A@jetpack";
      url = "https://addons.mozilla.org/firefox/downloads/file/3629969/pushbullet-362-fx.xpi";
      sha256 = "2bc8d8dc80e17dfdd1191e4760c214629eb938e5bc76f475c281d89fa5ab713e";
      meta = with stdenv.lib;
      {
        homepage = "https://www.pushbullet.com";
        description = "Pushbullet enables you to see your calls and texts on your computer and easily send links and files from your computer to phone.";
        license = licenses.mpl20;
        platforms = platforms.all;
        };
      };
    "simple-tab-groups" = buildFirefoxXpiAddon {
      pname = "simple-tab-groups";
      version = "4.7";
      addonId = "simple-tab-groups@drive4ik";
      url = "https://addons.mozilla.org/firefox/downloads/file/3683240/simple_tab_groups-4.7-fx.xpi";
      sha256 = "51f8aaec37c27eb31fd1f210dbcf3a5ad35c190e90b5a59c0d7573cca480f470";
      meta = with stdenv.lib;
      {
        homepage = "https://github.com/drive4ik/simple-tab-groups";
        description = "Create, modify, and quickly change tab groups";
        license = licenses.mpl20;
        platforms = platforms.all;
        };
      };
    }