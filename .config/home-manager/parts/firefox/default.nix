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
      ${userName} = {
        settings = {
          # first, disable stuff I don't want
          "extensions.pocket.enabled" = false;

          ## disable firefox thinking it's my first time using it (even if it is)
          "app.normandy.first_run" = false;
          "trailhead.firstrun.didSeeAboutWelcome" = true;

          # enable/change stuff I do want
          "browser.startup.homepage" = "https://search.jorre.dev/";

          ## use duckduckgo as default search engine
          "browser.urlbar.placeholderName" = "DuckDuckGo";

          ## force new tabs into containers
          "privacy.userContext.newTabContainerOnLeftClick.enabled" = true;

          ## this is needed for full dark-theme support of simple-tab-groups
          "svg.context-properties.content.enabled" = true;

          ## customize toolbar
          "browser.uiCustomization.state" = ''
            {"placements":{"widget-overflow-fixed-list":["jid1-mnnxcxisbpnsxq_jetpack-browser-action","https-everywhere_eff_org-browser-action"],"nav-bar":["back-button","forward-button","stop-reload-button","urlbar-container","downloads-button","library-button","_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action","jid1-bycqofyfmbmd9a_jetpack-browser-action","simple-tab-groups_drive4ik-browser-action"],"toolbar-menubar":["menubar-items"],"TabsToolbar":["tabbrowser-tabs","new-tab-button","alltabs-button"],"PersonalToolbar":["personal-bookmarks","managed-bookmarks"]},"seen":["developer-button","_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action","https-everywhere_eff_org-browser-action","jid1-bycqofyfmbmd9a_jetpack-browser-action","jid1-mnnxcxisbpnsxq_jetpack-browser-action","simple-tab-groups_drive4ik-browser-action"],"dirtyAreaCache":["nav-bar","toolbar-menubar","TabsToolbar","PersonalToolbar","widget-overflow-fixed-list"],"currentVersion":16,"newElementCount":4}
          '';

          ## force links to open in new tabs instead of windows
          "browser.link.open_newwindow" = 3;

          ## ask where to download files to
          "browser.download.useDownloadDir" = false;

          ## do not remember the stuff I enter in forms
          "browser.formfill.enable" = false;

          ## do not ask to set as default browser, I do that myself when I want
          "browser.shell.checkDefaultBrowser" = false;

          ## reopen last pages on browser startup
          "browser.startup.page" = 2;

          ## use blank page for new tabs
          "browser.newtabpage.enabled" = false;

          # allowing add-ons
          "extensions.webextensions.ExtensionStorageIDB.migrated.https-everywhere@eff.org" = true;
        };
      };
    };
  };

  home.file.firefoxContainers = {
    source = ./containers.json;
    target = ".mozilla/firefox/${userName}/containers.json";
  };
}
