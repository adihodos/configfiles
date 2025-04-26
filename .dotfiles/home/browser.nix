{
  pkgs,
  osConfig,
  setupOptions,
  lib,
  ...
}: let
  fonts = osConfig.themes.fonts;
  thm = osConfig.themes.colors;
in {
  home.packages = with pkgs; [
    # skypeforlinux
    # teams
  ];

  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;

    #extraOpts = {
    #  "BrowserSignin" = 0;
    #  "SyncDisabled" = true;
    #  "PasswordManagerEnabled" = false;
    #  "SpellcheckEnabled" = true;
    #  "SpellcheckLanguage" = [
    #    "en-US"
    #  ];
    #};

    #defaultSearchProviderSearchURL = "https://google.com";
    #defaultSearchProviderSuggestURL = "https://google.com";

    # extensions = [
    #   "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
    #   "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
    #   "lckanjgmijmafbedllaakclkaicjfmnk" # ClearURLs
    #   "ldpochfccmkkmhdbclfhpagapcfdljkj" # Decentraleyes
    #   "eimadpbcbfnmbkopoojfekhnkhdbieeh" # Dark Reader
    #   "nngceckbapebfimnlniiiahkandclblb" # Bitwarden
    #   "pmcmeagblkinmogikoikkdjiligflglb" # Privacy Redirect
    #   "fihnjjcciajhdojfnbdddfaoknhalnja" # I don't care about cookies
    #   "nomnklagbgmgghhjidfhnoelnjfndfpd" # Canvas Blocker - Fingerprint Protect
    # ];

    commandLineArgs = [
    ];
  };

  programs.librewolf = {
    #enable = true;
    # Enable WebGL, cookies and history
    settings = {
      "webgl.disabled" = false;
      "privacy.resistFingerprinting" = false;
      "privacy.clearOnShutdown.history" = false;
      "privacy.clearOnShutdown.cookies" = false;
      "network.cookie.lifetimePolicy" = 0;
    };
  };

  programs.aria2 = {
    enable = true;
    settings = (
      let
        getHomePath = with lib.strings; p: concatStrings ["/home/" "${setupOptions.user.username}" p];
      in {
        ### Basic ###
        # The directory to store the downloaded file.
        dir = getHomePath "/Downloads";
        # Downloads the URIs listed in FILE.;
        input-file = getHomePath "/.aria2/aria2.session";
        # Save error/unfinished downloads to FILE on exit.;
        save-session = getHomePath "/.aria2/aria2.session";
        # Save error/unfinished downloads to a file specified by --save-session option every SEC seconds. If 0 is given, file will be saved only when aria2 exits. Default: 0;
        save-session-interval = 60;
        # Set the maximum number of parallel downloads for every queue item. See also the --split option. Default: 5;
        max-concurrent-downloads = 5;
        # Continue downloading a partially downloaded file.;
        continue = true;
        # Set max overall download speed in bytes/sec. 0 means unrestricted. Default: 0;
        max-overall-download-limit = 0;
        # Set max download speed per each download in bytes/sec. 0 means unrestricted. Default: 0;
        max-download-limit = 0;
        # Make aria2 quiet (no console output). Default: false;
        quiet = true;

        ### Advanced ###
        # Restart download from scratch if the corresponding control file doesn't exist. Default: false
        allow-overwrite = true;
        # If false is given, aria2 aborts download when a piece length is different from one in a control file. If true is given, you can proceed but some download progress will be lost. Default: false
        allow-piece-length-change = true;
        # Always resume download. If true is given, aria2 always tries to resume download and if resume is not possible, aborts download. If false is given, when all given URIs do not support resume or aria2 encounters N URIs which does not support resume, aria2 downloads file from scratch. Default: true
        always-resume = true;
        # Enable asynchronous DNS. Default: true
        async-dns = false;
        # Rename file name if the same file already exists. This option works only in HTTP(S)/FTP download. Default: true
        auto-file-renaming = true;
        # Handle quoted string in Content-Disposition header as UTF-8 instead of ISO-8859-1, for example, the filename parameter, but not the extended version filename. Default: false
        content-disposition-default-utf8 = true;
        # Enable disk cache. If SIZE is 0, the disk cache is disabled. This feature caches the downloaded data in memory, which grows to at most SIZE bytes. SIZE can include K or M. Default: 16M
        disk-cache = "64M";
        # Specify file allocation method. none doesn't pre-allocate file space. prealloc pre-allocates file space before download begins. This may take some time depending on the size of the file. If you are using newer file systems such as ext4 (with extents support), btrfs, xfs or NTFS(MinGW build only), falloc is your best choice. It allocates large(few GiB) files almost instantly. Don't use falloc with legacy file systems such as ext3 and FAT32 because it takes almost same time as prealloc and it blocks aria2 entirely until allocation finishes. falloc may not be available if your system doesn't have posix_fallocate(3) function. trunc uses ftruncate(2) system call or platform-specific counterpart to truncate a file to a specified length. Possible Values: none, prealloc, trunc, falloc. Default: prealloc
        file-allocation = "falloc";
        # No file allocation is made for files whose size is smaller than SIZE. Default: 5M
        no-file-allocation-limit = "8M";
        # Set log level to output to console. LEVEL is either debug, info, notice, warn or error. Default: notice
        # console-log-level=notice
        # Set log level to output. LEVEL is either debug, info, notice, warn or error. Default: debug
        # log-level=debug
        # The file name of the log file. If - is specified, log is written to stdout. If empty string("") is specified, or this option is omitted, no log is written to disk at all.
        # log=

        ### HTTP/FTP/SFTP ###
        # The maximum number of connections to one server for each download. Default: 1
        max-connection-per-server = 16;
        # aria2 does not split less than 2*SIZE byte range. Possible Values: 1M -1024M. Default: 20M
        min-split-size = "8M";
        # Download a file using N connections. The number of connections to the same host is restricted by the --max-connection-per-server option. Default: 5
        split = 32;
        # Set user agent for HTTP(S) downloads. Default: aria2/$VERSION, $VERSION is replaced by package version.
        user-agent = "Transmission/2.77";
      }
    );
  };

  home.sessionVariables = {
    DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
  };

  programs.firefox = {
    enable = true;

    policies = {
      CaptivePortal = false;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DisableFirefoxAccounts = false;
      OfferToSaveLogins = false;
      OfferToSaveLoginsDefault = false;
      PasswordManagerEnabled = false;
      NoDefaultBoomarks = true;
      FirefoxHome = {
        Search = true;
        Pocket = false;
        Snippets = false;
        TopSites = false;
        Highlights = false;
      };
      UserMessaging = {
        ExtensionRecommendations = false;
        SkipOnboarding = true;
      };
    };

    profiles.default = {
      id = 0;
      name = "Default";
      isDefault = true;

      # extensions = with nur.repos.rycee.firefox-addons; [
      #   # adnauseam
      #   # aria2-integration
      #   betterttv
      #   darkreader
      #   dark-mode-webextension
      #   # gruvbox-dark-theme
      #   privacy-badger
      #   rust-search-extension
      #   ublock-origin
      # ];
      #
      search = {
        force = true;
        engines = {
          "Nix Packages" = {
            urls = [
              {
                template = "https://search.nixos.org/packages";
                params = [
                  {
                    name = "type";
                    value = "packages";
                  }
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];

            icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
            definedAliases = ["@np"];
          };

          "NixOS Wiki" = {
            urls = [{template = "https://nixos.wiki/index.php?search={searchTerms}";}];
            iconUpdateURL = "https://nixos.wiki/favicon.png";
            updateInterval = 24 * 60 * 60 * 1000; # every day
            definedAliases = ["@nw"];
          };

          "Bing".metaData.hidden = true;
          "Google".metaData.alias = "@g"; # builtin engines only support specifying one additional alias
        };
      };

      # - https://arkenfox.github.io/gui/
      settings = {
        "general.smoothScroll" = true;

        "browser.startup.page" = 0;
        "browser.startup.homepage" = "about:blank";
        "browser.newtabpage.enabled" = false;
        "browser.newtabpage.activity-stream.showSponsored" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.newtabpage.activity-stream.default.sites" = "";

        "browser.discovery.enabled" = false;
        "browser.shopping.experience2023.enabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "datareporting.healthreport.uploadEnabled" = false;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.server" = "data:,";
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.coverage.opt-out" = true;
        "toolkit.coverage.opt-out" = true;
        "toolkit.coverage.endpoint.base" = "";
        "browser.ping-centre.telemetry" = false;
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "app.shield.optoutstudies.enabled" = false;
        "app.normandy.enabled" = false;
        "app.normandy.api_url" = "";
        "breakpad.reportURL" = "";
        "browser.tabs.crashReporting.sendReport" = false;
        "browser.crashReports.unsubmittedCheck.enabled" = false;
        "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;

        "browser.urlbar.suggest.quicksuggest.nonsponsored" = false;
        "browser.urlbar.suggest.quicksuggest.sponsored" = false;
        "browser.search.suggest.enabled" = false;
        "browser.urlbar.suggest.searches" = false;
        "browser.urlbar.trending.featureGate" = false;
        "browser.urlbar.addons.featureGate" = false;
        "browser.urlbar.mdn.featureGate" = false;
        "browser.urlbar.pocket.featureGate" = false;
        "browser.urlbar.weather.featureGate" = false;
        "browser.urlbar.clipboard.featureGate" = false;
        "browser.formfill.enable" = false;
        "browser.urlbar.suggest.engines" = false;
        "layout.css.visited_links_enabled" = false;
        "browser.search.separatePrivateDefault" = true;
        "browser.search.separatePrivateDefault.ui.enabled" = true;

        "signon.autofillForms" = false;
        "signon.formlessCapture.enabled" = false;
        "network.auth.subresource-http-auth-allow" = 1;
        "network.http.windows-sso.enabled" = false;

        # disk avoidance
        "browser.cache.disk.enable" = false;
        "browser.privatebrowsing.forceMediaMemoryCache" = true;
        "media.memory_cache_max_size" = 65536;
        "browser.sessionstore.privacy_level" = 2;
        "toolkit.winRegisterApplicationRestart" = false;
        "browser.shell.shortcutFavicons" = false;

        # misc
        "browser.download.start_downloads_in_tmp_dir" = true;
        "browser.helperApps.deleteTempFileOnExit" = true;
        "browser.uitour.enabled" = false;
        "browser.uitour.url" = "";
        "devtools.debugger.remote-enabled" = false;
        "permissions.default.shortcuts" = 2;
        "permissions.manager.defaultsUrl" = "";
        "webchannel.allowObject.urlWhitelist" = "";
        "network.IDN_show_punycode" = true;
        "pdfjs.disabled" = false;
        "pdfjs.enableScripting" = false;
        "browser.tabs.searchclipboardfor.middleclick" = false;
        "browser.download.useDownloadDir" = false;
        "browser.download.alwaysOpenPanel" = false;
        "browser.download.manager.addToRecentDocs" = false;
        "browser.download.always_ask_before_handling_new_types" = true;
        "extensions.enabledScopes" = 5;
        "extensions.autoDisableScopes" = 15;
        "extensions.postDownloadThirdPartyPrompt" = false;
        "extensions.webextensions.restrictedDomains" = "";

        "browser.cache.memory.enable" = false;
        "browser.cache.memory.capacity" = 0;
        "signon.rememberSignons" = false;
        "permissions.memory_only" = true;
        "security.nocertdb" = true;
        "browser.chrome.site_icons" = false;
        "browser.sessionstore.max_tabs_undo" = 0;
        "browser.sessionstore.resume_from_crash" = false;
        "browser.download.forbid_open_with" = true;
        "browser.urlbar.suggest.history" = false;
        "browser.urlbar.suggest.bookmark" = false;
        "browser.urlbar.suggest.openpage" = false;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.maxRichResults" = 0;
        "browser.urlbar.autoFill" = false;
        "places.history.enabled" = false;
        "browser.taskbar.lists.enabled" = false;
        "browser.taskbar.lists.frequent.enabled" = false;
        "browser.taskbar.lists.recent.enabled" = false;
        "browser.taskbar.lists.tasks.enabled" = false;
        "browser.download.folderList" = 2;
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "dom.popup_allowed_events" = "click dblclick mousedownpointerdown";
        "browser.pagethumbnails.capturing_disabled" = true;
        "alerts.useSystemBackend.windows.notificationserver.enabled" = false;
        "keyword.enabled" = false;

        #"mousewheel.default.delta_multiplier_x" = 20;
        #"mousewheel.default.delta_multiplier_y" = 20;
        #"mousewheel.default.delta_multiplier_z" = 20;
      };
    };
  };

  #default browser
  xdg.mimeApps.defaultApplications = {
    "text/html" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
    "x-scheme-handler/about" = "firefox.desktop";
    "x-scheme-handler/unknown" = "firefox.desktop";
    "application/xhtml+xml" = "firefox.desktop";
    "application/xhtml_xml" = "firefox.desktop";
  };
}
