{ pkgs, osConfig, setupOptions, lib, ... }:
let
  fonts = osConfig.themes.fonts;
  thm = osConfig.themes.colors;
  nur = osConfig.nur;
in  
{
	# Module installing librewolf as default browser
	home.packages = with pkgs; [
		librewolf
    skypeforlinux
	];

  programs.aria2 = {
    enable = true;
    settings = (
      let
        getHomePath = with lib.strings; p: concatStrings [ "/home/" "${setupOptions.user.username}" p ];
      in
        {
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
        });
  };

	home.sessionVariables = {
		DEFAULT_BROWSER = "${pkgs.firefox}/bin/firefox";
	};

	home.file.".librewolf/librewolf.overrides.cfg".text = ''
		defaultPref("font.name.serif.latin","''+fonts.serif.family+''");
		defaultPref("font.name.monospace.latin","''+fonts.mono.family+''");

		defaultPref("font.size.variable.latin",${toString fonts.serif.size});
		defaultPref("browser.toolbars.bookmarks.visibility","always");
		defaultPref("privacy.resisttFingerprinting.letterboxing", true);
		defaultPref("network.http.referer.XOriginPolicy",2);
		defaultPref("privacy.clearOnShutdown.history",true);
		defaultPref("privacy.clearOnShutdown.downloads",true);
		defaultPref("privacy.clearOnShutdown.cookies",true);
		defaultPref("gfx.webrender.software.opengl",false);
		defaultPref("webgl.disabled",true);
		pref("font.name.serif.latin","''+fonts.serif.family+''");
		pref("font.name.monospace.latin","''+fonts.mono.family+''");

		pref("font.size.variable.latin",${toString fonts.serif.size});
		pref("browser.toolbars.bookmarks.visibility","always");
		pref("privacy.resisttFingerprinting.letterboxing", true);
		pref("network.http.referer.XOriginPolicy",2);
		pref("privacy.clearOnShutdown.history",true);
		pref("privacy.clearOnShutdown.downloads",true);
		pref("privacy.clearOnShutdown.cookies",true);
		pref("gfx.webrender.software.opengl",false);
		pref("webgl.disabled",true);
		'';

  programs.firefox = {
    enable = true;

    policies = {
      CaptivePortal = false;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DisableFirefoxAccounts = false;
      NoDefaultBookmarks = true;
      OfferToSaveLogins = false;
      OfferToSaveLoginsDefault = false;
      PasswordManagerEnabled = false;
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
      extensions = with nur.repos.rycee.firefox-addons; [
        adnauseam
        aria2-integration
        betterttv
        darkreader
        gruvbox-dark-theme
        privacy-badger
        rust-search-extension
        ublock-origin
      ];
      
      id = 0;
      userChrome = ''
          toolbar#nav-bar, nav-bar-customization-target {
            background: ${thm.base00} !important;
    }
          @-moz-document url("about:newtab") {
            * { background-color: ${thm.base00}  !important; }
          }
        '';
      
      settings = {
        "general.smoothScroll" = true;
        
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

        "extensions.autoDisableScopes" = 0;

        # "browser.startup.homepage" = "https://duckduckgo.com";

        "signon.rememberSignons" = false;
        "widget.use-xdg-desktop-portal.file-picker" = 1;

        "mousewheel.default.delta_multiplier_x" = 20;
        "mousewheel.default.delta_multiplier_y" = 20;
        "mousewheel.default.delta_multiplier_z" = 20;

        # Firefox 75+ remembers the last workspace it was opened on as part of its session management.
        # This is annoying, because I can have a blank workspace, click Firefox from the launcher, and
        # then have Firefox open on some other workspace.
        "widget.disable-workspace-management" = true;

        "browser.aboutConfig.showWarning" = false;
        "browser.compactmode.show" = true;
        "browser.cache.disk.enable" = false; # Be kind to hard drive
        "browser.search.defaultenginename" = "Google";
        "browser.search.selectedEngine" = "Google";
        "browser.urlbar.placeholderName" = "Google";
        "browser.search.region" = "US";

        "browser.uidensity" = 1;
        "browser.search.openintab" = true;
        "xpinstall.signatures.required" = false;
        "extensions.update.enabled" = false;

        "font.name.monospace.x-western" = "${fonts.mono.family}";
        "font.name.sans-serif.x-western" = "${fonts.main.family}";
        "font.name.serif.x-western" = "${fonts.serif.family}";

        # "browser.display.background_color" = thm.base00;
        # "browser.display.foreground_color" = thm.base05;
        # "browser.display.document_color_use" = 2;
        # "browser.anchor_color" = thm.base0D;
        # "browser.visited_color" = thm.base0C;
        "browser.display.use_document_fonts" = true;
        "pdfjs.disabled" = true;
        "media.videocontrols.picture-in-picture.enabled" = true;

        "widget.non-native-theme.enabled" = false;

        "browser.newtabpage.enabled" = false;
        "browser.startup.homepage" = "about:blank";

        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "browser.ping-centre.telemetry" = false;
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.hybridContent.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.reportingpolicy.firstRun" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.updatePing.enabled" = false;

        "experiments.activeExperiment" = false;
        "experiments.enabled" = false;
        "experiments.supported" = false;
        "network.allow-experiments" = false;
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
	};
}
