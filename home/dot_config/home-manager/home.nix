{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "phillip";
  home.homeDirectory = "/home/phillip";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    pkgs.nerdfonts
    pkgs.nixfmt-rfc-style
    pkgs.git
    pkgs.kitty

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/phillip/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = "Phillip Simons";
    userEmail = "phillip@book.io";

    extraConfig = {
      maintenance.repo = "/home/phillip/code/booktoken-web";
      init.defaultBranch = "main";
      core.editor = "emacsclient -c -a emacs";
      color.ui = "auto";
      gpg.format = "ssh";
      github.user = "phillip-simons";

      # Enforce SSH
      url."ssh://git@github.com".insteadOf = "https://github.com/";
      url."ssh://git@gitlab.com".insteadOf = "https://gitlab.com/";
      url."ssh://git@bitbucket.org".insteadOf = "https://bitbucket.org/";
    };
  };
  programs.kitty = {
    enable = true;
    font = {
      name = "JetBrains Mono Nerd Font";
      size = 16.0;
    };
    settings = {
      boldFont = "auto";
      italicFont = "auto";
      boldItalicFont = "auto";

      confirmOSWindowClose = false;
      linuxDisplayServer = "auto";

      scrollbackLines = 2000;
      wheelScrollMinLines = 1;

      enableAudioBell = false;
      windowPaddingWidth = 4;

      selectionForeground = "none";
      selectionBackground = "none";

      copyOnSelect = true;

      mouseMap = "right press ungrabbed paste_from_selection";
      clipboardControl = "write-primary write-clipboard no-append";
    };
    themeFile = "Doom_One";
  };
}
