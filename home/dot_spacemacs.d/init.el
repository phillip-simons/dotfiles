;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(systemd
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ansible
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-idle-delay 0.2
                      auto-completion-minimum-prefix-length 1
                      auto-completion-private-snippets-directory nil
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-use-company-box t)
     (better-defaults :variables
                      better-defaults-move-to-end-of-code-first t)
     bibtex
     (colors :variables
             colors-colorize-identifiers 'variables)
     dap
     django
     (docker :variables
             docker-dockerfile-backend 'lsp)
     emacs-lisp
     erc
     games
     (git :variables
          git-enable-magit-todos-plugin t
          spacemacs--git-blame-ts-full-hint-toggle t)
     github-copilot
     helm
     helpful
     (html :variables
           css-enable-lsp t
           less-enable-lsp t
           scss-enable-lsp t
           html-enable-lsp t)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     javascript
     (json :variables
           json-backend 'lsp
           json-fmt-tool 'prettier
           json-fmt-on-save t)
     kubernetes
     (latex :variables
            latex-build-command "xelatex"
            latex-enable-auto-fill t
            latex-enable-folding t
            latex-enable-magic t
            latex-backend 'lsp
            latex-refesh-preview t
            latex-view-pdf-in-split-window t)
     finance
     (llm-client :variables
                 llm-client-enable-ellama t
                 llm-client-enable-gptel t)
     (lsp :variables lsp-lens-enable t
          lsp-use-lsp-ui t
          lsp-rust-server 'rust-analyzer)
     lua
     (markdown :variables
               markdown-live-preview-engine 'vmd
               markdown-mmm-auto-modes '("python"
                                         "rust"
                                         ("elisp" "emacs-lisp")))
     multiple-cursors
     nav-flash
     nixos
     (org :variables
          org-enable-appear-support t
          org-appear-trigger 'manual
          org-enable-github-support t
          org-enable-notifications t
          org-start-notification-daemon-on-startup t
          org-enable-transclusion-support t
          org-want-todo-bindings t
          org-enable-org-contacts-support t)
     pass
     pdf
     python
     (restclient :variables
                 restclient-use-org t)
     (rust :variables rust-format-on-save t)
     (shell :variables
            shell-default-term-shell "/usr/bin/zsh"
            shell-default-position 'bottom
            shell-default-height 30
            shell-default-shell 'vterm)
     (shell-scripts :variables
                    shell-scripts-backend 'lsp
                    shell-scripts-format-on-save t)
     (spell-checking :variables
                     enable-flyspell-auto-completion t
                     spell-checking-enable-auto-dictionary t
                     spell-checking-enable-by-default nil)
     (sql :variables
          sql-backend 'lsp
          sql-capitalize-keywords t
          sql-lsp-sqls-workspace-config-path 'workspace
          sql-auto-indent nil)
     syntax-checking
     tmux
     toml
     (treemacs :variables
               treemacs-use-git-mode 'deferred)
     typescript
     (unicode-fonts :variables
                    unicode-fonts-enable-ligatures t
                    unicode-fonts-ligature-modes '(prog-mode)
                    unicode-fonts-ligature-set '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                                 ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                                 "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                                 "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                                 "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                                 "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                                 "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                                 "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                                 ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                                 "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                                 "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                                 "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                                 "\\\\" "://"))
     version-control
     (wakatime :variables
               wakatime-cli-path "/usr/bin/wakatime")

     (yaml :variables yaml-enable-lsp t)

     ;; custom layers
     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     bui
     dash
     dash-functional
     helm-mu
     igist
     lsp-docker
     mixed-pitch
     mu4easy
     org-auto-tangle
     org-msg
     org-noter
     org-reverse-datetree
     org-starter
     ox-ssh
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 30

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(vim :variables
                                    vim-style-visual-feedback t
                                    vim-style-remap-Y-to-y$ t
                                    vim-style-retain-visual-state-on-shift t
                                    vim-style-visual-line-move-text t
                                    vim-style-ex-substitute-global nil)

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent t

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-one
                         doom-one-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator slant :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("JetBrains Mono Nerd Font"
                               :size 16.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (flyspell-mode 0)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (global-subword-mode 1)
  (setq edit-server-default-major-mode 'org-mode)
  (setq undo-limit 500000
        evil-want-fine-undo t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-ahl --group-directories-first")
  (add-hook 'dired-mode-hook 'org-download-enable)

  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe
                                        :noquery t))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste)

  (dolist (e '("service" "timer" "target" "mount" "automount"
               "slice" "socket" "path" "netdev" "network"
               "link"))
    (push (cons (concat "\\." e "\\'") 'conf-unix-mode)
          auto-mode-alist))
  (add-hook 'org-mode-hook 'mixed-pitch-mode)
  (setq org-confirm-babel-evaluate nil)
  (setq
   gptel-model "codellama:latest"
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '("codellama:latest")))

  (setq auth-sources '("~/.authinfo.gpg"))

  (setq wakatime-api-key
        (let ((creds (car (auth-source-search :host "wakatime-api-key" :port nil))))
          (when creds
            (funcall (plist-get creds :secret)))))

  (setq org-hide-emphasis-markers t)
  (setq org-agenda-window-setup (quote current-window))
  (setq org-agenda-files (directory-files-recursively "~/org/" "\.org$"))
  (setq org-log-done 'time)
  (setq org-src-tab-acts-natively t)
  (setq org-return-follows-link  t)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-skip-unavailable-files t)
  (setq org-capture-templates
        '(

          ("n" "Notes")
          ("np" "Personal Note"
           entry (file+headline "~/org/10-19_Personal/12_Notes/12.11_notes.org" "General Notes")
           "** %?"
           :empty-lines 0)

          ("nw" "Work Note"
           entry (file+headline "~/org/20-29_Work/27_Notes/27.11_notes.org" "General Notes")
           "** %?"
           :empty-lines 0)

          ("t" "TODOs")
          ("tp" "Personal To-Do"
           entry (file+headline "~/org/10-19_Personal/10_System/10.03_todos.org" "General Tasks")
           "* TODO [#B] %?\n:Created: %T\n "
           :empty-lines 0)
          ("tw" "Work To-Do"
           entry (file+headline "~/org/20-29_Work/20_System/20.03_todos.org" "General Tasks")
           "* TODO [#B] %?\n:Created: %T\n "
           :empty-lines 0)
          ("tc" "Code To-Do"
           entry (file+headline "~/org/20-29_Work/20_System/20.03_todos.org" "Code Related Tasks")
           "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
           :empty-lines 0)

          ("m" "Meeting"
           entry (file+function "~/org/20-29_Work/24_Meetings/24.11_meetinglog.org" org-reverse-datetree-goto-date-in-file)
           "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
           :tree-type week
           :clock-in t
           :clock-resume t
           :empty-lines 0)

          ("c" "Contacts" entry (file "~/org/00-09_System/01_Inbox/01.02_Contacts.org")
           "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE:
:ALIAS:
:NICKNAME:
:IGNORE:
:ICON:
:NOTE:
:ADDRESS:
:BIRTHDAY:
:END:")

          ("f" "Follow Up")
          ("fp" "Personal Follow Up" entry (file+olp "~/org/10-19_Personal/10_System/10.03_todos.org" "Follow Up")
           "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
          ("fw" "Work Follow Up" entry (file+olp "~/org/20-29_Work/20_System/20.03_todos.org" "Follow Up")
           "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
          ))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w!)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "PAUSED(a!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" )
          ))

  (setq org-todo-keyword-faces
        '(
          ("TODO"        . (:foreground "GoldenRod"  :weight bold))
          ("WAITING"     . (:foreground "yellow1"    :weight bold))
          ("PLANNING"    . (:foreground "DeepPink"   :weight bold))
          ("IN-PROGRESS" . (:foreground "Cyan"       :weight bold))
          ("PAUSED"      . (:foreground "gray"       :weight bold))
          ("VERIFYING"   . (:foreground "DarkOrange" :weight bold))
          ("BLOCKED"     . (:foreground "Red"        :weight bold))
          ("DONE"        . (:foreground "LimeGreen"  :weight bold))
          ("OBE"         . (:foreground "LimeGreen"  :weight bold))
          ("WONT-DO"     . (:foreground "LimeGreen"  :weight bold))
          ))


  ;; Tags
  (setq org-tag-alist '(
                        ;; Meeting types
                        (:startgroup . nil)
                        ("team_meeting" . ?t)
                        ("1on1" . ?1)
                        ("all_hands" . ?h)
                        ("sync" . ?s)
                        (:endgroup . nil)

                        ;; Code TODOs tags
                        ("QA" . ?q)
                        ("backend" . ?k)
                        ("broken_code" . ?c)
                        ("infrastructure" . ?i)

                        ;; Special tags
                        ("CRITICAL" . ?x)
                        ("obstacle" . ?o)

                        ;; Meeting tags
                        ("general" . ?g)
                        ("meeting" . ?m)
                        ("misc" . ?z)
                        ("planning" . ?n)

                        ;; Work Log Tags
                        ("accomplishment" . ?a)

                        ;; Personal tags
                        ("hobby" . ?h)
                        ("personal" . ?p)

                        ;; Organization
                        (:startgroup . nil)
                        ("directory" . ?d)
                        ("file" . ?f)
                        ("link" . ?n)
                        (:endgroup . nil)
                        ))

  ;; Tag colors
  (setq org-tag-faces
        '(
          ("planning"  . (:foreground "mediumPurple1" :weight bold))
          ("backend"   . (:foreground "royalblue1"    :weight bold))
          ("frontend"  . (:foreground "forest green"  :weight bold))
          ("QA"        . (:foreground "sienna"        :weight bold))
          ("meeting"   . (:foreground "yellow1"       :weight bold))
          ("CRITICAL"  . (:foreground "red1"          :weight bold))
          )
        )
  ;; Agenda View "d"
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

  PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

  (setq org-agenda-skip-deadline-if-done t)

  (setq org-agenda-custom-commands
        '(
          ;; Daily Agenda & TODOs
          ("d" "Daily agenda and all TODOs"

           ;; Display items with priority A
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))

            ;; View 7 days in the calendar view
            (agenda "" ((org-agenda-span 7)))

            ;; Display items with priority B (really it is view all items minus A & C)
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                     (air-org-skip-subtree-if-priority ?C)
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:")))

            ;; Display items with pirority C
            (tags "PRIORITY=\"C\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
            )

           ;; Don't compress things (change to suite your tastes)
           ((org-agenda-compact-blocks nil)))
          ))
  (setq org-contacts-files '("~/org/00-09_System/01_Inbox/01.02_Contacts.org" "~/org/20-29_Work/23_People/23.11_Contacts.org" "~/org/10-19_Personal/17_People/17.11_Contacts.org"))
  (setq org-startup-indented t
        org-pretty-entities t)
  (spacemacs|diminish wakatime-mode " ⓦ" " W")
  (spacemacs|diminish mixed-pitch-mode "" " ")
  (setq mail-user-agent 'mu4e-user-agent)

  (setq-default evil-kill-on-visual-paste nil)
  (require 'git-commit)
  (global-git-commit-mode t)
  ;; Speed up TRAMP connections by skipping certain checks
  (setq tramp-default-method "ssh")
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq igist-auth-marker 'forge)
  (use-package igist
    :init
    (spacemacs/set-leader-keys
      "gg" 'igist-dispatch)
    :ensure t)
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  ;; Function to match second-level headings under "* Projects"
  (defun my/org-projects-refile-target ()
    (save-excursion
      (org-back-to-heading t)
      (when (looking-at "^\\* Projects")
        (org-map-entries (lambda () (point)) nil 'tree))))

  (setq org-refile-targets
        '((nil :maxlevel . 1) ;; Top-level headings globally
          (my/org-projects-refile-target :level . 2))) ;; Second-level under "* Projects"

  (setq mu4easy-contexts '((mu4easy-context
                            :c-name  "Personal"
                            :name    "Phillip Simons"
                            :maildir "GmailPersonal"
                            :mail    "phillip@simons.gg"
                            :smtp    "smtp.gmail.com"
                            :sent-action delete
                            :sig "

Best,

#+begin_signature
--
Phillip Simons
📧 [[mailto:phillip@simons.gg][phillip@simons.gg]]
📞 +1 (214) 842-0054
#+end_signature")
                           (mu4easy-context
                            :c-name  "Work"
                            :name    "Phillip Simons"
                            :maildir "GmailWork"
                            :mail    "phillip@book.io"
                            :smtp    "smtp.gmail.com"
                            :sent-action delete
                            :sig "

Best,

#+begin_signature
--
*Phillip Simons*
Book.io | Stuff.io
📧 [[mailto:phillip@book.io][phillip@book.io]]
📞 +1 (214) 842-0054
#+end_signature")
                           ))
  (use-package mu4easy
    :init (spacemacs/set-leader-keys "aem" 'mu4e)
    :ensure t
    :config
    (evilified-state-evilify-map mu4e-main-mode-map
      :mode mu4e-main-mode
      :bindings
      (kbd "j") 'mu4e-search-maildir
      (kbd "C-j") 'next-line
      (kbd "C-k") 'previous-line)

    (evilified-state-evilify-map
      mu4e-headers-mode-map
      :mode mu4e-headers-mode
      :bindings
      (kbd "C-j") 'mu4e-headers-next
      (kbd "C-k") 'mu4e-headers-prev
      (kbd "J") (lambda ()
                  (interactive)
                  (mu4e-headers-mark-thread nil '(read))))

    (evilified-state-evilify-map
      mu4e-view-mode-map
      :mode mu4e-view-mode
      :bindings
      (kbd "C-j") 'mu4e-view-headers-next
      (kbd "C-k") 'mu4e-view-headers-prev
      (kbd "J") (lambda ()
                  (interactive)
                  (mu4e-view-mark-thread '(read)))
      (kbd "gu") 'mu4e-view-go-to-url)

    (spacemacs/set-leader-keys-for-major-mode 'org-msg-edit-mode
      dotspacemacs-major-mode-leader-key 'org-ctrl-c-ctrl-c
      "c" 'org-ctrl-c-ctrl-c
      "k" 'message-kill-buffer
      "a" 'org-msg-attach
      "s" 'message-goto-subject
      "b" 'org-msg-goto-body
      "e" 'org-msg-preview
      "d" 'message-dont-send)
    (setq mu4easy-greeting "Howdy%s,\n\n")
    (mu4easy-mode)
    )

  (use-package helm-mu
    :defer t
    :config
    (setq helm-mu-contacts-personal t)
    :init
    (let ((mu4e-modes (append '(mu4e-main-mode mu4e-headers-mode)
                              '(mu4e-view-mode mu4e-compose-mode mu4e-loading-mode))))
      (dolist (m mu4e-modes)
        (spacemacs/set-leader-keys-for-major-mode m
          "S" 'helm-mu
          "/" 'helm-mu
          "C" 'helm-mu-contacts))))

  (require 'org-auto-tangle)

  (add-hook 'org-mode-hook 'org-auto-tangle-mode)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-modules
     '(ol-bbdb ol-bibtex org-crypt ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe org-protocol ol-rmail ol-w3m))
   '(org-project-capture-projects-file "~/org/20-29_Work/21_Projects/21.03_todos.org")
   '(org-refile-targets '((nil :maxlevel . 2)))
   '(package-selected-packages
     '(company-box frame-local company-quickhelp company-statistics flyspell-popup hledger-mode mwim sqlup-mode unfill ellama gptel helm-ag org-auto-tangle helm-mu org-msg org-transclusion org-wild-notifier mu4e-alert journalctl-mode systemd igist chezmoi company-restclient know-your-http-well ligature magic-latex-buffer ob-http ob-restclient ox-ssh restclient-helm restclient vmd-mode company-org-block ox-gfm treemacs-all-the-icons neotree pdf-view-restore pdf-tools dash-functional helm-bibtex org-noter org-ref ox-pandoc citeproc bibtex-completion biblio biblio-core parsebib queue org-reverse-datetree org-starter helm-pass password-store-otp password-store xref dap-mode lsp-docker bui eziam-themes farmhouse-themes fish-mode flatland-theme flatui-theme flycheck-bashate gandalf-theme gotham-theme grandshell-theme gruber-darker-theme gruvbox-theme hc-zenburn-theme helpful elisp-refs hemisu-theme heroku-theme ibuffer-projectile inkpot-theme insert-shebang ir-black-theme jazz-theme jbeans-theme jinja2-mode js-doc js2-refactor multiple-cursors json-mode json-navigator hierarchy json-reformat json-snatcher kaolin-themes kubernetes-evil kubernetes magit-popup light-soap-theme livid-mode lsp-latex consult lua-mode lush-theme madhat2r-theme majapahit-themes material-theme math-symbol-lists minimal-theme modus-themes moe-theme molokai-theme monochrome-theme monokai-theme mustang-theme naquadah-theme nav-flash noctilux-theme nodejs-repl npm-mode obsidian-theme occidental-theme oldlace-theme omtose-phellack-theme org-appear org-sticky-header organic-green-theme pacmacs persistent-scratch phoenix-dark-mono-theme phoenix-dark-pink-theme planet-theme pony-mode professional-theme purple-haze-theme railscasts-theme rebecca-theme reverse-theme seti-theme shfmt reformatter skewer-mode js2-mode smyx-theme soft-charcoal-theme soft-morning-theme soft-stone-theme solarized-theme soothe-theme autothemer spacegray-theme sql-indent subatomic-theme subatomic256-theme sublime-themes sudoku sunny-day-theme tango-2-theme tango-plus-theme tangotango-theme tao-theme toxi-theme twilight-anti-bright-theme twilight-bright-theme twilight-theme typescript-mode typit mmt ujelly-theme underwater-theme unicode-fonts ucs-utils font-utils persistent-soft pcache wakatime-mode white-sand-theme yaml-mode zen-and-art-theme zenburn-theme zonokai-emacs eat esh-help eshell-prompt-extras eshell-z helm-lsp lsp-origami origami lsp-pyright lsp-treemacs lsp-ui multi-term multi-vterm shell-pop terminal-here vterm lsp-mode auto-yasnippet company-anaconda company-web web-completion-data copilot helm-c-yasnippet helm-company company yasnippet-snippets yasnippet anaconda-mode auto-dictionary blacken code-cells cython-mode emmet-mode evil-org flycheck-pos-tip pos-tip flyspell-correct-helm flyspell-correct gh-md git-link git-messenger git-modes git-timemachine gitignore-templates gnuplot helm-css-scss helm-git-grep helm-ls-git helm-org-rifle helm-pydoc impatient-mode htmlize simple-httpd importmagic epc ctable concurrent deferred live-py-mode markdown-toc nose org-cliplink org-contrib org-download org-mime org-pomodoro alert log4e gntp org-present org-projectile org-project-capture org-category-capture org-rich-yank orgit-forge orgit forge yaml ghub closql emacsql treepy pip-requirements pipenv load-env-vars pippel poetry prettier-js pug-mode py-isort pydoc pyenv-mode pythonic pylookup pytest pyvenv ron-mode rustic xterm-color markdown-mode rust-mode sass-mode haml-mode scss-mode slim-mode smeargle sphinx-doc tagedit toml-mode treemacs-magit magit magit-section git-commit with-editor transient web-beautify web-mode yapfify ws-butler writeroom-mode winum which-key volatile-highlights vim-powerline vi-tilde-fringe uuidgen undo-tree treemacs-projectile treemacs-persp treemacs-icons-dired treemacs-evil toc-org term-cursor symon symbol-overlay string-inflection string-edit-at-point spacemacs-whitespace-cleanup spacemacs-purpose-popwin spaceline space-doc restart-emacs request rainbow-delimiters quickrun popwin pcre2el password-generator paradox overseer org-superstar open-junk-file nameless multi-line macrostep lorem-ipsum link-hint inspector info+ indent-guide hybrid-mode hungry-delete holy-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org helm-mode-manager helm-make helm-descbinds helm-comint google-translate golden-ratio flycheck-package flycheck-elsa flx-ido fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-evilified-state evil-escape evil-easymotion evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu emr elisp-slime-nav elisp-def editorconfig dumb-jump drag-stuff dotenv-mode dired-quick-sort diminish devdocs define-word column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-compile all-the-icons aggressive-indent ace-link ace-jump-helm-line)))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:background nil))))
   '(fixed-pitch ((t (:family "JetBrains Mono Nerd Font"))))
   '(org-level-1 ((t (:inherit (outline-1 variable-pitch) :extend nil))))
   '(org-level-2 ((t (:inherit (outline-2 variable-pitch) :extend nil))))
   '(org-level-3 ((t (:inherit (outline-3 variable-pitch) :extend nil))))
   '(org-level-4 ((t (:inherit (outline-4 variable-pitch) :extend nil))))
   '(org-level-5 ((t (:inherit (outline-5 variable-pitch) :extend nil))))
   '(org-level-6 ((t (:inherit (outline-6 variable-pitch) :extend nil))))
   '(org-level-7 ((t (:inherit (outline-7 variable-pitch) :extend nil))))
   '(org-level-8 ((t (:inherit (outline-8 variable-pitch) :extend nil))))
   '(variable-pitch ((t (:weight regular :family "Inter")))))
  )
