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
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(windows-scripts
     javascript
     sql
     version-control
     gtags
     (auto-completion  :variables
                       auto-completion-enable-help-tooltip 'manual)
     better-defaults
     git
     multiple-cursors
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-scripts-backend nil)
     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-tooltips nil)
     (org :variables
          ;; org-enable-trello-support t
          org-enable-roam-support t

          )
     (helm :variables
           spacemacs-helm-rg-max-column-number 1024)

     ;; Langs
     emacs-lisp
     common-lisp
     ;; semantic
     ;; csharp
     ;; (csharp :variables csharp-backend 'lsp)
     (python :variables python-backend 'anaconda)

     shell-scripts
     html
     markdown
     yaml
     ;; typescript
     ;; nim
     ;; kotlin
     (c-c++ :variable c-c++-backend nil)
     vimscript
     (lsp :variables
          lsp-headerline-breadcrumb-enable nil
          lsp-headerline-breadcrumb-segments '(symbol))
     (go :variables godoc-at-point-function 'godoc-gogetdoc go-backend 'lsp)
     (perl5 :variables perl5-backend nil)
     (clojure :variables
              clojure-backend 'cider
              clojure-enable-linters '(clj-kondo joker)
              clojure-enable-fancify-symbols t
              clojure-enable-sayid nil
              clojure-enable-clj-refactor t)
     ;; lua
     (scheme :variables
             scheme-implementations
             '(guile racket chicken))
     csv

     (haskell :variables haskell-completion-backend 'dante)
     ;; (julia :variables julia-backend 'lsp)


     ;; features
     ;; slack

     pass
     ;; media
     (rcirc :variables rcirc-enable-authinfo-support t
            ;; rcirc-enable-znc-support t
            )

     benj-csharp

     ;; benj
     benj-funcs
     benj-themes-config
     substitute-utils
     csharp-config
     redshiftel

     best-banners
     minder
     parens-config
     benj-pomodoro
     benj-roslyn
     team-projectile
     teamel
     helm-config
     sharpel
     benj-phone
     benj-csharp-perf-hacks
     benj-evil-motions

     team-snippets
     sxhkd-mode
     my-structural-csharp
     redshank
     system-utils

     lispy
     evil-better-jumper

     lispyville-csharp
     benj-helpful

     pdf
     dap

     ;; mu4e

                                        ;exwm


     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      rg
                                      doom-themes
                                      emr
                                      auth-source-pass
                                      dired-x
                                      nav-flash
                                      minsk-theme
                                      minimap
                                      shx
                                      string-edit
                                      geiser-chicken
                                      macrostep-geiser
                                      palimpsest
                                      backup-each-save
                                      ;; structural-haskell-mode
                                      ;; adoc-mode

                                      )


   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(org-superstar
                                    forge
                                    orgit-forge
                                    vterm
                                    ghub
                                    packed
                                    auto-highlight-symbol
                                    org-brain
                                    lsp-treemacs
                                    treemacs
                                    windows-scripts
                                    lsp-ui
                                    omnisharp
                                    )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only)


  ;; (when (frame-parameter (selected-frame) 'exwm-active)
  ;;   (add-to-list dotspacemacs-configuration-layers 'exwm))
  )

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
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
   dotspacemacs-elpa-timeout 5

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
   dotspacemacs-editing-style 'vim

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
   dotspacemacs-startup-banner 'random

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

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

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
   dotspacemacs-themes '(
                         doom-monokai-pro
                         spacemacs-dark
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(custom :separator nil :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 15
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

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

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
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

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
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
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
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t

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

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non nil. (default nil)
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
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (setq package-native-compile t)

  (setq package-check-signature nil)

  (setq org-roam-v2-ack t)

  (defun team/spacemacs-define-keys (leader-keys prefix-name &rest bindings)
    "Define spacemacs keys."
    (spacemacs/declare-prefix leader-keys prefix-name)
    (eval
     `(mapc (lambda (x) (spacemacs/set-leader-keys (concat ,leader-keys (car x)) (cdr x)))
            '(,@bindings))))

  (defun team/spacemacs-declare-keys (prefix name &rest bindings)
    "BINDINGS should be single letter strings and function symbol names."
    (declare (indent 2))
    (spacemacs/declare-prefix prefix name)
    (apply
     #'spacemacs/set-leader-keys
     (--mapcat
      (if (stringp it)
          (list (concat prefix it))
        (list it))
      (-flatten bindings))))

  (defun spaceline-custom-theme (&rest additional-segments)
    "Sapce line with less stuff"
    (spaceline-compile
      `(
        ;; '((:eval
        ;;    (when
        ;;        (featurep 'lispyville)
        ;;      (lispyville-mode-line-string))))
        '((persp-name
           workspace-number
           window-number)
          :fallback evil-state
          :face highlight-face
          :priority 100)
        auto-compile
        '((buffer-modified buffer-size buffer-id remote-host)
          :priority 98)
        (major-mode :priority 79)
        (process :when active)
        ((flycheck-error flycheck-warning flycheck-info)
         :when active
         :priority 89)
        (minor-modes :when active
                     :priority 9)
        (org-pomodoro :when active)
        (org-clock :when active)
        nyan-cat)
      `(which-function
        (python-pyvenv :fallback python-pyenv)
        (purpose :priority 94)
        ;; (battery :when active)
        ;; (selection-info :priority 95)
        input-method
        ((buffer-encoding-abbrev
          point-position
          line-column)
         :separator " | "
         :priority 96)
        ;; (global :when active)
        (buffer-position :priority 99)
        (hud :priority 99)
        ,@additional-segments))

    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))


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

  (use-package keychain-environment
    :straight  (:host github
                      :repo "tarsius/keychain-environment")
    :demand t
    :init
    (progn
      (keychain-refresh-environment)
      (auth-source-pass-enable)))


  (let ((default-directory "~/.spacemacs.d/"))
    (load (expand-file-name "lisp/team-elisp-config.el")))

  
  ;;projectile-fd
  (defconst my-fd-command "fd -H -E=.git -tf . -0")
  (setq-default projectile-indexing-method 'alien)
  (setq-default projectile-git-command my-fd-command)
  (setq-default projectile-generic-command my-fd-command)
  (setq projectile-enable-caching t)

  

  (setq uniquify-buffer-name-style 'forward)

  (setq-default evil-escape-key-sequence nil)
  (global-set-key (kbd "C-<escape>") 'evil-escape)

  (setq-default spacemacs-show-trailing-whitespace nil)
  (setq-default dotspacemacs-show-trailing-whitespace nil)
  (setq-default avy-timeout-seconds 0.25)
  (setq git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line))
  (set-face-foreground 'spaceline-python-venv "SeaGreen")
  (setq-default split-width-threshold 80)
  (setq-default split-hight-threshold 500)
  (show-smartparens-global-mode -1)
  (setq history-delete-duplicates t)
  ;; (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hook-common-lisp-mode)
  (set-face-foreground 'line-number "slateBlue")
  (setq-default page-break-lines-max-width 30)
  (define-key evil-normal-state-map (kbd "M-Y") #'evil-paste-pop-next)
  (setf gtags-enable-by-default nil)

  (setq auto-save-no-message t)
  ;; (setf completion-styles '(flex))
  (setf company-format-margin-function nil)

  ;; scrolling

  (setq jit-lock-defer-time 0)
  (setq redisplay-skip-fontification-on-input t)
  (setq fast-but-imprecise-scrolling t)
  (setf scroll-conservatively 0)
  (setf next-error-verbose nil)


  ;;; Disable prompt (but leave warning) on git symlink.
  (setq vc-follow-symlinks t)

  ;;; Clipboard and primary selection.
  ;; (setq select-enable-clipboard t)

  ;; this hogs when I put a big buff in the selection
  ;; (setq select-enable-primary t
  ;;       save-interprogram-paste-before-kill t)
  ;; (setq select-enable-primary t)

  (setf json-backend nil)

  (setf persistent-scratch-what-to-save '(major-mode))


  

  (global-so-long-mode 1)

  (global-evil-mc-mode 1)
  (defun disable-evil-mc-mode ()
    (evil-mc-mode -1))
  (add-hook 'dired-mode-hook #'disable-evil-mc-mode)

  ;; visual line mode
  (global-visual-line-mode 1)
  (defun disable-visual-line-mode ()
    (visual-line-mode -1))

  (add-hook 'git-commit-mode-hook #'disable-visual-line-mode)

  (setf persp-auto-save-opt 0)

  

  (setq org-agenda-files
        '("~/org/notes.org"
          "~/org/jan.org"
          "~/org/rico.org"))

  

  (spacemacs/set-leader-keys "km" #'spacemacs/macrostep-transient-state/body)

  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (spacemacs/declare-prefix-for-mode
      mode
      "ep"
      "pretty")
    (spacemacs/set-leader-keys-for-major-mode
      mode
      "epe" #'pp-eval-last-sexp
      "eps" #'pp-eval-expression))


  

  ;; `evil-goggles'
  (evil-goggles-mode)
  (set-face-background 'evil-goggles-default-face "DarkOliveGreen")

  

  (defun my/update-spacemacs ()
    (let ((default-directory "~/.emacs.d"))
      (magit-pull "spacemacs_public" "refs/heads/develop")))

  

  ;; Remove some magit status sections that take long

  (setq
   magit-status-sections-hook
   '(magit-insert-upstream-branch-header
     magit-insert-head-branch-header
     magit-insert-upstream-branch-header
     magit-insert-push-branch-header
     magit-insert-rebase-sequence
     magit-insert-am-sequence
     magit-insert-sequencer-sequence
     ;; magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes
     magit-insert-stashes
     magit-insert-unpushed-to-upstream-or-recent
     magit-insert-unpulled-from-upstream))

  (remove-hook
   'magit-pre-refresh-hook
   #'spacemacs//git-gutter+-refresh-in-all-buffers)

  (defun benj-add-no-merges-to-magit-log-buffer (args)
    "Add --no-merges arg to magit log buffers."
    (cl-destructuring-bind
        (revs log-args files &optional locked focus) args
      (list revs
            (cons
             "--no-merges"
             log-args)
            files locked focus)))

  (advice-add 'magit-log-setup-buffer :filter-args #'benj-add-no-merges-to-magit-log-buffer)

  

  (dolist (elm '(minibuffer-setup-hook eshell-mode-hook slack-message-buffer-mode-hook))
    (add-hook elm
              #'(lambda ()
                  (unless (bound-and-true-p helm--minor-mode)
                    (smartparens-strict-mode)))))

  (dolist (elm '(wdired-mode-hook))
    (add-hook elm #'evil-mc-mode))

  

  ;; helm

  (setq
   helm-follow-mode-persistent t
   helm-reuse-last-window-split-state t
   helm-display-header-line nil
   helm-findutils-search-full-path t
   helm-show-completion-display-function nil
   helm-completion-mode-string ""
   helm-dwim-target 'completion
   helm-echo-input-in-header-line t
   helm-use-frame-when-more-than-two-windows nil

   helm-apropos-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-eshell-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-M-x-fuzzy-match t
   helm-recentf-fuzzy-match t

   ;; Use woman instead of man.
   helm-man-or-woman-function nil

   ;; https://github.com/emacs-helm/helm/issues/1910
   helm-buffers-end-truncated-string "…"
   helm-buffer-max-length 22

   helm-window-show-buffers-function 'helm-window-mosaic-fn
   helm-window-prefer-horizontal-split t)


  (with-eval-after-load 'avy
    (setf avy-all-windows nil)
    (setf avy-all-windows-alt t)

    (defadvice avy--line-cands
        (around my/avy--line-cands-adv (&optional arg beg end bottom-up) activate)
      (let ((visual-line-mode nil))
        ad-do-it)))

  (defadvice spacemacs/open-file-or-directory-in-external-app (around my/open-file-external-advice (&optional arg) activate)
    (interactive"P")
    (when (yes-or-no-p "Open file or dir externally? ")
      ad-do-it))


  (defadvice spacemacs/prompt-kill-emacs (around my/kill-emacs-adv activate)
    (interactive)
    (when (yes-or-no-p "Kill emacs? ")
      ad-do-it))

  (defadvice evil-force-normal-state (before my/evil-normal-state-maybe-delete-mc-cursors activ)
    (require 'evil-mc-vars)
    (when (and
           evil-mc-cursor-state
           (eq evil-state 'normal))
      (evil-mc-undo-all-cursors)))


  

  (defun benj-sys/invoke-watch-vbs ()
    "Invoke python script that kills vbs compiler processes."
    (team/with-default-dir
     "~/"
     (start-process-shell-command "watch-vbs" "*watch-vbs*" "watch_vbs.py")))

  (defvar benj-sys/inhibit-kill-high-mem nil)
  (defun benj-sys/invoke-kill-high-mem ()
    "Invoke perl script that kills high mem procs"
    (unless benj-sys/inhibit-kill-high-mem
      (start-process-shell-command "kill-high-mem" "*kill-high-mem*" "kill-high-mem-procs")))



  (run-at-time 10 10 #'benj-sys/invoke-watch-vbs)
  (run-at-time 20 20 #'benj-sys/invoke-kill-high-mem)

  

  ;; redefine this shit becuase I always mash `k` anyway
  (defun persp--kill-buffer-query-function-foreign-check (&rest args) 'kill)

  ;; run at startup and then at desire with ,ot
  ;; don't run as timer because gtags are corrupted sometimes
  (when team-enable-gtags (cos/regenerate-gtags-background))

  
  ;; temp hacks

  (load "~/.spacemacs.d/temp-hacks.el")
  

  (add-hook 'shell-mode-hook #'ggtags-mode)

  

  (add-to-list 'load-path "~/.spacemacs.d/lisp/")

  (with-eval-after-load
      'org-capture (require 'config-org-capture))

  (require 'benj-shell-script)
  (require 'init-slime)

  (setf team-enable-slack nil)
  (when team-enable-slack
    (use-package
      init-slack
      :defer 100))

  (add-to-list 'load-path "~/.spacemacs.d/lisp/emacs-hex-to-rgba/")
  (use-package
    hex-to-rgba
    :defer t
    :init
    (team/spacemacs-declare-keys
        "oe."
        "misc tools"
      "h" #'hex-to-rgba))
  

  (use-package string-edit
    :init
    (team/spacemacs-declare-keys
        "oe."
        "misc tools"
      "s" #'string-edit-at-point))


  

  (add-to-list 'load-path "~/.spacemacs.d/lisp/lisp-extra-font-lock/")
  (require 'lisp-extra-font-lock)
  ;; REVIEW not sure if so useful for clojure and scheme because of lisp-1
  (setf
   lisp-extra-font-lock-modes
   (delete-dups
    (append
     lisp-extra-font-lock-modes
     '(clojure-mode common-lisp-mode scheme-mode))))

  (lisp-extra-font-lock-global-mode 1)
  (add-hook 'lisp-extra-font-lock-mode-hook #'dash-fontify-mode)

  

  (add-hook 'markdown-mode-hook 'evil-normalize-keymaps)
  (evil-define-key '(normal motion) markdown-mode-map
    (concat (kbd ",") (kbd ",")) 'with-editor-finish)

  (team/spacemacs-declare-keys
      ","
      "emacs dev etc"
    "t" #'trace-function
    "T" #'untrace-function
    "x" #'untrace-all
    "d" #'debug-on-entry
    "D" #'cancel-debug-on-entry
    "e" #'edebug-on-entry
    "E" #'cancel-edebug-on-entry
    "v" #'debug-on-variable-change
    "V" #'cancel-debug-on-variable-change
    "m" #'my-macro-expand)

  (defun my-macro-expand ()
    (interactive)
    (funcall
     (pcase major-mode
       ('clojure-mode #'cider-macroexpand-1-inplace)
       ('common-lisp-mode #'slime-macroexpand-1-inplace)
       (_ #'macrostep-expand))))


  (team/spacemacs-declare-keys "k"
      "lisp"
    "m" #'my-macro-expand)

  

  (set-face-foreground
   'font-lock-variable-name-face
   "LightGreen")

  

  (with-eval-after-load
      'csharp-mode
    (idlegame-add-csharp-yas-hook))

  (remove-hook
   'outline-mode-hook
   #'windows-scripts/bat-outline-setup)

  (custom-set-faces
   '(lsp-face-highlight-textual
     ((t
       (:inherit show-paren-match)
       (:underline "white")
       (:background "grey50")))))

  ;; shut up lsp
  (setf shell-scripts-backend nil)

  (setf auto-revert-verbose nil)

  

  ;; (setq auth-sources '(password-store))

  (setq rcirc-server-alist
        '(("irc.libera.chat"
           :nick "benj234"
           :user-name "benj234"
           ;; :auth "benj"
           :channels ("#emacs" "#guile" "#nyxt"))
          ;; ("irc.gimp.org"
          ;;  :nick "benj234"
          ;;  :user-name "benj234"
          ;;  ;; :auth "benj"
          ;;  :channels ("#gimp-users"))
          ))


  (with-eval-after-load
      'geiser
    (require 'init-geiser))

  ;; (cl-pushnew "~/.guix-profile/share/info" Info-additional-directory-list :test #'equal)

  


  ;; kill eval output
  (let ((kill (lambda (val)
                (prog1 val (kill-new (mkstr val))))))
    (advice-add 'eval-last-sexp :filter-return kill)
    (advice-add 'eval-defun     :filter-return kill))


  (use-package palimpsest
    :config (progn
              (add-hook 'prog-mode-hook 'palimpsest-mode)
              (spacemacs|diminish palimpsest-mode)))


  ;; (load "~/.spacemacs.d/hacks2.el")

  (when (file-exists-p "~/.guix-profile/share/emacs/site-lisp/")
    (let ((default-directory "~/.guix-profile/share/emacs/site-lisp/"))
      (load "subdirs.el")))

  ;; (use-package guix
  ;;   :load guix-autoloads
  ;;   :demand t)

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-/") 'helm-company))


  (use-package metronome
    :straight t
    :demand t
    :config 
    (setf metronome-click "/home/benj/game-assets/sci-fi-sounds/Audio/impactMetal_000.wav"
          metronome-accent "/home/benj/game-assets/sci-fi-sounds/Audio/laserSmall_004.wav"))



  

  (define-key spacemacs-default-map "ojh"
    (lambda (&optional arg) (interactive "P")
      (dired-jump nil (expand-file-name
                       (if arg "~/repos/"
                         "~/.profile")))))



  ;; (use-package flycheck-guile
  ;;   :demand t
  ;;   :config
  ;;   (add-hook
  ;;    'geiser-mode-hook
  ;;    (lambda ()
  ;;      (when (eq geiser-impl--implementation 'guile)
  ;;        (flycheck-mode 1)))))

  (use-package dired-dragon
    :straight (:host github
                     :repo "jeetelongname/dired-dragon")
    :config (spacemacs/declare-prefix-for-mode 'dired-mode "d" "dragon")
    :bind (:map spacemacs-dired-mode-map
                ("C-d d" . dired-dragon)
                ("C-d s" . dired-dragon-stay)
                ("C-d i" . dired-dragon-individual)))


  (add-hook 'prog-mode-hook #'lispyville-mode)

  (use-package memoize
    :straight  (:host github :repo "skeeto/emacs-memoize"))

  

  (defun say-many-fluid (kg)
    (let* ((ml (ounce-to-ml
                (/ (kg-to-lbs kg) 40)))
           (sips (/ ml (float 15))))
      (message "You need to drink around %s ml water every 15 minutes. This is around %s sips" ml sips)))

  (defun kg-to-lbs (kg)
    (/ kg 0.45359237))
  (defun ounce-to-ml (ounce)
    (* ounce 28.35))

  (run-at-time
   (* 15 60)
   (* 15 60)
   (apply-partially #'say-many-fluid 75))


  (with-eval-after-load 'python (require 'python-config))

  


  (use-package restclient
    :straight t
    :commands restclient-mode)

  (use-package company-restclient
    :after restclient
    :straight t
    :init (add-hook
           'restclient-mode-hook
           (lambda ()
             (add-to-list 'company-backends 'company-restclient)
             (company-mode-on))))


  (use-package backup-each-save
    :commands backup-each-save
    :preface
    (defun my-make-backup-file-name (file)
      (make-backup-file-name-1 (expand-file-name (file-truename file))))

    (defun backup-each-save-filter (filename)
      (not (string-match
            (concat "\\(^/tmp\\|\\.emacs\\.d/data\\(-alt\\)?/"
                    "\\|\\.newsrc\\(\\.eld\\)?\\|"
                    "\\(archive/sent/\\|recentf\\`\\)\\)")
            filename)))

    (defun my-dont-backup-files-p (filename)
      (unless (string-match filename "\\(archive/sent/\\|recentf\\`\\)")
        (normal-backup-enable-predicate filename)))

    :hook after-save
    :config
    (setq backup-each-save-filter-function 'backup-each-save-filter
          backup-enable-predicate 'my-dont-backup-files-p))

  (setf
   backup-directory-alist
   '((".*" . "~/.cache/emacs/backups"))
   undo-tree-history-directory-alist
   '((".*" . "~/.cache/emacs/backups")))


  (use-package youtube-dl
    :straight (:host github :repo "skeeto/youtube-dl-emacs")
    :config (progn
              (setf youtube-dl-directory "~/tmp/vids/")

              (defun benj-push-all-vids-to-phone-and-delete ()
                "Push all vids in `youtube-dl-directory' wiht adb and delete them"
                (interactive)
                (with-dir
                 youtube-dl-directory
                 "fd -tf . -e mkv -e mp4 -X sh -c \"adb -d push {} /storage/self/primary/misc-downloads && rm -f {}\""))

              (team/spacemacs-declare-keys
                  "oe"
                  "external"
                "y" #'youtube-dl
                "V" #'youtube-dl-list)
              )
    )

  (use-package team-trello
    :commands #'team-trello-card-dispatch
    :config
    (progn
      (setf team-trello-token
            (auth-source-pick-first-password :host "trello-token")
            team-trello-key
            (auth-source-pick-first-password :host "trello-key"))))


  (with-eval-after-load 'flyspell
    (dolist (hook '(text-mode-hook org-mode-hook))
      (add-hook hook #'flyspell-mode-on)))


  ;; `spacemacs/next-error' should not exist
  ;; (setf flycheck-standard-error-navigation t)
  ;; (defalias 'spacemacs/next-error #'next-error)


  (use-package quick-peek
    :straight (:host github :repo "cpitclaudel/quick-peek")
    :commands quick-peek-overlay-ensure-at
    :demand t
    :after flycheck)

  (use-package flycheck-inline
    :straight (:host github :repo "flycheck/flycheck-inline")
    :after quick-peek
    :preface

    (defun my-quick-peek-flycheck-inline-fn (msg pos err)
      (let* ((ov (quick-peek-overlay-ensure-at pos))
             (contents (quick-peek-overlay-contents ov)))
        (overlay-put
         ov
         'quick-peek--contents
         (concat contents (when contents "\n") msg))
        (quick-peek-update ov)))


    (defun my-flycheck-inline-setup ()
      (remove-hook 'next-error-hook #'flycheck-display-error-at-point 'local)
      (flycheck-inline-mode t))

    ;; flycheck-inline ads calls clear function to post command hook
    ;; this is cool but not after `flycheck-next-error'
    (defvar my-flycheck-inlide-hide-errs-tanks-one nil)
    (defvar my-flycheck-inlide-hide-errs-tanks-after-next nil)

    (defadvice flycheck-next-error
        (before
         my-flycheck-inlide-hack-flycheck-next-err-adv
         activate)
      (setf my-flycheck-inlide-hide-errs-tanks-after-next t))

    (defadvice flycheck-inline-hide-errors
        (around
         my-flycheck-inlide-hide-errs-hack-adv
         activate)
      (if my-flycheck-inlide-hide-errs-tanks-one
          (setf my-flycheck-inlide-hide-errs-tanks-one nil)
        ad-do-it)
      (setf my-flycheck-inlide-hide-errs-tanks-one
            my-flycheck-inlide-hide-errs-tanks-after-next
            my-flycheck-inlide-hide-errs-tanks-after-next
            nil))

    :init (add-hook 'flycheck-mode-hook #'my-flycheck-inline-setup)
    :config
    (setq flycheck-inline-display-function
          #'my-quick-peek-flycheck-inline-fn
          flycheck-inline-clear-function
          #'quick-peek-hide))


  (with-eval-after-load 'cider
    (require 'init-cider))

  (use-package omnisharp
    :straight  (:host github :repo "rtnlmeme-DestroyerOfDeath/omnisharp-emacs")
    :defer t
    :init
    (add-hook 'csharp-mode-hook 'omnisharp-mode)

    (add-to-list 'spacemacs-jump-handlers-csharp-mode
                 '(omnisharp-go-to-definition :async t))

    :config
    (spacemacs//csharp-configure))


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
 '(evil-want-Y-yank-to-eol nil)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(package-selected-packages
   '(macrostep-geiser geiser-guile geiser-chicken benj-file-syncer elint string-edit visual-fill-column vterm slime shx org-rich-yank org-cliplink magit-section js2-mode markdown-mode ghub flycheck magit git-commit emojify ht dired-quick-sort csharp-mode company-quickhelp company-emoji company packed auto-complete helm request projectile anzu lispy iedit transient helm-core with-editor dash-functional dash multiple-cursors circe websocket powerline org-plus-contrib evil goto-chg hydra persistent-scratch pdf-tools tablist tern realgud company-plsense quelpa csv-mode godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc flycheck-golangci-lint company-go go-mode google-c-style disaster cpp-auto-include company-c-headers clang-format insert-shebang flycheck-bashate company-shell yaml-mode yapfify pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements live-py-mode importmagic epc ctable concurrent deferred helm-pydoc helm-gtags helm-cscope xcscope cython-mode company-anaconda anaconda-mode pythonic web-mode tagedit pug-mode impatient-mode helm-css-scss haml-mode emmet-mode counsel-css company-web web-completion-data add-node-modules-path yasnippet-snippets xterm-color ws-butler writeroom-mode winum which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unfill toc-org symon string-inflection spaceline-all-the-icons smeargle shell-pop rg restart-emacs rainbow-delimiters popwin persp-mode password-generator paradox overseer orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets open-junk-file omnisharp nameless mwim multi-term move-text mmm-mode minitest markdown-toc magit-svn magit-gitflow macrostep lorem-ipsum  link-hint json-navigator json-mode indent-guide hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org-rifle helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-pos-tip flycheck-package flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-themes doom-modeline diminish diff-hl define-word counsel-projectile company-tern company-statistics column-enforce-mode clean-aindent-mode chruby centered-cursor-mode bundler browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ace-link ace-jump-helm-line ac-ispell))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook 'time-stamp)
     (geiser-guile-load-path "~/.guix-profile/share/guile/site/3.0")
     (ambrevar/prettify-inhibit-p)
     (whitespace-style quote
                       (face trailing empty tabs))
     (whitespace-action)
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (go-backend . go-mode)))
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(lsp-face-highlight-textual ((t (:inherit show-paren-match) (:underline "white") (:background "grey50"))))
 '(sp-show-pair-match-face ((t (:inherit sp-show-pair-match-face :underline t :foreground "#16f7dd")))))
)
