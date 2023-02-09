;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu))
(use-service-modules cups desktop networking ssh xorg)

(operating-system
  (locale "en_US.utf8")
  (timezone "America/Chicago")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "guixvm")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "ryan")
                  (comment "Ryan")
                  (group "users")
                  (home-directory "/home/ryan")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (specification->package "dmenu")
			  ;; xmonad and dependencies
                          (specification->package "xmonad-next")
                          (specification->package "ghc-xmonad-contrib-next")
                          (specification->package "xmobar")
                          (specification->package "ghc")
                          (specification->package "xmessage")
                          (specification->package "gcc")
                          (specification->package "gcc-toolchain")
                          (specification->package "binutils")
                          (specification->package "xterm")
                          ;; text editors
                          (specification->package "vim")
                          (specification->package "emacs")
			  ;; doom emacs dependencies
			  (specification->package "ripgrep")
			  (specification->package "fd")
			  ;; fonts
			  (specification->package "font-adobe-source-code-pro")
			  (specification->package "font-terminus")
			  (specification->package "xlsfonts"
			  ;; miscellaneous
                          (specification->package "neofetch")
			  (specification->package "cmatrix")
                          (specification->package "xrandr")
			  (specification->package "git")
                          (specification->package "nss-certs"))
			  (specification->package "picom")
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   %desktop-services)
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "09423284-684f-43aa-ad1f-c2f7330747d0")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "0c89ba6b-f561-47ca-91e0-2f610637bed1"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
