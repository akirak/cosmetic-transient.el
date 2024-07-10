rice-flake := "github:emacs-twist/rice-config"

melpa := "github:akirak/melpa/akirak"

common-options := "--override-input rice-src \"path:$PWD\" --override-input melpa " + quote(melpa)

emacs-version := "emacs-release-snapshot"

package := "cosmetic-transient"

arch := shell('nix eval --expr builtins.currentSystem --impure --raw')

# Show the flake
show:
    nix flake show {{ rice-flake }} {{ common-options }}

# Enter a shell for byte-compiling individual source files
shell-compile:
    nix develop {{ rice-flake }}\#{{ emacs-version }}-for-{{ package }} {{ common-options }}

# Re-run byte-compile every time a file is modified
watch-compile:
    nix develop {{ rice-flake }}\#{{ emacs-version }}-for-{{ package }} {{ common-options }} -c bash -c 'echo >&2 Watching *.el; echo *.el | entr -p elisp-byte-compile /_'

# Byte-compile the package
check-compile:
    nix build {{ rice-flake }}\#checks.{{ arch }}.{{ package }}-compile-{{ emacs-version }} {{ common-options }}

# Enter a shell for running tests
shell-emacs:
    nix shell {{ rice-flake }}\#{{ emacs-version }}-with-packages {{ common-options }}
