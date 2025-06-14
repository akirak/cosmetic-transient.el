# Update this if you have forked rice-config
rice-config := "github:emacs-twist/rice-config"

# Specify a flake reference to a repository and branch where the package recipe
# is defined.
melpa := "github:akirak/melpa/akirak"

# Relative path to the lock directory
lock-dir := ".rice-lock/default"

# This is only to avoid repetition, and you usually don't edit this.
common-options-without-lock := "--override-input rice-src \"path:$(readlink $PWD)\" --override-input melpa " + quote(melpa)

package := "cosmetic-transient"
common-options-with-lock := common-options-without-lock + " --override-input rice-lock \"path:$(readlink $PWD)/" + lock-dir + "\""

# The name of an Emacs package from nix-emacs-ci
emacs := "emacs-snapshot"

# Don't edit this
arch := shell('nix eval --expr builtins.currentSystem --impure --raw')

# Show the flake
show *OPTIONS:
    nix flake show {{ rice-config }} {{ OPTIONS }} {{ common-options-with-lock }}

# Evaluate an attribute on the flake, e.g. just eval melpaRecipes.
eval ATTR *OPTIONS:
    nix eval {{rice-config}}\#{{ATTR}} {{OPTIONS}} {{ common-options-with-lock }}

# Generate a lock directory.
lock *OPTIONS:
    mkdir -p "$(dirname {{ lock-dir }})"
    nix run "{{ rice-config }}?dir=make-lock#lock-with-{{ emacs }}" {{ common-options-without-lock }} --impure -- {{ OPTIONS }} {{ lock-dir }}

# Enter a shell for byte-compiling individual source files
shell-compile:
    nix develop {{ rice-config }}\#{{ emacs }}-for-{{ package }} {{ common-options-with-lock }}

# Re-run byte-compile every time a file is modified
watch-compile:
    nix develop {{ rice-config }}\#{{ emacs }}-for-{{ package }} {{ common-options-with-lock }} -c bash -c 'echo >&2 Watching *.el; ls *.el | entr -p elisp-byte-compile /_'

# Byte-compile the package
check-compile:
    nix build {{ rice-config }}\#checks.{{ arch }}.{{ package }}-compile-{{ emacs }} {{ common-options-with-lock }} --print-build-logs

# Enter a shell for running tests
shell-emacs *OPTIONS:
    nix shell {{ rice-config }}\#{{ emacs }}-with-packages {{ common-options-with-lock }} {{ OPTIONS }}

test-buttercup *OPTIONS:
    nix run {{ rice-config }}\#test-buttercup-with-{{ emacs }} {{ common-options-with-lock }} {{ OPTIONS }}
