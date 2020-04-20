# Provide Nix / NixOS support by expressing system packages required, rather than manually having to install stuff like Kafka or PostgreSQL
# Inspired by https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
# Don't forget to add the following to your ~/.stack/config.yaml to enable Nix support
# nix:
#   enable: true
#   shell-file: stack-integration.nix
#   path:
#     - nixpkgs=/home/sir4ur0n/.nix-defexpr/channels/nixpkgs-unstable

{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [
    lzma
    zlib
  ];
}
