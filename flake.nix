{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = with pkgs; [
            watchexec
            elmPackages.elm
            elmPackages.elm-live
            elmPackages.elm-git-install
            elmPackages.elm-review
            elmPackages.elm-test
            elmPackages.elm-language-server
          ];
        };
      }
    );
}
