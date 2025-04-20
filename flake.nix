{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in rec {
        lib.mkPackage = ({ epkgs }: (epkgs.trivialBuild rec {
          pname = "yapilot";
          version = "v1.0.1";
          src = ./.;
          packageRequires = with epkgs; [llm markdown-mode];
          buildInputs = packageRequires;
        }));

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.ollama
            ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: [
              epkgs.llm
              epkgs.markdown-mode
              (lib.mkPackage { inherit epkgs; })
            ]))
          ];
        };
      });
}
