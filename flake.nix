{
  description = "Mark Karpov's artistic web site";
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
  };
  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        config.allowUnfree = true;
      };
      compiler = "ghc982";
      appSourceRegex = [
        "^app.*$"
        "^lib.*$"
        "^markkarpov-art\.cabal$"
        "^README\.md$"
      ];
      siteSourceRegex = [
        "^art-per-year.*$"
        "^artworks\.yaml$"
        "^contact\.md$"
        "^env\.yaml$"
        "^essay.*$"
        "^exhibitions\.yaml$"
        "^templates.*$"
      ];
      haskellPackages = pkgs.haskell.packages.${compiler}.override
        {
          overrides = (self: super: {
            "markkarpov-art" = super.callCabal2nix "markkarpov-art"
              (pkgs.lib.sourceByRegex ./. appSourceRegex)
              { };
          });
        };
      html5validator = with pkgs.python39Packages; buildPythonPackage rec {
        pname = "html5validator";
        version = "0.4.0";
        src = fetchPypi {
          inherit pname version;
          sha256 = "sha256-PObj5zbJx7N+XibrFzugd3rgB3ZUm9YIkz+hSVUmDUk=";
        };
        propagatedBuildInputs = [
          pkgs.openjdk
          pyyaml
        ];
        doCheck = false;
        meta = {
          description = "Command line tool that tests files for HTML5 validity";
          homepage = https://github.com/svenkreiss/html5validator;
          license = pkgs.lib.licenses.mit;
        };
      };
      mkSite = doCheck: isPreview: pkgs.stdenv.mkDerivation {
        name = "mk-art";
        buildInputs = [
          haskellPackages.markkarpov-art
          html5validator
          pkgs.glibcLocales
          pkgs.zlib
        ];
        LANG = "en_US.UTF-8";
        src = pkgs.lib.sourceByRegex ./. siteSourceRegex;
        buildPhase = ''
          mk-art
          echo 'User-agent: *' > _build/robots.txt
        '' + (if isPreview
        then ''
          echo 'Disallow: /' >> _build/robots.txt
        ''
        else "");
        inherit doCheck;
        checkPhase = ''
          html5validator --version
          html5validator --root _build/ --show-warnings --ignore "This document appears to be written in"
        '';
        installPhase = ''
          mkdir "$out"
          cp -r _build/. "$out/"
        '';
      };
    in
    rec {
      inherit compiler;
      netlify-cli = pkgs.netlify-cli;
      app = haskellPackages.markkarpov-art;
      site = mkSite true false;
      site-quick = mkSite false false;
      site-preview = mkSite true true;
      defaultPackage.x86_64-linux = site;
      apps.x86_64-linux.netlify = {
        type = "app";
        program = "${pkgs.netlify-cli}/bin/netlify";
      };
    };
}
