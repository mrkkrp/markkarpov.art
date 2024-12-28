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
      mkSite = doCheck: isPreview: pkgs.stdenv.mkDerivation {
        name = "mk-art";
        buildInputs = [
          haskellPackages.markkarpov-art
          pkgs.glibcLocales
          pkgs.validator-nu
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
          vnu --version
          vnu --skip-non-html --Werror --verbose _build/
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
