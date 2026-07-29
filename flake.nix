{
  description = "Mark Karpov's artistic web site";
  inputs = {
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      # prevent nix-direnv from fetching stackage
      inputs.stackage.url = "github:input-output-hk/empty-flake";
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs, haskellNix }:
    let
      system = "x86_64-linux";
      compiler = "ghc9122";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      haskellNixPkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [ haskellNix.overlay ];
      };
      hsProject = haskellNixPkgs.haskell-nix.cabalProject {
        src = pkgs.lib.sourceByRegex ./. [
          "^app.*$"
          "^lib.*$"
          "^markkarpov-art\.cabal$"
          "^cabal\.project$"
          "^README\.md$"
        ];
        compiler-nix-name = compiler;
      };
      hsPkgs = hsProject.hsPkgs;
      mk-art = hsPkgs.markkarpov-art.components.exes.mk-art;

      siteSourceRegex = [
        "^art-per-year.*$"
        "^artworks\.yaml$"
        "^contact\.md$"
        "^env\.yaml$"
        "^essay.*$"
        "^exhibitions\.yaml$"
        "^robots\.txt$"
        "^templates.*$"
      ];
      mkSite = doCheck: isPreview: pkgs.stdenv.mkDerivation {
        name = "mk-art";
        buildInputs = [
          mk-art
          pkgs.glibcLocales
          pkgs.validator-nu
          pkgs.zlib
        ];
        LANG = "en_US.UTF-8";
        src = pkgs.lib.sourceByRegex ./. siteSourceRegex;
        buildPhase = ''
          mk-art
          mkdir -p _build/static/css
          cp ${styles}/css/styles.css _build/static/css/styles.css
        '' + (if isPreview
        then ''
          echo 'User-agent: *' > _build/robots.txt
          echo 'Disallow: /' >> _build/robots.txt
        ''
        else "cp robots.txt _build/robots.txt");
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
      styles = pkgs.stdenv.mkDerivation {
        name = "mk-art-styles";
        src = pkgs.lib.sourceByRegex ./. [
          "^styles.*$"
          "^templates.*$"
          "^lib.*$"
        ];
        nativeBuildInputs = [ pkgs.tailwindcss_4 ];
        buildPhase = ''
          tailwindcss \
            --input styles/app.css \
            --output styles.css \
            --cwd . \
            --minify
        '';
        installPhase = ''
          mkdir -p "$out/css"
          cp styles.css "$out/css/styles.css"
        '';
      };
    in
    rec {
      inherit compiler styles;
      netlify-cli = pkgs.netlify-cli;
      app = mk-art;
      site = mkSite true false;
      site-quick = mkSite false false;
      site-preview = mkSite true true;
      defaultPackage.x86_64-linux = site;
      apps.x86_64-linux.netlify = {
        type = "app";
        program = "${pkgs.netlify-cli}/bin/netlify";
      };
    };
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://markkarpov-sites.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "markkarpov-sites.cachix.org-1:tzrAG4NHl/VkbtjotbuQJ7kCSaq/dkzj2IaSUgxo4Gs="
    ];
  };
}
