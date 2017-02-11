{ nixpkgs ? (import <nixpkgs> {})
, compiler ? "default"
}:

let
  reflex-unpatched = pkgs.fetchgit {
    url = "https://github.com/reflex-frp/reflex-platform";
    rev = "0de88ce3a17fc47353111150ba86a91159fe2cb6";
    fetchSubmodules = true;
    sha256 = "1l87svhfhkhyhb27qig927d5g4ddc1x3q7whr40r2y894wsaw5vr";
  };

  reflex = pkgs.runCommand "reflex" {} ''
    mkdir -p $out
    cp -R ${reflex-unpatched}/* $out
    sed -i '/doHaddock/d' "$out/default.nix"
    substituteInPlace "$out/default.nix" --replace "compilers/ghcjs\"" "compilers/ghcjs/base.nix\""
  '';

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages."${compiler}";

  reflex-platform = pkgs.callPackage reflex {
    nixpkgsFunc = import nixpkgs.pkgs.path;
    # inherit config;
  };

  inherit (pkgs) lib;
  inherit (lib) inNixShell;

  closureZip = pkgs.fetchzip {
    url = "http://dl.google.com/closure-compiler/compiler-20161201.tar.gz";
    sha256 = "1rnjvc7myz45gngpwxcfqksry76g63xqdj1bil23ccb4sc38zk74";
    stripRoot = false;
  };

  allSrc = filterHsSource ./.;
  backendSrc = "${allSrc}/backend";
  frontendSrc = "${allSrc}/frontend";

  filterHsSource = builtins.filterSource (path: type: !(
    baseNameOf path == ".git" ||
    baseNameOf path == "dist"
  ));

  nodePkgs = pkgs.callPackage ./frontend/generated/node-composition.nix {};

  backendBase = haskellPackages.callPackage (
    pkgs.stdenv.lib.overrideDerivation (reflex-platform.cabal2nixResult backendSrc) (drv: {
      ${if inNixShell then null else "buildCommand"} = ''
        cabal2nix file://"${backendSrc}" -fproduction >"$out"
      '';
    })
  ) {};
  frontendBase = with pkgs.haskell.lib; reflex-platform.ghcjs.callPackage
    (reflex-platform.cabal2nixResult frontendSrc) {};

  versionTag = lib.substring 0 7 (lib.commitIdFromGitRepo ./.git);

  backend = pkgs.haskell.lib.overrideCabal backendBase (drv: rec {
    configureFlags = (drv.configureFlags or []) ++ [ "-fproduction" ];
    src = allSrc;
    preCompileBuildDriver = "pushd backend";
    buildTools = (drv.buildTools or [])
      ++ lib.optionals inNixShell [
        pkgs.nodePackages.bower haskellPackages.cabal-install
      ];
    version = "${drv.version}-${versionTag}";
  });

  frontend = (pkgs.haskell.lib.overrideCabal frontendBase (drv: rec {
    configureFlags = (drv.configureFlags or []) ++ [ "-fproduction" ];
    src = allSrc;
    preCompileBuildDriver = "pushd frontend";
    buildTools = (drv.buildTools or [])
      ++ [ pkgs.openjdk pkgs.sass nodePkgs.cssnano-cli ];
    postInstall = ''
      cd $out/bin/frontend.jsexe
      for file in $(find . -name '*.js'); do
        echo >&2 "Minifying $file"
        cat "$file" | java -jar ${closureZip}/closure-compiler-v20161201.jar > "$file.tmp"
        mv "$file.tmp" "$file"
      done
    '';
    version = "${drv.version}-${versionTag}";
  })).overrideScope (self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      doHaddock = !(builtins.elem args.pname [ "MemoTrie" "fail" "text" "nats" ]);
    });
  });

in {
  inherit haskellPackages;

  inherit backend frontend;
}
