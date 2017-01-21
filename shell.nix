{ nixpkgs ? (import <nixpkgs> {})
, compiler ? "default"
}:

let pkg = import ./default.nix { inherit nixpkgs compiler; };

in nixpkgs.pkgs.stdenv.mkDerivation {
  name = "smash.gg-env";
  buildInputs = pkg.backend.env.nativeBuildInputs ++ pkg.frontend.env.nativeBuildInputs;
  shellHook = ''
    pushd backend >/dev/null
  '' + pkg.backend.env.shellHook + ''
    popd >/dev/null
    pushd frontend >/dev/null
  '' + pkg.frontend.env.shellHook + ''
    popd >/dev/null
  '';
}
