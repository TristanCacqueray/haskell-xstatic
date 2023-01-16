{
  description = "haskell-xstatic-demos";
  nixConfig.bash-prompt = "[nix(xstatic)] ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/3665c429d349fbda46b0651e554cca8434452748";
  };
  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      haskellOverrides = hpFinal: hpPrev:
        let mk = name: hpPrev.callCabal2nix name "${self}/${name}" { };
        in {
          xstatic = mk "xstatic";
          xstatic-htmx = mk "xstatic-htmx";
          xstatic-remixicon = mk "xstatic-remixicon";
          xstatic-sortable = mk "xstatic-sortable";
          xstatic-tailwind = mk "xstatic-tailwind";
          xstatic-xterm = mk "xstatic-xterm";
          xstatic-novnc = mk "xstatic-novnc";
          xstatic-th = mk "xstatic-th";
          lucid-xstatic = mk "lucid-xstatic";
          servant-xstatic = mk "servant-xstatic";

          demo-xstatic = mk "demo-xstatic";
          demo-websockets-ki-htmx = mk "demo-websockets-ki-htmx";
          demo-xterm = mk "demo-xterm";
          demo-novnc = mk "demo-novnc";
        };
      hspkgs = pkgs.haskell.packages.ghc925.override ({
        overrides = haskellOverrides;
      });
      mk-exe = pkgs.haskell.lib.justStaticExecutables;
    in {
      packages.x86_64-linux.default = mk-exe hspkgs.demo-websockets-ki-htmx;
      packages.x86_64-linux.demo-websockets-ki-htmx =
        mk-exe hspkgs.demo-websockets-ki-htmx;
      packages.x86_64-linux.demo = mk-exe hspkgs.demo-xstatic;
      packages.x86_64-linux.xterm = mk-exe hspkgs.demo-xterm;
      packages.x86_64-linux.novnc = mk-exe hspkgs.demo-novnc;
      devShells.x86_64-linux.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          (hspkgs.ghcWithPackages (p: [
            p.bytestring
            p.warp
            p.websockets
            p.network-run
            p.network
            p.ki
            p.servant-server
            p.servant-lucid
            p.tasty
            p.tasty-hunit
            p.servant-websockets
            p.lucid
            p.lucid-htmx
            p.string-interpolate
            p.relude
            p.lens
            p.lens-aeson
            p.stm
            p.posix-pty
          ]))
          hpack
          ghcid
          hspkgs.cabal-install
          esbuild
          (haskell-language-server.override {
            supportedGhcVersions = [ "925" ];
          })
        ];
      };
    };
}
