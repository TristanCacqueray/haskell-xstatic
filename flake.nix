{
  description = "haskell-xstatic-demos";
  nixConfig.bash-prompt = "[nix(xstatic)] ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/b79cc961fe98b158ea051ae3c71616872ffe8212";
  };
  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      haskellOverrides = hpFinal: hpPrev:
        let mk = name: hpPrev.callCabal2nix name "${self}/${name}" { };
        in {
          xstatic = mk "xstatic";
          xstatic-ace = mk "xstatic-ace";
          xstatic-htmx = mk "xstatic-htmx";
          xstatic-remixicon = mk "xstatic-remixicon";
          xstatic-sortable = mk "xstatic-sortable";
          xstatic-tailwind = mk "xstatic-tailwind";
          xstatic-xterm = mk "xstatic-xterm";
          xstatic-novnc = mk "xstatic-novnc";
          xstatic-th = mk "xstatic-th";
          xstatic-quill = mk "xstatic-quill";
          lucid-xstatic = mk "lucid-xstatic";
          servant-xstatic = mk "servant-xstatic";

          ot = hpPrev.callCabal2nix "ebml" (pkgs.fetchFromGitHub {
            owner = "TristanCacqueray";
            repo = "ot.hs";
            rev = "daa7dc081389dab4f567b5b75121743c0a67c93a";
            sha256 = "sha256-wBsMnXPjKAiKBgXI84NEb6fgvTBGey7+Wp+C3IpvY/c=";
          }) { };

          demo-xstatic = mk "demo-xstatic";
          demo-websockets-ki-htmx = mk "demo-websockets-ki-htmx";
          demo-xterm = mk "demo-xterm";
          demo-novnc = mk "demo-novnc";
          demo-quill-ot = mk "demo-quill-ot";
        };
      hspkgs = pkgs.haskell.packages.ghc964.override ({
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
            p.lucid2
            p.lucid-htmx
            p.string-interpolate
            p.relude
            p.lens
            p.lens-aeson
            p.stm
            p.posix-pty
            p.ot
          ]))
          hpack
          ghcid
          hspkgs.cabal-install
          esbuild
          (haskell-language-server.override {
            supportedGhcVersions = [ "964" ];
          })
        ];
      };
    };
}
