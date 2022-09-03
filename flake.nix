{
  description = "haskell-xstatic-demos";
  nixConfig.bash-prompt = "[nix(xstatic)] ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/00d73d5385b63e868bd11282fb775f6fe4921fb5";
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

          demo-xstatic = mk "demo-xstatic";
        };
      hspkgs = pkgs.haskell.packages.ghc924.override ({
        overrides = haskellOverrides;
      });
      mk-exe = pkgs.haskell.lib.justStaticExecutables;
    in {
      packages.x86_64-linux.demo = mk-exe hspkgs.demo-xstatic;
      devShells.x86_64-linux.default =
        hspkgs.shellFor { packages = p: [ p.demo-xstatic ]; };
    };
}
