let
  config = 
  {
    packageOverrides = pkgs: 
    {
      haskellPackages = pkgs.haskellPackages.override 
      {
        overrides = new: old:
        {
          interview = new.callPackage ./default.nix;
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs;
    [ cabal-install
      interview
      zlib
      haskell-language-server
    ]
  );
in

with pkgs;

mkShell 
{ buildInputs = [ ghc ];
}
