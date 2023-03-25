{ pkgs, ... }:

{
  packages = [ pkgs.git pkgs.haskellPackages.hlint ];

  enterShell = ''
    ghc --version
    git --version
  '';

  languages.haskell.enable = true;
}
