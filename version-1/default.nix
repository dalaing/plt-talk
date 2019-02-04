{ nixpkgs ? import <nixpkgs> {}} :
let
  inherit (nixpkgs) pkgs;
  mytexlive = (pkgs.texlive.combine {
    inherit (pkgs.texlive)
    prftree
    ulem
    minted
    fvextra
    upquote
    ifplatform
    xstring
    framed
    mdframed
    needspace
    scheme-basic
    collection-latexrecommended
    collection-mathscience;
  });
in
  pkgs.stdenv.mkDerivation {
    name = "plt-talk";
    src = ./.; # TODO filter source

    buildPhase = ''
      pdflatex --shell-escape $src/slides.tex
    '';

    buildInputs = [pkgs.pandoc mytexlive pkgs.python36Packages.pygments];
  }
