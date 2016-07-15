{ mkDerivation, base, reflex, reflex-dom, reflex-dom-contrib
, stdenv
}:
mkDerivation {
  pname = "triage-tree";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base reflex reflex-dom reflex-dom-contrib
  ];
  license = stdenv.lib.licenses.unfree;
}
