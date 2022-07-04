{ lib, mkDerivation,
    base, HUnit, parsec, optparse-applicative, terminal-size
}:
mkDerivation {
  pname = "SummarizeSSHKeys";
  version = "1.0.0.0";
  src = lib.cleanSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base  HUnit  parsec  optparse-applicative  terminal-size
  ];
  homepage = "https://github.com/neilmayhew/SummarizeSSHKeys";
  description = "A utility to produce readable summaries of SSH authorized_keys files";
  license = lib.licenses.mit;
}
