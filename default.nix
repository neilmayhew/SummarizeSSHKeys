{ stdenv, mkDerivation,
    base, HUnit, parsec
}:
mkDerivation {
  pname = "SummarizeSSHKeys";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base  HUnit  parsec
  ];
  homepage = "https://github.com/neilmayhew/SummarizeSSHKeys";
  description = "A utility to produce readable summaries of SSH authorized_keys files";
  license = stdenv.lib.licenses.mit;
}