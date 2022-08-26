{ fetchgit, fetchurl, fetchFromGitHub }:
{
  agda-stdlib = {
    pname = "agda-stdlib";
    version = "69c93f587822acc69b5a1a123841a5c2420347d1";
    src = fetchFromGitHub ({
      owner = "agda";
      repo = "agda-stdlib";
      rev = "69c93f587822acc69b5a1a123841a5c2420347d1";
      fetchSubmodules = false;
      sha256 = "sha256-CT8EqYgNzEnAGI0mLwM1ZyKCHPcq4i9xnjCp+dJgdWI=";
    });
  };
}
