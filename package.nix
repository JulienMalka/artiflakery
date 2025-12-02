{
  mkDerivation,
  buildNpmPackage,
  makeWrapper,
  ansi-terminal,
  async,
  base,
  base64-bytestring,
  bcrypt,
  bytestring,
  co-log,
  co-log-core,
  containers,
  cookie,
  crypton,
  directory,
  exceptions,
  file-embed,
  filepath,
  http-types,
  optparse-applicative,
  process,
  text,
  time,
  unix,
  wai,
  wai-app-static,
  wai-websockets,
  warp,
  websockets,
  unliftio,
}:
let
  staticAssets = buildNpmPackage {
    pname = "artiflakery-static";
    version = "1.0.0";
    src = ./static;
    npmDepsHash = "sha256-KcCaJ+1fTadpw8QTdN09I2nLnrEkr/Ypu/TJBCiBIn0=";
    dontNpmBuild = true;
    installPhase = ''
      mkdir -p $out
      cp -r node_modules $out/
      cp -r pdfjs $out/
      cp *.html $out/
    '';
  };
in
mkDerivation {
  pname = "artiflakery";
  version = "1.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal
    async
    base
    base64-bytestring
    bcrypt
    bytestring
    co-log
    co-log-core
    containers
    cookie
    crypton
    directory
    exceptions
    file-embed
    filepath
    http-types
    optparse-applicative
    process
    text
    time
    unix
    wai
    wai-app-static
    wai-websockets
    warp
    websockets
    unliftio
  ];
  executableHaskellDepends = [
    ansi-terminal
    async
    base
    base64-bytestring
    bcrypt
    bytestring
    co-log
    co-log-core
    containers
    cookie
    crypton
    directory
    exceptions
    file-embed
    filepath
    http-types
    optparse-applicative
    process
    text
    time
    unix
    wai
    wai-app-static
    wai-websockets
    warp
    websockets
    unliftio
  ];
  doCheck = false;
  buildTools = [ makeWrapper ];
  postInstall = ''
    mkdir -p $out/share/artiflakery
    cp -r ${staticAssets}/* $out/share/artiflakery/

    wrapProgram $out/bin/artiflakery-exe \
      --set-default ARTIFLAKERY_STATIC_DIR "$out/share/artiflakery"
  '';
  homepage = "https://github.com/JulienMalka/artiflakery#readme";
  license = "EUPL-1.2";
  mainProgram = "artiflakery-exe";
}
