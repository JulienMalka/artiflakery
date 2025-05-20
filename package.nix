{
  mkDerivation,
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
}:
mkDerivation {
  pname = "artiflakery";
  version = "1.0.0";
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
  ];
  doCheck = false;
  homepage = "https://github.com/JulienMalka/artiflakery#readme";
  license = "EUPL-1.2";
  mainProgram = "artiflakery-exe";
}
