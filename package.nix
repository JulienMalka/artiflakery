{
  mkDerivation,
  aeson,
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
  filepath,
  hslogger,
  http-types,
  lib,
  lifted-async,
  monad-control,
  mtl,
  process,
  text,
  time,
  transformers,
  unix,
  unliftio-core,
  wai,
  wai-app-static,
  wai-websockets,
  warp,
  websockets,
}:
mkDerivation {
  pname = "artiflakery";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
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
    filepath
    hslogger
    http-types
    lifted-async
    monad-control
    mtl
    process
    text
    time
    transformers
    unix
    unliftio-core
    wai
    wai-app-static
    wai-websockets
    warp
    websockets
  ];
  executableHaskellDepends = [
    aeson
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
    filepath
    hslogger
    http-types
    lifted-async
    monad-control
    mtl
    process
    text
    time
    transformers
    unix
    unliftio-core
    wai
    wai-app-static
    wai-websockets
    warp
    websockets
  ];
  doCheck = false;
  homepage = "https://github.com/JulienMalka/artiflakery#readme";
  license = lib.licenses.bsd3;
  mainProgram = "artiflakery-exe";
}
