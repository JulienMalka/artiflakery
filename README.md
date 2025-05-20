# Artiflakery

Artiflakery is a webserver for on the fly delivery of flake artifacts. 

It allows you to define pairs such as `(route1, flakeref1)` such that upon loading `route1`, the default package of `flakeref1` gets served.
Each route can be public or limited to groups of authenticated users.

## 🔁 Auto reload

Upon load of a route, Artiflakery serves the last built output for the associated flakeref and launch an asynchroneous rebuild of the artifact. If an flakeref produces a different artifact, all the open pages for the associated route get reloaded.


# ⚙️ Configuration

Artiflakery is configured through 2 configuration files:

## 🛤️ Routes

Routes are defined in a textual file with lines of the format `route flakeref authgroup1 authgroup2 ... authgroupn`, for example:

```
/hello/world/ github:hello/world#test public
/foo/bar/ github:foo/bar?dir=test admins friends
/hello/test/ github:hello/test admins
```

## 🔓 Authentification groups

Authentification groups are defined in a textual file with lines of the format `groupname,username:bcrypt_hash`, where `username:bcrypt_hash` can be obtained by running `htpasswd -nB $user`.

The special `public` group can be assigned to resources that should be accessed publicly.


## Running the program

As a standalone, run `artiflakery --routes routes.txt --auth auth.txt`


# NixOS module

This repository exports a NixOS module for streamlined configuration and deployment of Artifactory. 

A typical configuration could look like this:

```nix
services.artiflakery = {
    enable = true;
    authFile = "/server/artiflakery-auth.txt";
    routes = {
      "/hello/world/" = {
        flakeref = "github:foo/bar?dir=test";
        access = [
          "admins"
          "friends"
        ];
      };
      "/foo/bar/" = {
        flakeref = "github:foo/bar?dir=test";
        access = [
          "public"
        ];
      };
    };
  };
```






