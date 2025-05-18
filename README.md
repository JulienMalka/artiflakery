# Artiflakery

Artiflakery is a webserver for on the fly delivery of flake artifacts.


It allows you to define `routes` associated with `flakeref` like so:

```
/hello/world/ -> github:hello/world#test
/foo/bar/ -> github:foo/bar?dir=test
```

When loading these routes, the artifacts for those flakerefs are served from the build cache, and rebuilt if necessary. Artiflakery also supports automatic page reloading, when an artifact is updated.

It is also possible to guard certain routes under HTTP basic auth, if some artifacts are meant to be shared with a restricted audience.
