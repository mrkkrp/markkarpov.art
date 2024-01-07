# Mark Karpov's artistic web site

![CI](https://github.com/mrkkrp/markkarpov.art/workflows/CI/badge.svg?branch=master)

To build the site:

```shell
$ nix build .#site
```

The site will be in `result/`, you'll need to start an HTTP server to browse
it locally.

For interactive editing of articles:

```shell
$ ./watch.sh
```

## License

Copyright © 2023–present Mark Karpov

All rights reserved.
