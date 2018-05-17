# crawfish

## Installation

1. Install a Java JDK
2. Install Leiningen
3. Clone the repository

## Usage

```
$ lein run -- [opts] site-root #  `--` tells leiningen to pass args through to -main

```

Example:

```
lein run -- -d tree daveduthie.github.io
```

After building a jar (`lein uberjar`), you can run

```
$ java -jar target/uberjar/crawfish-0.1.0-standalone.jar [args]
```

which will shave a bit off boot time.

## Docs

They're [here](docs/uberdoc.html).

## License

Copyright © 2018 David Duthie

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
