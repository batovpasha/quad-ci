# quad-ci

Own implementation of the Quad CI system described in ["The Simple Haskell Handbook"](https://leanpub.com/simple-haskell-book)

## Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/)
- [Node.js](https://nodejs.org/en) v14
- [Yarn](https://yarnpkg.com)
- [Docker](https://www.docker.com)

## Getting Started

```shell
$ stack build

# Run server
$ stack run -- start-server

# Run agent
$ stack run -- start-agent
```

To run a sample build:

```shell
$ curl -X POST -H "Content-Type: application/json" -d \
@test/github-payload.sample.json "http://localhost:9000/webhook/github"
```

Quad CI comes with a web UI, which can be accessed at http://localhost:3000.
To install and start it, run the following:

```shell
$ cd frontend/
$ yarn
$ yarn next
```

## Testing

1. Start the Docker daemon/Docker Desktop.
2. Run the following command:

```shell
stack test
```
