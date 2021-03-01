<p align="center"><img src="https://reasonml.github.io/img/reason.svg" alt="logo" width="316" /></p>
<h1 align="center">Reason</h1>
<p align="center">Simple, fast & type safe code that leverages the JavaScript & OCaml ecosystems.</p>
<p align="center">
  <a href="https://dev.azure.com/reasonml/reason/_build/latest?definitionId=3&branchName=master">
    <img src="https://dev.azure.com/reasonml/reason/_apis/build/status/reasonml.reason?branchName=master" alt="Build Status" />
  </a>
  <a href="https://circleci.com/gh/reasonml/reason/tree/master">
    <img src="https://circleci.com/gh/reasonml/reason/tree/master.svg?style=svg" alt="CircleCI" />
  </a>
  <a href="https://discord.gg/reasonml">
    <img src="https://img.shields.io/discord/235176658175262720.svg?logo=discord&colorb=blue" alt="Chat" />
  </a>
</p>


## Latest Releases:

[![native esy package on npm][reason-badge]](https://www.npmjs.com/package/@esy-ocaml/reason)

## User Documentation

**The Reason user docs live online at [https://reasonml.github.io](https://reasonml.github.io)**.
The repo for those Reason docs lives at [github.com/reasonml/reasonml.github.io](https://github.com/reasonml/reasonml.github.io)

Docs links for new users:

- [Getting Started](https://reasonml.github.io/docs/en/installation)

- [Community](https://reasonml.github.io/docs/en/community.html)

### Contributing:

```sh
npm install -g esy@next
git clone https://github.com/facebook/reason.git
cd reason
esy
esy test # Run the tests
```

### Contributor Documentation:

The [`docs/`](./docs/) directory in this repo contains documentation for
contributors to Reason itself (this repo).


## License

See Reason license in [LICENSE.txt](LICENSE.txt).

Works that are forked from other projects are under their original licenses.

## Credit

The general structure of `refmt` repo was copied from [whitequark's m17n project](https://github.com/whitequark/ocaml-m17n), including parts of the `README` that instruct how to use this with the OPAM toolchain. Thank you OCaml!

[reason]: https://www.npmjs.com/package/@reason-native/console
[reason-badge]: https://img.shields.io/npm/v/@esy-ocaml/reason/latest.svg?color=blue&label=@esy-ocaml/reason&style=flat&logo=data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAzOTcgNDE3IiB3aWR0aD0iMzk3IiBoZWlnaHQ9IjQxNyI+PGcgZmlsbD0iI0ZDRkFGQSI+PHBhdGggZD0iTTI2Ny42NDYgMTQyLjk4MmwzOS42MTYtMjIuOTQ2TDI2Ny41ODMgOTcuMmwtMzkuNjE2IDIyLjk0NiAzOS42NzkgMjIuODM2em0tNjkuMzI4IDQwLjEyOWwzOS42MTYtMjIuOTQ1LTM5LjY3OS0yMi44MzYtMzkuNjE2IDIyLjk0NiAzOS42NzkgMjIuODM1em0tNjkuNDM5LTQwLjEzbDM5LjYxNi0yMi45NDVMMTI4LjgxNiA5Ny4yIDg5LjIgMTIwLjE0NmwzOS42NzkgMjIuODM1em02OS4zMjgtMzkuOThsMzkuNjE2LTIyLjk0NS0zOS42NzktMjIuODM2LTM5LjYxNiAyMi45NDYgMzkuNjc5IDIyLjgzNXoiLz48cGF0aCBkPSJNMTkuODU2IDEzNy41OTFsMTY4LjYzOCA5Ny4wNTEuMjA2IDE0OC43ODlMMjAuMDYzIDI4Ni4zOGwtLjIwNy0xNDguNzg5ek0xOTguMTEyIDIyLjg5bDE2OC42MzcgOTcuMDUyLTE2OC4zNjcgOTcuNTE5TDI5Ljc0NCAxMjAuNDFsMTY4LjM2OC05Ny41MnptMTc4LjU3MyAxMTQuMjA2bC4yMDcgMTQ4Ljc4OS0xNjguMzY4IDk3LjUxOS0uMjA2LTE0OC43ODkgMTY4LjM2Ny05Ny41MTl6TTE5OC4wOCAwTDAgMTE0LjcyOGwuMjU1IDE4My4xMjUgMTk4LjM5NyAxMTQuMTc4IDE5OC4wOC0xMTQuNzI4LS4yNTUtMTgzLjEyNUwxOTguMDggMHoiLz48L2c+PC9zdmc+Cg== "esy package on npm"
