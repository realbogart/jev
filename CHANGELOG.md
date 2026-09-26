# Changelog

## 0.1.1.0

- Accept probabilities rounded to two decimals, as returned by OpenRouter.
  Distribution sums now tolerate `0.005` per returned probability, the selected
  Choice may trail another option by a rounded tie (`0.01`), and Score checks
  allow for rounded probabilities. Values are still not renormalized; clearly
  invalid distributions are still rejected.
- Add `renderJevError`, a log-friendly rendering of `JevError` with the
  constructor, status code, request ID, and message or truncated body. Unlike
  `show`, it never includes response headers such as `Set-Cookie`.

## 0.1.0.0

- Enforce `timeoutMicros` across the complete HTTP operation, including reading
  the response body. Validation and JSON decoding remain outside the deadline.
- Add structured transport failures. `TransportError Text` becomes
  `TransportError TransportFailure`; total deadline expiry is `DeadlineExceeded`.
- Change `HttpError Int body` to `HttpError ResponseMetadata body`. Read the
  status from `metadata.statusCode`; headers and request IDs are retained.
- Add `ResponseDecodeError ResponseMetadata Text` for HTTP decoding failures.
  `DecodeError Text` is used by the pure fixture decoder.
- Reject contradictory distributions and scores using documented rounding
  tolerances, and require legends to cover returned probability indices.
- Add pure question validation, request inspection, and fixture decoding.
- Add optional OpenRouter routing and observability settings through
  `decideWith`, `decideJSONWith`, and `prepareRequestWith`.
- Add `Functor` instances for value-carrying public data types.
- Document public API contracts and consumer installation; add package bounds,
  license text, and isolated source-distribution verification.
- Move Nix tooling overrides to `cabal.project.nix`; ordinary Cabal builds now
  use a minimal project file.
