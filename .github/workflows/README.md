# GitHub Actions CI/CD Workflow

## Overview

This workflow runs automated tests for the datastar-cl SDK on every commit, pull request, and monthly schedule.

## Workflow: `sdk-test.yml`

### What It Does

1. **Installs dependencies**
   - Roswell (Common Lisp implementation manager)
   - SBCL (Steel Bank Common Lisp)
   - golang-go (required for the official Datastar SDK conformance suite)

2. **Runs unit tests** -- pure function/macro tests, no servers, fast

3. **Runs integration tests** -- FiveAM suite that starts the test servers
   internally, exercises SSE, CQRS, compression, and the official SDK
   conformance suite, then stops the servers

### Triggers

- **Push**: Every commit to any branch
- **Pull Request**: When PRs are opened or updated
- **Manual**: Via GitHub Actions UI (`workflow_dispatch`)
- **Scheduled**: First day of each month at midnight UTC

### How It Works

```
Unit tests (no servers) -> Integration tests (servers managed internally)
```

Both test stages run via FiveAM loaded through Roswell:

- Unit stage: `(asdf:test-system :datastar-cl/tests/unit)` -- no I/O, completes in seconds
- Integration stage: `datastar-cl/tests/integration:run-all-tests` -- starts servers on
  ports 7331 (Hunchentoot) and 7333 (Clack+Hunchentoot), runs all HTTP suites, stops
  servers on exit

### Key Files

- `.github/workflows/sdk-test.yml` -- GitHub Actions workflow definition
- `tests/unit/` -- FiveAM unit test suite (`datastar-cl/tests/unit`)
- `tests/integration/` -- FiveAM integration test suite (`datastar-cl/tests/integration`)
- `tests/fixtures/*.lisp` -- Server endpoint fixtures (used by the integration suite)

### Running Locally

```lisp
;; Unit tests only (fast, no servers needed)
(asdf:test-system :datastar-cl/tests/unit)
;; or
(fiveam:run! :datastar-cl/core)

;; Integration tests (starts servers automatically)
(asdf:test-system :datastar-cl/tests/integration)
;; or
(datastar-cl/tests/integration:run-all-tests)

;; Everything
(fiveam:run! :datastar-cl)
```

### Troubleshooting

**Unit tests fail:**
- Check SBCL/Roswell installation
- Verify `ql:quickload :datastar-cl` completes cleanly

**Integration tests fail to start servers:**
- Check that ports 7331 and 7333 are not already in use
- Load `datastar-cl-tests` manually and call `start-all-servers` to debug

**SDK conformance tests fail:**
- Check that Go is properly installed and in PATH
- Verify internet connectivity (downloads the Datastar test binary)
- Review test output for specific ADR violations

**SSE/CQRS tests fail:**
- Restart servers and re-run: `(datastar-cl/tests/integration:run-all-tests)`
- Check server logs for exceptions or connection errors

### Monitoring

Scheduled runs help detect:
- Breaking changes in dependencies (SBCL, libraries)
- Breaking changes in the upstream Datastar SDK test suite
- Bit rot or platform issues
