# GitHub Actions CI/CD Workflow

## Overview

This workflow runs automated tests for the datastar-cl SDK on every commit, pull request, and monthly schedule.

## Workflow: `sdk-test.yml`

### What It Does

1. **Installs dependencies**
   - Roswell (Common Lisp implementation manager)
   - SBCL (Steel Bank Common Lisp)
   - libev-dev (required for Woo server)
   - golang-go (required for official Datastar test suite)

2. **Starts test servers** (in background)
   - Port 48331: Hunchentoot backend
   - Port 48332: Clack+Woo backend
   - Port 48333: Clack+Hunchentoot backend

3. **Waits for servers** to be ready (health checks on all 3 ports)

4. **Runs SDK compliance tests** using official Datastar test suite

5. **Runs SSE macro tests** to verify long-lived connections work correctly

6. **Cleans up** (kills SBCL process)

### Triggers

- **Push**: Every commit to any branch
- **Pull Request**: When PRs are opened or updated
- **Manual**: Via GitHub Actions UI (`workflow_dispatch`)
- **Scheduled**: First day of each month at midnight UTC

### How It Works

The workflow uses a **simple sequential approach**:

```
Start SBCL with servers → Wait for health checks → Run tests → Cleanup
```

This is simpler than previous approaches that used complex third-party actions or Docker containers.

### Key Files

- `.github/workflows/sdk-test.yml` - GitHub Actions workflow definition
- `tests/run.lisp` - Script that loads the system and starts all servers
- `tests/run-sdk-tests.sh` - Shell script to run official Datastar SDK tests
- `tests/run-sse-tests.sh` - Shell script to test SSE connection macros

### Running Locally

To reproduce the CI environment locally:

```bash
# Start servers (in background)
sbcl --non-interactive --load tests/run.lisp &
SBCL_PID=$!

# Wait for servers
for port in 48331 48332 48333; do
  while ! curl -f -s http://localhost:$port/ > /dev/null 2>&1; do
    echo "Waiting for port $port..."
    sleep 2
  done
done

# Run tests
./tests/run-sdk-tests.sh
./tests/run-sse-tests.sh

# Cleanup
kill $SBCL_PID
```

### Troubleshooting

**Servers fail to start:**
- Check SBCL logs in GitHub Actions output
- Ensure all dependencies are installed (libev-dev for Woo)
- Verify ports 48331-48333 are not already in use

**Health check timeout:**
- Default timeout is 60 seconds (30 checks × 2 seconds)
- Check if `tests/run.lisp` properly calls `start-all-servers`
- Verify each server has a root endpoint that responds

**SDK tests fail:**
- Check if Go is properly installed
- Verify internet connectivity (downloads Datastar test suite)
- Review test output for specific failures

**SSE tests fail:**
- Ensure `curl` is available
- Check timeout settings in `run-sse-tests.sh`
- Verify servers are properly handling SSE connections

### Design Decision: Why This Approach?

We chose this **simple sequential approach** over alternatives because:

✅ **Simple**: No complex third-party actions or Docker containers
✅ **Transparent**: Easy to understand and debug
✅ **Reproducible**: Same commands work locally and in CI
✅ **Fast**: No image building or complex setup
✅ **Maintainable**: Standard shell commands, no hidden complexity

Other approaches considered (and rejected):
- **Docker containers**: Overkill, slower, more complex
- **Background action**: Third-party dependency, harder to debug
- **Service containers**: Requires image registry, complex setup
- **Matrix testing**: Higher CI costs, unnecessary parallelization

### Monitoring

View workflow runs at: `https://github.com/YOUR_ORG/datastar-cl/actions`

Scheduled runs help detect:
- Breaking changes in dependencies (SBCL, libraries)
- Breaking changes in Datastar test suite
- Bit rot or platform issues
