#!/bin/bash
#
# Run official Datastar SDK compliance tests against all backend configurations.
# Can be run locally or in CI/CD.
#
# Usage:
#   ./run-sdk-tests.sh              # Test all ports (7331, 7332, 7333)
#   SDK_TEST_PORTS="7331" ./run-sdk-tests.sh  # Test specific port(s)

set -e

# Default to all three backend configurations
PORTS="${SDK_TEST_PORTS:-7331 7332 7333}"

# Backend names for display
declare -A BACKEND_NAMES
BACKEND_NAMES[7331]="Hunchentoot"
BACKEND_NAMES[7332]="Clack+Woo"
BACKEND_NAMES[7333]="Clack+Hunchentoot"

echo ""
echo "* Running Datastar SDK Compliance Tests *"
echo ""

for port in $PORTS; do
  backend="${BACKEND_NAMES[$port]:-Unknown}"
  echo "Testing $backend on port $port..."

  if ! go run github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@latest -server http://localhost:$port; then
    echo ""
    echo "ERR SDK compliance test failed for $backend (port $port)"
    exit 1
  fi

  echo "OK $backend passed SDK compliance tests"
  echo ""
done

echo "* All SDK Compliance Tests Passed *"
echo ""
