#!/bin/bash
#
# Test SSE macro functionality with multiple sequential connections.
# Multiple connections ensure proper worker/connection cleanup (issue with Woo).

set -e

# This should be higher than the :body-interval defined in the
# test-sse endpoint (which is set at 1). The reason is that for
# Clack+woo, we are closing the stream when the socket was closed
# during send-event.

timeout_interval=2


# Function to test SSE endpoint with multiple sequential connections
# Args: $1=port, $2=backend_name, $3=output_prefix
test_sse_endpoint() {
  local port=$1
  local name=$2
  local output_prefix=$3
  local connections=3  # Test 3 sequential connections

  echo ""
  echo "Testing $name with-sse-connection macro (port $port)..."
  echo "  Running $connections sequential connections to verify worker cleanup..."

  for i in $(seq 1 $connections); do
    local output_file="/tmp/${output_prefix}-conn${i}.txt"

    timeout ${timeout_interval} curl -N http://localhost:$port/test-sse 2>&1 | head -n 20 > "$output_file" || true

    if grep -q "datastar-patch-signals" "$output_file" && grep -q "time" "$output_file"; then
      echo "  Connection $i: OK"
    else
      echo "  Connection $i: FAILED"
      echo "ERR $name SSE test failed on connection $i"
      echo "Output:"
      cat "$output_file"
      exit 1
    fi
  done

  echo "OK $name SSE macro works (all $connections connections succeeded)"
}

echo 
echo "* Testing SSE Macro with All Backends *"
echo ""
echo "Note: Testing multiple sequential connections ensures proper cleanup"
echo "      (prevents worker thread leaks that cause connection hangs)"
echo
echo "Timeout:" ${timeout_interval} "s"
# Test all backends
test_sse_endpoint 7331 "Hunchentoot" "ht-sse"
test_sse_endpoint 7332 "Clack+Woo" "clack-woo-sse"
test_sse_endpoint 7333 "Clack+Hunchentoot" "clack-ht-sse"

# If we're here, nothing failed
echo ""
echo "* All SSE Tests Passed *"
echo ""
