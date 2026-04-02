#!/usr/bin/env bash

CHECKER="$1"

if [ -z "$CHECKER" ]; then
  echo "Usage: $0 <type-checker-executable>"
  exit 1
fi

TMP_FILE=$(mktemp)
PASS=0
FAIL=0

run_test() {
  local name="$1"
  local program="$2"
  local expected="$3"

  echo "$program" > "$TMP_FILE"
  output=$("$CHECKER" "$TMP_FILE" 2>&1)

  if [ "$output" == "$expected" ]; then
    echo "[PASS] $name"
    PASS=$((PASS + 1))
  else
    echo "[FAIL] $name"
    echo "  Program:  $program"
    echo "  Expected: $expected"
    echo "  Got:      $output"
    FAIL=$((FAIL + 1))
  fi
}

########################################
# TEST CASES
########################################

# Constants
run_test "int constant" \
  "CInt 5" \
  "TInt"

run_test "bool constant" \
  "CBool True" \
  "TBool"

# Arithmetic
run_test "simple addition" \
  "Plus (CInt 1) (CInt 2)" \
  "TInt"

run_test "addition type error" \
  "Plus (CInt 1) (CBool True)" \
  "Type Error"

run_test "minus valid" \
  "Minus (CInt 5) (CInt 3)" \
  "TInt"

# Equality
run_test "equal ints" \
  "Equal (CInt 1) (CInt 2)" \
  "TBool"

run_test "equal mismatch" \
  "Equal (CInt 1) (CBool True)" \
  "Type Error"

# If-then-else
run_test "valid if" \
  "ITE (CBool True) (CInt 1) (CInt 2)" \
  "TInt"

run_test "if condition error" \
  "ITE (CInt 1) (CInt 2) (CInt 3)" \
  "Type Error"

run_test "if branch mismatch" \
  "ITE (CBool True) (CInt 1) (CBool False)" \
  "Type Error"

# Variables (should fail if unbound)
run_test "unbound variable" \
  "Var \"x\"" \
  "Type Error"

# Lambda
run_test "identity function" \
  "Abs \"x\" TInt (Var \"x\")" \
  "TArr TInt TInt"

run_test "const function" \
  "Abs \"x\" TInt (CInt 5)" \
  "TArr TInt TInt"

# Application
run_test "apply identity" \
  "App (Abs \"x\" TInt (Var \"x\")) (CInt 1)" \
  "TInt"

run_test "apply type mismatch" \
  "App (Abs \"x\" TInt (Var \"x\")) (CBool True)" \
  "Type Error"

run_test "apply non-function" \
  "App (CInt 5) (CInt 1)" \
  "Type Error"

# Nested functions
run_test "curried function" \
  "Abs \"x\" TInt (Abs \"y\" TBool (Var \"x\"))" \
  "TArr TInt (TArr TBool TInt)"

# Let bindings
run_test "simple let" \
  "LetIn \"x\" TInt (CInt 1) (Var \"x\")" \
  "TInt"

run_test "let type mismatch" \
  "LetIn \"x\" TInt (CBool True) (Var \"x\")" \
  "Type Error"

run_test "let shadowing" \
  "LetIn \"x\" TInt (CInt 1) (LetIn \"x\" TBool (CBool True) (Var \"x\"))" \
  "TBool"

# Higher-order
run_test "function returning function applied" \
  "App (App (Abs \"x\" TInt (Abs \"y\" TInt (Var \"x\"))) (CInt 1)) (CInt 2)" \
  "TInt"

########################################

echo "----------------------"
echo "Passed: $PASS"
echo "Failed: $FAIL"
echo "----------------------"

rm "$TMP_FILE"

if [ "$FAIL" -ne 0 ]; then
  exit 1
fi

exit 0
