#!/usr/bin/env bash

# Advanced Boolean Proposition Parser Test Script
# Usage: ./test_parser.sh ./parser_program

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <parser_program>"
    exit 1
fi

PARSER="$1"

if [ ! -x "$PARSER" ]; then
    echo "Error: '$PARSER' is not executable"
    exit 1
fi

PASS=0
FAIL=0

run_test() {
    local name="$1"
    local input="$2"
    local expected="$3"
	local actual

    tmp_in=$(mktemp)
    tmp_out=$(mktemp)
    tmp_exp=$(mktemp)

    echo "$input" > "$tmp_in"
    echo "$expected" > "$tmp_exp"

    "$PARSER" "$tmp_in" > "$tmp_out" 2>&1
	actual=$(cat "$tmp_out")

	if [ "$actual" = "$expected" ]; then
        echo "[PASS] $name"
        PASS=$((PASS + 1))
    else
        echo "[FAIL] $name"
        echo "Input: $input"
        echo "------ Expected ------"
        cat "$tmp_exp"
        echo "------ Got -----------"
        cat "$tmp_out"
        echo "----------------------"
        FAIL=$((FAIL + 1))
    fi
	echo "----------------------"

    rm "$tmp_in" "$tmp_out" "$tmp_exp"
}

########################################
# RIGHT ASSOCIATIVITY TESTS
########################################

run_test "Right-assoc implication" \
"a -> b -> c" \
"Imply (Var \"a\") (Imply (Var \"b\") (Var \"c\"))"

run_test "Right-assoc biconditional" \
"a <-> b <-> c" \
"Iff (Var \"a\") (Iff (Var \"b\") (Var \"c\"))"

run_test "Right-assoc disjunction" \
"a \\/ b \\/ c" \
"Or (Var \"a\") (Or (Var \"b\") (Var \"c\"))"

run_test "Right-assoc conjunction" \
"a /\\ b /\\ c" \
"And (Var \"a\") (And (Var \"b\") (Var \"c\"))"

########################################
# PRECEDENCE TESTS
########################################

run_test "Conjunction binds tighter than disjunction" \
"a \\/ b /\\ c" \
"Or (Var \"a\") (And (Var \"b\") (Var \"c\"))"

run_test "Implication lower than disjunction" \
"a \\/ b -> c" \
"Imply (Or (Var \"a\") (Var \"b\")) (Var \"c\")"

run_test "Biconditional lowest precedence" \
"a -> b <-> c" \
"Iff (Imply (Var \"a\") (Var \"b\")) (Var \"c\")"

run_test "Negation binds tightly" \
"!a /\\ b" \
"And (Not (Var \"a\")) (Var \"b\")"

run_test "Multiple negations" \
"!!!F" \
"Not (Not (Not (Const False)))"

########################################
# PARENTHESES OVERRIDE
########################################

run_test "Parentheses override precedence" \
"(a \\/ b) /\\ c" \
"And (Or (Var \"a\") (Var \"b\")) (Var \"c\")"

run_test "Nested parentheses" \
"(a -> (b \\/ c)) /\\ d" \
"And (Imply (Var \"a\") (Or (Var \"b\") (Var \"c\"))) (Var \"d\")"

run_test "Deep nesting" \
"((T))" \
"Const True"

########################################
# COMPLEX EXPRESSIONS
########################################

run_test "Complex 1" \
"!a /\\ b -> c \\/ d <-> e" \
"Iff (Imply (And (Not (Var \"a\")) (Var \"b\")) (Or (Var \"c\") (Var \"d\"))) (Var \"e\")"

run_test "Complex 2" \
"a -> b /\\ c -> d \\/ e" \
"Imply (Var \"a\") (Imply (And (Var \"b\") (Var \"c\")) (Or (Var \"d\") (Var \"e\")))"

run_test "Complex 3" \
"! (a <-> b) \\/ c /\\ !d" \
"Or (Not (Iff (Var \"a\") (Var \"b\"))) (And (Var \"c\") (Not (Var \"d\")))"

########################################
# VARIABLE EDGE CASES
########################################

run_test "Alphanumeric variable" \
"a1" \
"Var \"a1\""

run_test "Long variable name" \
"abc123xyz" \
"Var \"abc123xyz\""

########################################
# INVALID INPUT TESTS
########################################

run_test "Invalid uppercase variable start" \
"Avar" \
"Parse Error"

run_test "Invalid token" \
"a && b" \
"Parse Error"

run_test "Missing operand" \
"a ->" \
"Parse Error"

run_test "Unbalanced parentheses" \
"(a \\/ b" \
"Parse Error"

run_test "Empty parentheses" \
"()" \
"Parse Error"

run_test "Operator without lhs" \
"-> a" \
"Parse Error"

########################################

echo "=================================="
echo "Passed: $PASS"
echo "Failed: $FAIL"
echo "=================================="

if [ $FAIL -ne 0 ]; then
    exit 1
fi
