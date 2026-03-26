#!/bin/bash
 
# Usage: ./test_p1.sh <your_executable>
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <your_executable>"
    exit 1
fi
 
EXE=$1
TEST_DIR="temp_tests"
mkdir -p "$TEST_DIR"
 
# Comprehensive test suite: Basic, Laws, Contradictions, and Edge Cases
inputs=(
    "Const True"
    "Const False"
    "Not (Const True)"
    "Not (Const False)"
    "Var \"x\""
    "And (Var \"p\") (Not (Var \"p\"))"
    "Or (Var \"p\") (Not (Var \"p\"))"
    "Imply (Var \"p\") (Var \"p\")"
    "Iff (Var \"p\") (Var \"p\")"
    "Not (Iff (Var \"p\") (Var \"p\"))"
    "Imply (Const True) (Const False)"
    "Imply (Const False) (Const True)"
    "And (Var \"a\") (And (Var \"b\") (Not (Var \"a\")))"
    "Iff (Not (And (Var \"x\") (Var \"y\"))) (Or (Not (Var \"x\")) (Not (Var \"y\")))"
    "Iff (Not (Or (Var \"x\") (Var \"y\"))) (And (Not (Var \"x\")) (Not (Var \"y\")))"
    "Imply (And (Var \"p\") (Imply (Var \"p\") (Var \"q\"))) (Var \"q\")"
    "And (Imply (Var \"p\") (Var \"q\")) (And (Var \"p\") (Not (Var \"q\")))"
    "Iff (And (Var \"a\") (Or (Var \"b\") (Var \"c\"))) (Or (And (Var \"a\") (Var \"b\")) (And (Var \"a\") (Var \"c\")))"
    "Imply (And (Imply (Var \"p\") (Var \"q\")) (Imply (Var \"q\") (Var \"r\"))) (Imply (Var \"p\") (Var \"r\"))"
    "Imply (Imply (Imply (Var \"p\") (Var \"q\")) (Var \"p\")) (Var \"p\")"
    "And (Or (Var \"x\") (Var \"y\")) (And (Not (Var \"x\")) (Not (Var \"y\")))"
    "Or (Var \"a\") (Or (Var \"b\") (Or (Var \"c\") (Not (Var \"a\"))))"
    "Not (Imply (Var \"p\") (Or (Var \"p\") (Var \"q\")))"
    "Iff (Var \"a\") (Iff (Var \"b\") (Var \"a\"))"
    "And (Const True) (And (Var \"x\") (Not (Var \"x\")))"
)
 
expected=(
    "SAT" "UNSAT" "UNSAT" "SAT" "SAT" "UNSAT" "SAT" "SAT" "SAT" "UNSAT" 
    "UNSAT" "SAT" "UNSAT" "SAT" "SAT" "SAT" "UNSAT" "SAT" "SAT" "SAT" 
    "UNSAT" "SAT" "UNSAT" "SAT" "UNSAT"
)
 
echo "--- Starting Expanded Logic Solver Test Suite ---"
passed=0
total=${#inputs[@]}
 
for ((i=0; i<$total; i++)); do
    current_input="${inputs[$i]}"
    current_expected="${expected[$i]}"
 
    input_file="$TEST_DIR/test_$i.input"
    echo "$current_input" > "$input_file"
 
    # Run and normalize output
    actual=$("./$EXE" "$input_file" 2>/dev/null | tr -d '[:space:]' | tr '[:lower:]' '[:upper:]')
 
    if [ "$actual" == "$current_expected" ]; then
        echo -e "\033[32m[PASS]\033[0m $current_input"
        ((passed++))
    else
        echo -e "\033[31m[FAIL]\033[0m $current_input"
        echo "       Expected: $current_expected, but got: '$actual'"
    fi
done
 
rm -rf "$TEST_DIR"
 
echo "--------------------------------------------------"
if [ $passed -eq $total ]; then
    echo -e "\033[32mALL TESTS PASSED ($passed/$total)\033[0m"
else
    echo -e "\033[31mSOME TESTS FAILED ($passed/$total)\033[0m"
fi