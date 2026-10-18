#!/usr/bin/env python3
"""
Test Suite Runner for vale-rework
Runs all .vaf files through dist/vale-compiler and tests F* verification.
"""

import sys
import os
import glob
import time
import subprocess

RESET = "\033[0m"
BOLD = "\033[1m"
GREEN = "\033[32m"
RED = "\033[31m"
YELLOW = "\033[33m"
CYAN = "\033[36m"

def run_tests():
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    compiler = os.path.join(root, "bin", "vale-compiler")
    if not os.path.isfile(compiler):
        compiler = os.path.join(root, "dist", "vale-compiler")
    suite_dir = os.path.join(root, "tests", "suite")
    ext_fixtures_dir = os.path.join(os.path.dirname(root), "tests", "fixtures", "vale")
    local_fixtures_dir = os.path.join(root, "tests", "fixtures", "vale")
    out_dir = os.path.join(root, "dist", "test_out")
    os.makedirs(out_dir, exist_ok=True)

    if not os.path.isfile(compiler):
        print(f"{RED}Error: Compiler binary not found at {compiler}{RESET}")
        print("Please build it first.")
        sys.exit(1)

    suite_files = sorted(glob.glob(os.path.join(suite_dir, "*.vaf")))
    fixture_files = sorted(list(set(glob.glob(os.path.join(ext_fixtures_dir, "*.vaf")) + glob.glob(os.path.join(local_fixtures_dir, "*.vaf")))))
    all_files = fixture_files + suite_files

    print(f"{BOLD}{CYAN}=== Vale Modernized Test Suite Runner ==={RESET}")
    print(f"Discovered {len(all_files)} total test specifications across fixtures & suite:\n")

    passed = 0
    failed = 0
    start_total = time.time()

    for idx, path in enumerate(all_files, 1):
        rel = os.path.relpath(path, root)
        base = os.path.splitext(os.path.basename(path))[0]
        fst = os.path.join(out_dir, f"{base}.fst")
        fsti = os.path.join(out_dir, f"{base}.fsti")

        start = time.time()
        cmd = [compiler, "-in", path, "-out", fst, "-outi", fsti]
        res = subprocess.run(cmd, capture_output=True, text=True)
        dur = (time.time() - start) * 1000

        if res.returncode == 0:
            passed += 1
            print(f"  [{idx:2d}/{len(all_files):2d}] {GREEN}PASS{RESET} {rel:45s} ({dur:5.1f} ms)")
        else:
            failed += 1
            err = res.stderr.strip().splitlines()
            first_err = err[-1] if err else "unknown failure"
            print(f"  [{idx:2d}/{len(all_files):2d}] {RED}FAIL{RESET} {rel:45s} ({dur:5.1f} ms)")
            print(f"         {YELLOW}--> {first_err}{RESET}")

    total_dur = time.time() - start_total
    print(f"\n{BOLD}Test Summary:{RESET}")
    print(f"  Passed: {GREEN}{passed}{RESET} / {len(all_files)}")
    print(f"  Failed: {RED if failed else GREEN}{failed}{RESET} / {len(all_files)}")
    print(f"  Duration: {total_dur:.2f}s\n")

    if failed == 0:
        print(f"{BOLD}{GREEN}ALL TEST SUITE SPECIFICATIONS PASSED!{RESET}")
        return 0
    else:
        print(f"{BOLD}{RED}SOME TESTS FAILED!{RESET}")
        return 1

if __name__ == "__main__":
    sys.exit(run_tests())
