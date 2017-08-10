#! /bin/python3

import argparse
import os
import subprocess
from sys import stdout

TEST_FOLDER = "/home/nephe/Documents/Research/dev/baProduct/test/"
BAPRODUCT_SCRIPT = "/home/nephe/Documents/Research/dev/baProduct/script/baProduct.py"

def main():
    parser = argparse.ArgumentParser(description='Instrument a program for LTL model checking')
    parser.add_argument('testname', metavar='name', type=str, nargs='*',
                    help='Test files to run')
    parser.add_argument('--update', action='store_true',
                        help='Update oracles with current output')

    args = parser.parse_args()

    if not args.testname:
        args.testname = [name for name in os.listdir(TEST_FOLDER)
                         if os.path.isdir(os.path.join(TEST_FOLDER, name))
                         and name[0] != '_']

    n_errors = 0

    for test_name in args.testname:
        if not os.path.exists(os.path.join(TEST_FOLDER, test_name)):
            print("Invalid test folder: " + test_name)
            exit(1)

        print()
        if args.update:
            try:
                print("Updating oracle for test: " + test_name)
                subprocess.run(
                    ["cp",
                     os.path.join(TEST_FOLDER, test_name, test_name + "_instr.c"),
                     os.path.join(TEST_FOLDER, test_name, test_name + "_instr.oracle"),
                    ]).check_returncode()
            except subprocess.CalledProcessError:
                n_errors += 1
                print("**/!\\** Error while updating oracle: " + test_name)
        else:
            print("Running test: " + test_name)
            try:
                subprocess.run(
                    [BAPRODUCT_SCRIPT,
                     '-i', os.path.join(TEST_FOLDER, test_name, test_name + ".c"),
                     '-s', os.path.join(TEST_FOLDER, test_name, test_name + ".spec"),
                     '-t', os.path.join(TEST_FOLDER, test_name, "tmp")
                    ]).check_returncode()
            except subprocess.CalledProcessError:
                print("**/!\\** Error while running test: " + test_name)
                n_errors += 1
                break

            print("Comparing test {} with the oracle".format(test_name))
            try:
                subprocess.run(
                    ["diff",
                     os.path.join(TEST_FOLDER, test_name, test_name + "_instr.c"),
                     os.path.join(TEST_FOLDER, test_name, test_name + "_instr.oracle")
                    ], stdout=stdout).check_returncode()
            except subprocess.CalledProcessError:
                print("**/!\\** Error in diff: " + test_name)
                n_errors += 1

    print("-------------------------------------------")
    print("Errors: {}".format(n_errors))

if __name__ == "__main__":
    main()
