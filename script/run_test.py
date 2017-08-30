#! /bin/python3

import argparse
import os
import subprocess
from sys import stdout

TEST_FOLDER = "/home/nephe/Documents/Research/dev/baProduct/test/"
BAPRODUCT_SCRIPT = "/home/nephe/Documents/Research/dev/baProduct/script/baProduct.py"

def main():
    # Parsing of command line argument
    parser = argparse.ArgumentParser(description='Instrument a program for LTL model checking')
    parser.add_argument('testname', metavar='name', type=str, nargs='*',
                    help='Test files to run')
    parser.add_argument('-u', '--update', action='store_true',
                        help='Update oracles with current output')
    parser.add_argument('-c', '--clear', action='store_true',
                        help='Update oracles with current output')

    args = parser.parse_args()

    # If no test name is provided, collect all test names in the test folder
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
            # Update the oracle with the current generated file

            print("Updating oracle for test: " + test_name)
            try:
                subprocess.run(
                    ["cp",
                     os.path.join(TEST_FOLDER, test_name, test_name + "_instr.c"),
                     os.path.join(TEST_FOLDER, test_name, test_name + "_instr.oracle"),
                    ]).check_returncode()
            except subprocess.CalledProcessError:
                n_errors += 1
                print("**/!\\** Error while updating oracle: " + test_name)
        elif args.clear:
            # Clear the test folder from temporary file

            print("Clearing temporary file for test: " + test_name)
            if os.path.exists(os.path.join(TEST_FOLDER, test_name, "tmp")):
                try:
                    subprocess.run(
                        ["rm", "-r", os.path.join(TEST_FOLDER, test_name, "tmp")]
                    ).check_returncode()
                except subprocess.CalledProcessError:
                    n_errors += 1
                    print("**/!\\** Error while clearing temporary files for test: " + test_name)


            # Clear unexected files in the test folder (with confirmation)
            expected_files = [
                test_name + ".c",
                test_name + ".spec",
                test_name + "_instr.c",
                test_name + "_instr.oracle",
                "esbmc_cmd.txt"
            ]
            to_delete = [name for name in os.listdir(os.path.join(TEST_FOLDER, test_name))
                         if name not in expected_files]

            for fname in to_delete:
                print("Unexpected file {}. Delete it ? [y/N]".format(fname))
                if input().strip().lower() == "y":
                    try:
                        subprocess.run(["rm", fname]).check_returncode()
                    except subprocess.CalledProcessError:
                        n_errors += 1
                        print("**/!\\** Error while clearing file: " + fname)

        else:
            # Run the test

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
