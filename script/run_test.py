#! /bin/python3

import argparse
import logging
import os
import subprocess
import shutil
from sys import stdout

TEST_FOLDER = "/home/nephe/Documents/Research/dev/baProduct/test/"
BAPRODUCT_SCRIPT = "/home/nephe/Documents/Research/dev/baProduct/script/baProduct.py"

BACKENDS = ["esbmc", "cbmc"]

n_errors = 0

def update_oracles(test_names, backends):
    """
    Update oracles with the current generated file for test cases
    """
    global n_errors

    for test_name in test_names:
        logging.info("Updating oracles for test: " + test_name)
        for backend in backends:
            try:
                shutil.copy(
                    os.path.join(TEST_FOLDER, test_name, "{}_{}_instr.c"
                                 .format(test_name, backend)),
                    os.path.join(TEST_FOLDER, test_name, "{}_{}_instr.oracle"
                                 .format(test_name, backend))
                )
            except OSError:
                n_errors += 1
                logging.error("Fail to update oracle %s_%s_instr.oracle",
                              test_name, backend)

def clean_tests(test_names):
    """
    Clear the test folder from temporary file
    """

    global n_errors
    for test_name in test_names:
        logging.info("Clearing temporary file for test: " + test_name)
        if os.path.exists(os.path.join(TEST_FOLDER, test_name, "tmp")):
            try:
                shutil.rmtree(os.path.join(TEST_FOLDER, test_name, "tmp"))
            except OSError:
                n_errors += 1
                logging.error("Fail to delete temporary folder for test %s",
                              test_name)

        # Clear unexected files in the test folder (with confirmation)
        expected_files = [test_name + ".c", test_name + ".spec"] + \
        ["{}_{}_instr.oracle".format(test_name, b) for b in BACKENDS] + \
        ["{}_{}_instr.c".format(test_name, b) for b in BACKENDS] + \
        ["%s_cmd.txt" % b for b in BACKENDS]

        to_delete = [name for name in
                     os.listdir(os.path.join(TEST_FOLDER, test_name))
                     if name not in expected_files]

        for fname in to_delete:
            print("Unexpected file {}. Delete it ? [y/N]".format(fname))
            if input().strip().lower() == "y":
                try:
                    os.remove(os.path.join(TEST_FOLDER, test_name, fname))
                except OSError:
                    n_errors += 1
                    logging.error("Fail to delete the file %s", fname)

def run_tests(test_names, backends):
    """
    Run tests cases for specified tests and backends
    """
    global n_errors

    for test_name in test_names:
        for backend in backends:
            logging.info("Running test %s for backend %s", test_name, backend)
            try:
                subprocess.run(
                    [BAPRODUCT_SCRIPT,
                     '-i', os.path.join(TEST_FOLDER, test_name, test_name + ".c"),
                     '-s', os.path.join(TEST_FOLDER, test_name, test_name + ".spec"),
                     '-t', os.path.join(TEST_FOLDER, test_name, "tmp"),
                     '-o', os.path.join(TEST_FOLDER, test_name,
                             "{}_{}_instr.c".format(test_name, backend)),
                     '--baproduct', '-m', backend
                    ]).check_returncode()
            except subprocess.CalledProcessError:
                n_errors += 1
                logging.error("Fail to run the test %s for backend %s",
                              test_name, backend)
                continue

            check_diff(
                os.path.join(TEST_FOLDER, test_name,
                             "{}_{}_instr.c".format(test_name, backend)),
                os.path.join(TEST_FOLDER, test_name,
                             "{}_{}_instr.oracle".format(test_name, backend))
                )

def check_diff(test_file, oracle_file):
    """
    Compare a test file with an oracle
    """
    global n_errors

    logging.info("Comparing test %s with the oracle", test_file)
    try:
        subprocess.run(["diff", test_file, oracle_file],
                       stdout=stdout).check_returncode()
    except subprocess.CalledProcessError:
        n_errors += 1
        logging.error("Fail to compare %s with the oracle %s",
                      test_file, oracle_file)

def main():
    logging.basicConfig(level=logging.INFO,
                         format="%(levelname)s -- %(message)s")

    # Parsing of command line argument
    parser = argparse.ArgumentParser(description='Instrument a program for LTL model checking')
    parser.add_argument('testname', metavar='name', type=str, nargs='*',
                    help='Test files to run')
    parser.add_argument('-u', '--update', action='store_true',
                        help='Update oracles with current output')
    parser.add_argument('-c', '--clear', action='store_true',
                        help='Update oracles with current output')
    parser.add_argument('-b', '--backend', help='Backend to run the test for')

    args = parser.parse_args()

    # If no test name is provided, collect all test names in the test folder
    if not args.testname:
        args.testname = [name for name in os.listdir(TEST_FOLDER)
                         if os.path.isdir(os.path.join(TEST_FOLDER, name))
                         and name[0] != '_']

    # If no backend is provided, use all backends
    if not args.backend:
        args.backend = BACKENDS
    else:
        args.backend = [args.backend]

    for test_name in args.testname:
        if not os.path.exists(os.path.join(TEST_FOLDER, test_name)):
            logging.error("Invalid test folder: " + test_name)
            exit(1)

    print()
    if args.update:
        update_oracles(args.testname, args.backend)
    elif args.clear:
        clean_tests(args.testname)
    else:
        run_tests(args.testname, args.backend)

    print("-------------------------------------------")
    print("Errors: {}".format(n_errors))

if __name__ == "__main__":
    main()
