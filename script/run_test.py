#! /bin/python3

import argparse
import os
import subprocess

TEST_FOLDER = "/home/nephe/Documents/Research/dev/baProduct/test/"
BAPRODUCT_SCRIPT = "/home/nephe/Documents/Research/dev/baProduct/script/baProduct.py"

def main():
    parser = argparse.ArgumentParser(description='Instrument a program for LTL model checking')
    parser.add_argument('testname', metavar='name', type=str, nargs='*',
                    help='Test files to run')

    args = parser.parse_args()

    if not args.testname:
        args.testname = [name for name in os.listdir(TEST_FOLDER)
                         if os.path.isdir(os.path.join(TEST_FOLDER, name))
                         and name[0] != '_']

    for test_name in args.testname:
        if not os.path.exists(os.path.join(TEST_FOLDER, test_name)):
            print("Invalid test folder : " + test_name)
            exit(1)

        print("Running test : " + test_name)
        test_process = subprocess.Popen(
            [BAPRODUCT_SCRIPT,
                '-i', os.path.join(TEST_FOLDER, test_name, test_name + ".c"),
                '-s', os.path.join(TEST_FOLDER, test_name, test_name + ".spec"),
                '-t', os.path.join(TEST_FOLDER, test_name, "tmp")
            ])
        test_process.wait()

if __name__ == "__main__":
    main()
