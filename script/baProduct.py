#! /bin/python3

"""
This script provided a simplified interface to baProduct.
It preprocess a file, correct the syntax of LTL formulas and set options in
a convenient way.
"""

import argparse
import os
import json
import subprocess
import sys

BA_PRODUCT_PATH = "/home/nephe/Documents/Research/dev/baProduct/main.native"
LTL2BA_PATH = "/home/nephe/Documents/Research/dev/ltl2ba/ltl2ba"
PROCESSOR = "clang"

TMP_FILE_FOLDER_DEFAULT = "/home/nephe/Documents/Research/dev/baProduct/tmp/"

def main():
    parser = argparse.ArgumentParser(description='Instrument a program for LTL model checking')
    parser.add_argument("-s", "--spec", required=True, help="Specification file")
    parser.add_argument("-i", "--input", required=True, help="Input file")
    parser.add_argument("-t", "--tmp_folder", help="Temporary file folder")
    parser.add_argument("--baproduct", dest="remainder", nargs=argparse.REMAINDER)

    args = parser.parse_args()

    if not args.remainder:
        args.remainder = []

    if not args.tmp_folder:
        args.tmp_folder = TMP_FILE_FOLDER_DEFAULT
    if not os.path.exists(args.tmp_folder):
        os.makedirs(args.tmp_folder)

    preproc_file = os.path.join(args.tmp_folder, "preproc.c")

    print("Preprocessing the file...")
    try:
        subprocess.run([PROCESSOR, '-E', args.input, '-o', preproc_file]).check_returncode()
    except subprocess.CalledProcessError:
        print("Error while preprocessing the file.")
        exit(1)

    print("Calling baProduct...")
    output = os.path.splitext(args.input)[0] + "_instr.c"
    try:
        subprocess.run(
            [BA_PRODUCT_PATH,
             '-i', preproc_file,
             '-s', args.spec,
             '-o', output,
             '--dot',
             '--tmp', args.tmp_folder,
             '--ltl2ba', LTL2BA_PATH,
            ] + args.remainder, stdout=sys.stdout
        ).check_returncode()
    except subprocess.CalledProcessError:
        print("Error while instrumenting the file.")
        exit(1)

    print("Result printed to file {}".format(output))

if __name__ == "__main__":
    main()
