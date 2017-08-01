#! /bin/python3

import argparse
import os
import json
import subprocess
import sys

BA_PRODUCT_PATH = "/home/nephe/Documents/Research/dev/baProduct/main.native"
LTL2BA_PATH = "/home/nephe/Documents/Research/dev/ltl2ba/ltl2ba"
PROCESSOR = "clang"

TMP_FILE_FOLDER = "/home/nephe/Documents/Research/dev/baProduct/tmp/"
MERGE_FILE = os.path.join(TMP_FILE_FOLDER, "merge.c")
PREPROC_FILE = os.path.join(TMP_FILE_FOLDER, "preproc.c")

def main():
    parser = argparse.ArgumentParser(description='Instrument a program for LTL model checking')
    parser.add_argument("-s", "--spec", required=True, help="Specification file")
    parser.add_argument("-i", "--input", required=True, help="Input file")
    # parser.add_argument("--baproduct", dest="remainder", nargs=argparse.REMAINDER)

    args = parser.parse_args()

    with open(args.spec) as spec_file:
        spec = json.load(spec_file)

    ltl_formula = spec["ltl"]
    ltl_formula = ltl_formula.replace("G", "[]").replace("F", "<>")
    ltl_formula = "! ( " + ltl_formula + " )"

    if not os.path.exists(TMP_FILE_FOLDER):
        os.makedirs(TMP_FILE_FOLDER)

    print("Preprocessing the file...")
    preproc_process = subprocess.Popen([PROCESSOR, '-E', args.input, '-o', PREPROC_FILE])
    preproc_process.wait()

    print("Calling baProduct...")
    output = os.path.splitext(args.input)[0] + "_instr.c"
    baprod_process = subprocess.Popen(
        [BA_PRODUCT_PATH,
         '-i', PREPROC_FILE,
         '-s', args.spec,
         '-o', output,
         '--dot',
         '--tmp', TMP_FILE_FOLDER,
         '--ltl2ba', LTL2BA_PATH,
         ], stdout=sys.stdout)
    baprod_process.wait()
    print("Result printed to file {}".format(output))

if __name__ == "__main__":
    main()
