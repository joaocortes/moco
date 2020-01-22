__author__="Andreia P. Guerreiro"
__copyright__ = "Copyright 2019 (C) Andreia P. Guerreiro"
__license__ = "GNU General Public License, version 3"
__version__ = "0.1"

import numpy as np
import argparse
import sys

from utils import listInputFiles, createFolder, getBaseName

# -------------------------------------------------------------------------------- #
# ----------------------- Parse arguments ---------------------------------------- #
# -------------------------------------------------------------------------------- #


def readArguments():

    probchoices = ["KS-CP16", "SC-CP16", "SP-CP16"]

    parser = argparse.ArgumentParser(
        description="Encode Multiobjective Optimization (MO) problems as Pseudo-Boolean"
        "Multiobjective Optimization (PBMO) problems."
        "Available Problems and formats:"
        "  - KnapSack problem (KS-CP16)"
        "  - Set Covering problem (SC-CP16)"
        "  - Set Packing problem (SP-CP16)"
        )
    parser.add_argument("-p", type=str, required=True, dest="probtype", choices=probchoices, help="Problem type and format")
    parser.add_argument("-f", type=str, dest="fname", help="input file")
    parser.add_argument("-i", type=str, dest="inFolder", help="input folder (used only if flag -f is not used)")
    parser.add_argument("-o", type=str, dest="outFolder", default="garbage", help="output folder")

    args = parser.parse_args(sys.argv[1:])
    print ("args: %r\n" % args)


    if args.fname is None and args.inFolder is None:
        print("Either -f or -i must be used")
        exit()

    ls = [args.fname] if args.fname is not None else listInputFiles(args.inFolder)
    createFolder(args.outFolder)

    return ls, args.probtype, args.outFolder



# -------------------------------------------------------------------------------- #
# ----------------------- Convert functions -------------------------------------- #
# -------------------------------------------------------------------------------- #

def convert2PBMO(fname, probtype):

    if probtype.startswith("KS-"):
        return convert_KS(fname, probtype)
    if probtype.startswith("SC-"):
        return convert_SC(fname, probtype)
    if probtype.startswith("SP-"):
        return convert_SP(fname, probtype)

# --------- Knapsack (KS) --------------------- #

def convert_KS(fname, probtype):
    if probtype == "KS-CP16":
        p, w, W = read_KS_CP16(fname)

    d = len(p)
    n = len(p[0])
    nc = len(w)
    minmax = ["max"] * d
    objfs_c = p
    objfs_v = (np.arange(n)+1)[np.newaxis,:].repeat(d, axis=0)
    cs_c = w
    cs_v = (np.arange(n)+1)[np.newaxis,:].repeat(nc, axis=0)
    cs_s = ["<="] * nc
    cs_b = [W]

    return minmax, objfs_c, objfs_v, cs_c, cs_v, cs_s, cs_b


"""
    Returns (MO) knapsack instance (p, w, W)
    p is a (d x n)-profit matrix (n itens, d objectives)
    w is a (1 x n)-weight vector
    W is the KS capacity
"""
def read_KS_CP16(fname):
    with open(fname, "r") as f:
        n = int(f.readline())
        d = int(f.readline())

        p = np.empty((d, n), dtype=int)
        w = np.empty((1, n), dtype=int)

        for i in range(d):
            p[i] = map(int, f.readline().split())
        w[0] = np.array(map(int, f.readline().split()))

        W = int(f.readline())

        return p, w, W



# --------- Set Covering (SC) ----------------- #

def convert_SC(fname, probtype):
    if probtype == "SC-CP16":
        costs, belongs = read_SC_CP16(fname)

    d = len(costs)
    n = len(costs[0])
    m = len(belongs)

    objfs_m = ["min"] * d
    objfs_c = np.array(costs)
    objfs_v = (np.arange(n)+1)[np.newaxis,:].repeat(d, axis=0)
    cs_v = np.array(belongs)
    cs_c = np.array([np.ones(len(c), dtype=int) for c in belongs])
    cs_s = [">="] * m
    cs_b = [1] * m

    return objfs_m, objfs_c, objfs_v, cs_c, cs_v, cs_s, cs_b


"""
    Returns (MO) set covering (c A)
    c is a (d x n)-cost matrix (n itens, d objectives)
    A is a (m x n)-constraint matrix (m constraints)
"""
def read_SC_CP16(fname):

    f = open(fname, "r")
    a = map(int, f.readline().split())
    if len(a) < 3:
        a.append(int(f.readline()))

    m, n, d = a
    costs = []
    belongs = []

    for i in range(d):
        costs.append(np.array(map(int, f.readline().split())))

    for i in range(n):
        f.readline()
        belongs.append(np.array(map(int, f.readline().split())))

    return costs, belongs



# --------- Set Packing (SP) ------------------ #

def convert_SP(fname, probtype):
    if probtype == "SP-CP16":
        _, objfs_c, objfs_v, cs_c, cs_v, _, cs_b = convert_SC(fname, "SC-CP16")

        #same as SC-CP16, except it maximizes objective functions, and constraints are "<="
        objfs_m = ["max"] * len(objfs_c)
        cs_s = ["<="] * len(cs_c)

    return objfs_m, objfs_c, objfs_v, cs_c, cs_v, cs_s, cs_b



#def read_SP_CP16(fname):

    #return read_SC_CP16(fname)



# --------- Independent Set (IS) -------------- #


# -------------------------------------------------------------------------------- #
# ----------------------- Save to File ------------------------------------------- #
# -------------------------------------------------------------------------------- #

""" Convert list of coeffs and indexes into a string representing the corresponding linear function """
def linearFuncStr(coeffs, varix):
    return " ".join(map(lambda (c, i): ("+" if c >= 0 else "")+str(c)+" x"+str(i), zip(coeffs, varix)))


"""
    Saves PBMO problem (of n variables, d objectives, and nc constraints) to file.
    Input:
        objfs_m:    d-dimensional vector with elements in {"min", "max"}, i-th elements indicates
                    whether objective i is to be minmized or maximized
        objfs_c:    list of d vectors, the i-th vector contains the coeffients of objective function i
        objfs_v:    list of d vectors, the i-th vector contains the literal indexes of objective function i
        cs_c, cs_v: lists of nc vectors, analogous to objfs_c and objfs_v, respectively
        cs_s:       (nc)-vector with constraint signs (">=", "<=", "=")
        cs_b        (nc)-vector with constraint bounds
    TODO: Fazer a versao que, para cada objectivo e ca restricao, recebe um vector de indices e um vector
        de coeficientes (pois a versao actual nao e muito eficiente se a matriz dos objectivos e/ou
        das restricoes e esparsa)
"""
def savePBMOToFile(objfs_m, objfs_c, objfs_v, cs_c, cs_v, cs_s, cs_b, saveTo, comments=None, minversion=True):

    f = open(saveTo, "w")
    if comments is not None:
        f.write("*******************************************************************************\n")
        for line in comments:
            f.write("* %s\n" % line)

        s = set()
        for i in range(len(objfs_v)):
            s = s.union(objfs_v[i])
        nvars = len(s)
        f.write("* %d objectives\n" % len(objfs_m))
        f.write("* %d variables\n" % nvars)
        f.write("* %d constraints\n" % len(cs_c))
        f.write("*******************************************************************************\n")
        f.write("\n")

    d = len(objfs_c)
    nc = len(cs_c)

    for di in range(d):
        ix = np.where(objfs_c[di] != 0)
        if minversion and objfs_m[di] == "max":
            f.write("min: %s ;\n" % (linearFuncStr(-objfs_c[di][ix], objfs_v[di][ix])))
        else:
            f.write("%s: %s ;\n" % (objfs_m[di], linearFuncStr(objfs_c[di][ix], objfs_v[di][ix])))
    f.write("\n")
    for ci in range(nc):
        ix = np.where(cs_c[ci] != 0)[0]
        f.write("%s %s %d ;\n" % (linearFuncStr(cs_c[ci][ix], cs_v[ci][ix]), cs_s[ci], cs_b[ci]))

    f.close()

if __name__ == "__main__":
    ls, probtype, outFolder = readArguments()

    for fname in ls:
        bname = getBaseName(fname)
        print("Encode input file of type %s: %s" % (probtype, fname))
        objfs_m, objfs_c, objfs_v, cs_c, cs_v, cs_s, cs_b = convert2PBMO(fname, probtype)

        bname = getBaseName(fname)
        pname = probtype.split("-")[0]
        comments = ["Problem type: %s" % probtype, "Input file: %s" % fname]
        saveTo = outFolder+"/"+bname+"-"+pname+".pbmo"

        print("Save PBMO instance to: %s" % saveTo)
        savePBMOToFile(objfs_m, objfs_c, objfs_v, cs_c, cs_v, cs_s, cs_b, saveTo, comments=comments)
        print("Done!")
