__author__="Andreia P. Guerreiro"
__copyright__ = "Copyright 2019 (C) Andreia P. Guerreiro"
__license__ = "GNU General Public License, version 3"
__version__ = "0.2"

from itertools import combinations 
import numpy as np
import argparse
import math
import sys

from utils import listInputFiles, createFolder, getBaseName

_digits = 4 #cost of SPL features is multiplied by 10**_digits and then rounded

# -------------------------------------------------------------------------------- #
# ----------------------- Parse arguments ---------------------------------------- #
# -------------------------------------------------------------------------------- #

def readArguments():
    
    probchoices = ["KS-CP16", "SC-CP16", "SP-CP16", "SPL-ICSE15"]
    #for di in range(2,5):
        #probchoices.extend(map(lambda ds: "SPL-ICSE15-"+"_".join(map(str, ds)), combinations(range(5), di)))
    
    parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter,
        description="Encode Multiobjective Optimization (MO) problems as Pseudo-Boolean\n"
        "Multiobjective Optimization (PBMO) problems.\n"
        "Available problems and formats:\n"
        "  - KnapSack problem              (KS-CP16)\n"
        "  - Set Covering problem          (SC-CP16)\n"
        "  - Set Packing problem           (SP-CP16)\n"
        "  - Software Product Line problem (SPL-ICSE15)\n"
        )
    
    parser.add_argument("-p", type=str, required=True, dest="probtype", choices=probchoices, help="Problem type and format")
    parser.add_argument("-f", type=str, dest="fname", 
                        help="input file")
    parser.add_argument("-i", type=str, dest="inFolder", 
                        help="input folder (used only if flag -f is not used)")
    parser.add_argument("-o", type=str, dest="outFolder", default="garbage", 
                        help="output folder")
    parser.add_argument("-d", type=int, dest="dims", action="append",
                        help="Which objective functions to print (available\n"
                        "only for SPL-ICSE15). By default all objective\n"
                        "functions are printed")
    args = parser.parse_args(sys.argv[1:])
    print ("args: %r\n" % args)

    
    if args.fname is None and args.inFolder is None:
        print("Either -f or -i must be used")
        exit()
        
    if args.dims is not None:
        args.dims = sorted(args.dims)
        
    ls = [args.fname] if args.fname is not None else listInputFiles(args.inFolder)
    createFolder(args.outFolder)
    
    return ls, args.probtype, args.outFolder, args.dims



# -------------------------------------------------------------------------------- #
# ----------------------- Convert functions -------------------------------------- #
# -------------------------------------------------------------------------------- #

def convert2PBMO(fname, probtype, dims):
    
    if probtype.startswith("KS-"):
        return convert_KS(fname, probtype)
    if probtype.startswith("SC-"):
        return convert_SC(fname, probtype)
    if probtype.startswith("SP-"):
        return convert_SP(fname, probtype)
    if probtype.startswith("SPL-"):
        return convert_SPL(fname, probtype, dims)
    
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


# --------- Software Product Lines (SPL) -------------- #

def SPL_ICSE15_read_constraints(fname):
    cs_c, cs_v,cs_b = [], [], []
    with open(fname, "r") as f:
        for line in f:
            line = line.strip()
            if len(line) > 0 and not line.startswith("c ") and not line.startswith("p "):
                #print "line:", line
                lits = map(int, line.split()[:-1])
                coeffs = [1 if l >= 0 else -1 for l in lits]
                vrs = [abs(l) for l in lits]
                cs_c.append(coeffs)
                cs_v.append(vrs)
                cs_b.append(1-(np.array(coeffs) < 0).sum())
        
    noc = len(cs_c) #number of original constraints
    mand = np.loadtxt(fname+".mandatory", dtype=int, ndmin=1)[:,np.newaxis]
    cs_v.extend(list(mand))
    cs_c.extend(list(np.ones((len(mand),1), dtype=int)))
    cs_b.extend([1] * len(mand))
    
    forb = np.loadtxt(fname+".dead", dtype=int, ndmin=1)[:,np.newaxis]
    cs_v.extend(list(forb))
    cs_c.extend(list(-np.ones((len(forb),1), dtype=int)))
    cs_b.extend([0] * len(forb))
    
    m = len(cs_c)
    cs_s = [">="] * m
    #cs_b = [1] * m
    return cs_c, cs_v, cs_s, cs_b, noc


def SPL_ICSE15_read_objfs(fname, cs_c, cs_v, objis=[0,1,2,3,4]):
    d = 5
    costs = np.loadtxt(fname+".augment")
    n = len(costs)
    
    objfs_v =  costs[:,0][np.newaxis,:].astype(int).repeat(d-1, axis=0)
    objfs_c = np.empty((d-1,n), dtype=int)
    
    maxc = costs[:,0].sum()
    nd = round(math.log10(maxc))
    maxd = max(nd, _digits)
    objfs_c[0,:] = np.around(costs[:,1] * (10**(maxd-nd))).astype(int) #cost
    
    costs[:,2] = np.where(costs[:,2] > 0, 0, 1)
    objfs_c[1,:] = costs[:,2].astype(int) #not used_before
    objfs_c[2,:] = costs[:,3].astype(int) #defects
    objfs_c[3,:] = -np.ones(n, dtype=int) #deselected features
    
    if 4 in objis:
        m = len(cs_c)
        madd = 0
        for ci in range(m):
            #if len(cs_c[ci]) > 1:
            cs_c[ci].append(1)
            cs_v[ci].append(n+1+ci)
            madd += 1
            
        #(minimize) number of violated constraints
        objfs_c = [list(objfs_c[i,:]) for i in range(d-1)]
        objfs_c.append(np.ones(madd, dtype=int))
        objfs_v = [list(objfs_v[i,:]) for i in range(d-1)]
        objfs_v.append(range(n+1, n+madd+1))
        
    objfs_c_sel, objfs_v_sel = [], []
    for di in objis:
        objfs_c_sel.append(objfs_c[di])
        objfs_v_sel.append(objfs_v[di])
    objfs_c = objfs_c_sel
    objfs_v = objfs_v_sel
    
    return objfs_c, objfs_v, cs_c, cs_v


def convert_SPL(fname, probtype, dims):
    if dims is not None:
        ds = np.array(dims)
        if (ds < 0).any() or (ds >= 5).any():
            print("Error: Invalid dimension index (must be in {0,...,5})")
            exit(-1)
    else:
        dims = range(5) #select all obj functions
    print("include obj functions %r\n" % dims)

    if probtype == "SPL-ICSE15":
        cs_c, cs_v, cs_s, cs_b, noc = SPL_ICSE15_read_constraints(fname)
        objfs_c, objfs_v, cs_c[:noc], cs_v[:noc] = SPL_ICSE15_read_objfs(fname, cs_c[:noc], cs_v[:noc], dims)
        
        #same as SC-CP16, except it maximizes objective functions, and constraints are "<="
        objfs_m = ["min"] * len(objfs_c)
        cs_s = [">="] * len(cs_c)
        
    
    return objfs_m, objfs_c, objfs_v, cs_c, cs_v, cs_s, cs_b
    
    
    
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
        objfs_c[di] = np.array(objfs_c[di])
        objfs_v[di] = np.array(objfs_v[di])
        ix = np.where(objfs_c[di] != 0)
        if minversion and objfs_m[di] == "max":
            f.write("min: %s ;\n" % (linearFuncStr(-objfs_c[di][ix], objfs_v[di][ix])))
        else:
            f.write("%s: %s ;\n" % (objfs_m[di], linearFuncStr(objfs_c[di][ix], objfs_v[di][ix])))
    f.write("\n")
    for ci in range(nc):
        cs_c[ci] = np.array(cs_c[ci])
        cs_v[ci] = np.array(cs_v[ci])
        ix = np.where(cs_c[ci] != 0)#[0]
        f.write("%s %s %d ;\n" % (linearFuncStr(cs_c[ci][ix], cs_v[ci][ix]), cs_s[ci], cs_b[ci]))

    f.close()


if __name__ == "__main__":
    ls, probtype, outFolder, dims = readArguments()
    
    for fname in ls:
        
        #if
        #encode
        bname = getBaseName(fname)
        print("Encode input file of type %s: %s" % (probtype, fname))
        objfs_m, objfs_c, objfs_v, cs_c, cs_v, cs_s, cs_b = convert2PBMO(fname, probtype, dims)
        
        #set output vars
        d = len(objfs_m)
        bname = getBaseName(fname)
        
        pname = probtype.split("-")[0]
        if probtype == "SPL-ICSE15":
            bname += fname[fname.find("."):]
            pname += "-"+str(d)+"D"
        
        if dims is not None:
            pname += "-"+"_".join(map(str, dims))
        comments = ["Problem type: %s" % probtype, "Input file: %s" % fname]
        saveTo = outFolder+"/"+pname+"-"+bname+".pbmo"
        
        #save to file
        print("Save PBMO instance to: %s" % saveTo)
        savePBMOToFile(objfs_m, objfs_c, objfs_v, cs_c, cs_v, cs_s, cs_b, saveTo, comments=comments)
        print("Done!")
