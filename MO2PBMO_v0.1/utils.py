import glob, os

# -------------------------------------------------------------------------------- #
# ------------------- General functions  ----------------------------------------- #
# -------------------------------------------------------------------------------- #


""" Returns a list of input files in _inFolder folder """
def listInputFiles(inFolder):
    ls = glob.glob(inFolder+"/*")
    #ls = list(map(getFilename, ls))
    #print("input files:\n%r\n" % ls)
    print("Number of input data sets: %d\n" % len(ls))

    ls.sort() #TODO: usar lexsort
    return ls#[:2]

def createFolder(folder):
    if not os.path.exists(folder):
        os.makedirs(folder)


def getBaseName(fname):
    fname = fname.split("/")[-1]
    e = fname.rindex(".") if "." in fname else len(fname)
    return fname[:e]
        
        
