# Convert2PBMO

Encode classic Multiobjective Optimization (MO) problems as Pseudo-Boolean Multiobjective Optimization (PBMO) problems.

**Note**: The resulting encodings correspond to the minimization version of the problems.

**Requirements:** Requires `numpy`. For now, the code works only for python 2.7.


###Options

- `--help`: print options
- `-p`: Problem type and format given by a predefined string (must be `KS-CP16`, `SC-CP16`, or `SP-CP16`).
- `-f`: input file
- `-i`: input folder. Reads all files in the input folder. This options is used only if `-f`is not. 
- `-o`: output folder. Creates this folder if it does not exist, and saves the encoded instances in this folder. For each input file, an output file is created. The output file name consists of input file name concatenated with the name of the problem (e.g., KS, SP, SC,...) and the extension `.pbmo`.  For example, if the input file of a knapsack instance is `data.inp`, then the name of the output file will be `data-KS.pbmo`.

Regarding option `-p`, currently, the accepted problems and formats are:

 - Knapsack problem (formats: `KS-CP16`)
 - Set Covering problem (formats: `SC-CP16`)
 - Set Packing problem (formats: `SP-CP16`)

The input formats are described in *Problems* section.

**Note:** Although for now only one format is accepted for each problem, the idea is that instances may be provided in different formats, and thus, this code can be extended to accept those formats.


### Examples

The folder `examples` contains the following input files:
 - `small_knapsack.dat`
 - `small_set_cover-pack-ing.dat`
 
File `small_knapsack.dat`contains a knapsack problem instance. Run the following:

	python convert2PBMO.py -p "KS-CP16" -f examples/small_knapsack.dat -o results
	
This will create the file `results/small_knapsack-KS.pbmo`.


File `small_set_cover-pack-ing.dat`contains an instance of the set covering/packing problem. To generate the corresponding PBMO instance of the set *covering* problem run the following:

	python convert2PBMO.py -p "SC-CP16" -f examples/small_set_cover-pack-ing.dat -o results
	
This will create the file `results/small_set_cover-pack-ing-SC.pbmo`.

To generate the corresponding PBMO instance of the set *packing* problem run the following:

	python convert2PBMO.py -p "SP-CP16" -f examples/small_set_cover-pack-ing.dat -o results
	
This will create the file `results/small_set_cover-pack-ing-SP.pbmo`.





# Problems

## Knapsack (KS)

**KS-CP16 format**


Variables:

 - `n`: number of items
 - `d`: number of objectives
 - `W`: knapsack capacity
 - `pi`: i-th objective (i-th knapsack) profit vector (`pi_1, ..., pi_n`), for `i=1,...,d`
  	- `pi_j`: profit of item `j` in knapsack `i` 
 - `w`: weight vector (`w_1 ... w_n`)
 
 The goal is to select a subset of the `n` items such that the profit of each of the `d` knapsacks is maximized, without exceeding the overall weight of the selected items.

Input format:

	n
	d
	p1_1 p1_2 ... p1_n
	...
	pd_1 pd_2 ... pd_n
	w_1 w_2 ... w_n
	W

This is an example with 10 items, 3 objectives, and capacity W=3019:

	10
	3
	252 45 809 662 985 694 606 666 699 456 
	296 967 842 141 858 367 922 181 734 493 
	872 24 990 577 284 445 915 116 323 929 
	66 575 973 875 588 958 568 546 623 266 
	3019

This is an example with 10 items, 4 subsets, and 2 objectives:

	4 8 2
	10 14 22 27
	32 20 10 5
	4
	1 2 3 4
	3
	2 3 4
	2
	1 4
	2
	2 2
	2
	1 3
	2
	2 4
	3
	1 3 4
	2
	1 2

where the subsets of items (let us assume they are identified by number 1 to 8) are: {1,3,5,7,8}, {1,2,4,6,8}, {1,2,5,7}, {1,2,3,6,7}, and have the following corresponding costs: (10,32), (14,20), (22,10), (27,5).


##Set Covering (SC) problem

**SC-CP16 format**

Input:

 - `n`: number of subsets (of items)
 - `m`: number of items
 - `d`: number of objectives
 - `ci`: cost vector of the i-th objective (ci_1,...,ci_n), for all `i=1,...,d`
  	- `ci_j`: cost of selecting subset `j` for the i-th objective, where `j \in {1,...,n}`
- `s`: m-vector indicating to how many subsets each item belongs to
	- `sk`: item `k`belongs to `sk`subsets, where `k \in {1,...,m}`
- `bk`: vector of size `sk` containing the indexes of the subsets where item `k` is present, where `k \in {1,...,m}`
	- `bk_t`: item `k` is in subset `bk_t \in {1,...,n}`, where `t \in {1,...,sk}`
 	
The goal is to find out which of the `n` subsets to select, provided that each of the `m`items belong to *at least* one of such subsets, such that the cost of each of the `d` objectives is *minimized*.

Input format:

	m n d
	c1_1 ... c1_n
	...
	cd_1 ... cd_n
	s1
	b1_1 ... b1_s1
	s2
	b2_1 ... b2_s2
	...
	sm
	bm_1 ... bm_sm
	
	
	
##Set Packing (SP) problem

**SP-CP16 format**

The input format is the same as for the `SC-CP16` format.

In this case, the goal is to find out which of the `n` subsets to select, provided that each of the `m`items belong to *at most* one of such subsets (the subsets are pairwise disjoint), such that the cost of each of the `d` objectives is *maximized*.

	
#Data sets

The "CP16" data sets are those available from [here][1].

Folder `data`contains 3 types of instances:

 - binproblem
 - indepset
 - knapsack

This code is prepared to encode the instances in `knapsack` folder (the format is `KS_CP16`), and to encode the instances in `binproblem` folder which are used as instances of the Set Covering problem (the format is `SC-CP16`), and of the Set Packing problem (the format is `SP-CP16`).

**Note**: The script is not yet able to encode instances from `indepset`.

	
	
[//]: # (Links)
[1]: http://www.andrew.cmu.edu/user/vanhoeve/mdd/code/multiobjective_cp2016.tar.gz