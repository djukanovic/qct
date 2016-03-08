# Quark Contraction Tool - QCT
QCT is a package for performing Wick contractions of fermionic operators in
quantum field theory. Furthermore it implements most common algebraic
manipulations used in the calculation of physical matrix elements in lattice
QCD. The package aims at automatically generating expressions suitable for
numerical evaluation within C++. For more details on the package please see 

http://arxiv.org/abs/1603.01576

## Installation 
The repository contains an example notebook (Example.nb) showing the basic
functionality. To load the package either load the .m file directly, i.e. 

`Get[<absolute path to qct.m>]`

or make sure that the cloned repository is within Mathematica's standard search
path. One can check the paths used in Mathematica inspecting the output of
`$Path`.
Cloning the repository into one of the directories within the $Path environment
variable, makes the package accessible as
``<<qct` ``

## Terms of use
The software may be used under the terms of the  GPL license. Please cite 
http://arxiv.org/abs/1603.01576 
in papers that made use of the package. 

