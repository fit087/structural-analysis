# Structural Analysis Program


This project contain a Fortran program to perform a analysis of statically indeterminate structures.

We have two distinct method of analysis for statically indeterminate structure depending upon how the above equations are satisfied: 
1.  Force method of analysis (also known as flexibility method of analysis, method of consistent deformation, flexibility matrix method);
2.  **Displacement method of analysis** (also known as stiffness matrix method).

## Displacement Method of Analysis

This program will implement the displacement method of analysis. In it, the primary unknowns are the displacements. In this method, first force-displacement relations are computed and subsequently equations are written satisfying the equilibrium conditions of the structure. After determining the unknown displacements, the other forces are calculated satisfying the compatibility conditions and force displacement relations. The displacement-based method is amenable to computer programming and hence the method is being widely used in the modern day structural analysis. 
