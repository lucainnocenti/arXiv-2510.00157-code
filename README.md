# Code for [arXiv:2510.00157](https://arxiv.org/abs/2510.00157)

This repository contains Wolfram Mathematica code used to reproduce some of the numerical results presented in:

**Gabriele Lo Monaco, Salvatore Lorenzo, Alessandro Ferraro, Mauro Paternostro, G. Massimo Palma, Luca Innocenti**, *The non-stabilizerness cost of quantum state estimation*, arXiv:2510.00157.

## Files

The main file is:

* **`pauliproductsgenerators.nb`** — Mathematica notebook containing the numerical calculations and searches.

The remaining files are auxiliary data/code used by the notebook:

* **`pauliStringsHandler.m`** — helper functions for manipulating Pauli strings and stabilizer groups.
* **`n3t6_generatori_casibuoni.txt`** — precomputed data used in the notebook.
* **`statesn1t2IC.txt`** — additional precomputed data used in the numerical analysis.

## Requirements

The notebook was developed using **Wolfram Mathematica 14.0**.

To run the calculations, clone the repository, keep all files in the same directory, and open `pauliproductsgenerators.nb` in Mathematica.
