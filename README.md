

# ECDSA tool

This simple tool implements ECDSA key pair generation and signature generation
as well as verification using Haskell. It was created as a project for FLP
course at BUT FIT as an introduction to functional language haskell.

The original assignment can be found in FLP\_project1\_2023.pdf.

## Requires

Installation of haskell packages:
- Parsec
- Random

## Usage

```bash
make

./flp22-fun [-i|-k|-s|-v] [input]
```

The *input* is either a file path from which to read the input itself or if
left unsepcified the stdin. 

Brief options description:
- Switch ``-i``:
	- takes an eliptic curve from the *input* 
	- prints it to the stdout.
- Switch ``-k``:
	- takes an eliptic curve from the *input* 
	- generates a key pair.
- Switch ``-s``:
	- takes an eliptic curve, a key pair and a message hash from the *input* 
	- generates a signature.
- Switch ``-v``: 
	- takes an eliptic curve, a signature, a publicKey and a message hash from the *input* 
	- verifies the specified signature.

Examples for possible inputs can be seen in tests/test\*.in.
