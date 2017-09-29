# LMG Document Search Suite

OCaml command-line tool for document searching and manipulations. Joint work with Gabriel Goldberg.

## Installation

1. Navigate to lmg_dss directory
2. Type	`make`
3. That's it!

## Usage

There are two modes:  

1. Text-based interface that allows access to the entire suite:

`./LMG`

2. Feed LMG its arguments directly:

`./LMG <mode> <document> <search_term>`

Arguments:
- `<mode>`: string search algorithm to employ
  - `bm`: Boyer-Moore
  - `kmp`: Knuth-Morris-Pratt
  - `fp`: fingerprinting
  - `rgx`: regular expression search
  - `ds`: document similarity
  - `pd`: plagiarism detection
- `<document>`: text document to search
- `<search_term>`: phrase to search for

Example:

Use the Boyer-Moore algorithm to search whit.txt for the word "grass":

`./LMG bm whit.txt grass` 

# Running the Regular Expression Search

The regular expression matcher understands the following characters:

- c: c can be any character that is not an escape sequence, +, *, ?, (, ), or |
- ?: a single occurence of any character
- |:  the logical or operator
- *: the Kleene star, which by default applies only to the previous character so use parentheses!
- +: the Kleene plus, where r+ = r(r^*)
- (,): used to indicate precedence

For example, all of the following are allowable inputs
- (a|b)*c
- (d(a|b)*)?*a
- the?*boy
