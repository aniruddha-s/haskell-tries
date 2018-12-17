Title: Purely Functional Hash Array Mapped Tries
Members: Aniruddha Shukla and Jainey James

The project was to implement HAMT and CHAMP, which are two space-efficient tries that store hash prefixes and can be used in lieu of hash-based maps. An additional goal was to demonstrate the use of HAMT and CHAMP by showing a motivating application - we chose to go with implementing a histogram of words from a wordlist.

Structure of the code:
HAMT
 - HAMT.hs : The implementation of a Hash Array Mapped Trie. This is a standalone data structure with minimal dependencies. Apart from base, it only requires the packages containers (for Data.Sequence) and hashable (for Data.Hashable). It does require a newer version of GHC than Ubuntu distributes by default - this will be addressed later in the README.

- HAMT_histogram.hs : It is an implementation of a histogram using HAMT as the underlying storage.

- QuickCheck.hs : Contains QuickCheck tests for HAMT. Only contains algebraic tests. This requires QUickCheck to be installed.

- Trie.hs :  An implementaiton of a basic trie that attempts to be analogous to HAMT in functionality. This is meant to serve as a baseline against which to compare HAMT and CHAMP.

- Trie_histogram.hs : Similar to HAMT_histogram.hs, but using a Trie as an underlying structure.

- Autocomplete.hs : An application that uses Trie.hs to emulate autocomplete funcitonality. This was done as an exercise to better understand Trie construction and usage, and as a demo of the Trie implementation.

Setting up project using stack:
We have used stack to manage our GHC and library versions. The entries under `dependencies` in package.yaml are

- base >= 4.7 && < 5
- containers
- hashable
- QuickCheck

With these entries in place, stack fetches and builds our project correctly. Due to a dependency in HAMT.hs on a newer version of Data.Sequence, we were unable to use the GHC provided with Ubuntu 18.04. Stack installed GHC 8.4.4, which has the correct version of Data.Sequence.

Running the histogram tests:
HAMT_histogram and Trie_histogram work by taking a file containing text, another file containing a list of words, and prints the frequency of words from teh second file occurring in the first file's text.

Usage: runhaskell HAMT_histogram.hs BookFile.txt WordFile.txt

Example usage with provided files:
runhaskell HAMT_histogram.hs Moby_Dick.txt words.txt

Autocomplete works by taking a text file as input and storing all the words in the trie. It then waits for user input, and returns a list of possible words that can be completed from the provided prefix.



Work done for the project:

	Initial challenge:
	Understanding the concept of tries, HAMT and CHAMP, and the functionality and benefits provided by them.
	Understanding how these data structures should be implemented in Haskell.
	Coming up with a suitable application for showcasing HAMT. Initially we envisioned comparing it with a Trie, before we realized that HAMTs are closer to Maps.

	Implementation challenges:
	Coming up with a suitable data definition for Trie and especially HAMT. Poor data definitions were thought of and discarded.
	Understanding how to use existing typeclasses like Hashable to provide the funcitonality we needed without having to reimplement it ourselves.
	Learning how to manipulate bits in Haskell.
	Verifying that the bit manipulations being performed for the HAMT were correctly implemented, since it is difficult to fully verify by simply observing the Int values corresponding to the bitmaps.
	Making HAMT an instance of Functor and Foldable.
	Makng HAMT an instance of Arbitrary - we were unable to do this, and it limited the types of QuickCheck tests that we could have written.

	Abandoned approaches:
	Data definitions for HAMT that did not enforce invariants
	Data definitions that attempted to provide better performance for operations like size
	GADT approaches so that CHAMP could be implemented too - we were unable to understand how to proceed with this.
	Unable to implement CHAMP due to confusion about seemingly heterogenous types in a sequence and supposed requirements for arrays that use contiguous memory.

Lessons learned:
Coming up with a suitable data definition is extremely important and affects all development of functionality down the line.
Invariants are enforceable with data definition and function type signatures.
Making the data structure instance of Foldable, etc adds lot of functionality and generality.
A few low level operations , well composed, can make it easy to develop several other funcitonalities.
Building applications based on the data definitions and working with their limitations (eg. when custom Show is not defned and builtin fails due to ambiguous types).
Knowing what types are needed exactly solves many questions related to design.