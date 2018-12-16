# haskell-tries
Haskell implementation of various tries and its applications

For running our implementation of Hash Array Mapped Tries, we require the insertAt function from Data.Sequence from containers which was not available with the version of ghc distributed by ubuntu. 
To ensure this functionality, use stack and in the package.yaml file, under dependencies, insert the following lines:
- containers
- hashable
