# Web meme generator

A web application for creating
[memes](https://en.wikipedia.org/wiki/Internet_meme) from pictures:

![Printing a newline in Haskell](https://s-media-cache-ak0.pinimg.com/736x/d4/0e/34/d40e34931f55f7fb2d6e1ef7eff11b73.jpg)

This is a demo beginners project for ZuriHac 2016. See the
[codelab](codelab/memegen_codelab.md) for more details.


## Libraries

- aeson (JSON library)
- bytestring (Low-level library that allows us to work with bytes)
- containers (Some standard data structures likes sets and maps)
- directory (Used to list and modify directories in the filesystem)
- enumerator (A low-level streaming library)
- filepath (Manipulating filepaths)
- gd (This is an image manipulation library)
- lens (Lens is, amongst other things, a utility library for modifying Haskell records)
- mtl (The standard monad libraries)
- snap (The web framework we will use)
- snap-core (Core part of the web framework)
- snaplet-sqlite-simple (Glue between our web framework and the database library)
- snap-server (Engine for our web framework)
- sqlite-simple (Allows us to access the `sqlite` database)
- text (Manipulating human-readable strings)

## External dependencies

- [GD library](https://github.com/libgd/libgd)
- [SQLite](https://www.sqlite.org/)
