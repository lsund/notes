# Notes.hs

This is a notetaking program built inspired by
[Tiddlywiki](https://tiddlywiki.com/)(TW) and built with
[Brick](https://github.com/jtdaugherty/brick).

I like Tiddlywiki because of its lightweight feel and how frictionless it is
taking and linking notes. However, there are some aspects that I do not
particularly like about TW:

1. A lot of clicking. I'd rather quickly take and link notes using the keyboard
   only.

2. The UI "out of the box". The only way to organize your open notes is closing
   them and reopening them in a different order. There are animations that make
   the interaction slower. The styles are not great, and bloated with a lot of
   options that isn't directly helpful to the essential process of taking and
   linking notes.

3. The feel of a being a SPA, but it has no real dedicated backend. TW stores
   its state in the DOM and it is up to you to figure out how to persist the
   changes to disk. You can use a Node implementation that TW provides, or any
   WebDAV. With my WebDAV implementation, I could not figure out a way to
   automatically update links when their corresponding tiddler names changed.
   As a result, anytime I update the name of a tiddler, I potentially break
   multiple links in other tiddlers and have to verify this manually every
   time.

## What This is

This is a note-taking program focused on the ease of taking and linking notes
in a bi-directional, non-linear way.

1. In a terminal environment, we don't need the mouse at all and have a
   isolated environment for customizable hotkeys. We don't need to implement
   terminal input or rendering ourselves, for this we use Brick.

2. The terminal interface has an universal look and feel. Anybody familiar with
   the command line and basic text editors should feel confortable using our
   program. The color and formatting options that modern terminals handle are
   enough for our purpose. No animations, no redundant, distracting options.

3. The entire state can be persisted to a file, and this is done locally in a
   common file format. It has a single binary and no external dependencies. A
   note has a unique ID, and changing its name does not change any references
   to it. On the other hand, changing the name of a note updates the names of
   all links in other notes.

## Installing and Running

I use stack for development. To create an executable:
```
stack install
```

## Usage

Start the program with the path to the database (JSON) file:

```
~/.local/bin/notes my-database.json
```

If `my-database.json` does not exist, it will be created as an empty array.

### Keybindings

#### While not editing

* `C-n` to create a new note
* `Tab` or `S-Tab` cycle focus between notes
* `C-o` edit the focused note

#### While editing

* `Tab` switches focus between title and content
* `C-g` Stop editing a note
* `C-l` Jump to note with title under cursor or create a new note titled that
  word.

## Alternatives to Notes.hs

Alternatives to this that actually works are
[VimWiki](https://github.com/vimwiki/vimwiki#key-bindings) or [Org
Mode](https://orgmode.org/), but those are based and constrained by the usage
of those editors.

