# Notes.hs

## What?

This is a notetaking program built inspired by
[Tiddlywiki](https://tiddlywiki.com/)(TW) and built with
[Brick](https://github.com/jtdaugherty/brick).

This is an attempt to mimic some of TW's functionality in a terminal UI, with
focus on keyboard ergonomics and simplicity.

## Why?

Every productivity / organizing tool I've encountered have been either too
simplistic or too complicated, with a lot of unused features.

TW falls into the second category. Furthermore, it lives in the browser and
that leads to a lot of clicking. I started this project to try to express my
preference of a non-linear note-taking tool.

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

