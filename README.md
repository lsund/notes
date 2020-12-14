# Notes WIP

This is a notetaking program built on [brick](https://github.com/jtdaugherty/brick),
inspired by [Tiddlywiki](https://tiddlywiki.com/).

I wanted a note taking program that has the same basic functionality as
tiddlywiki (associative, non-linear notetaking with links as the main
construct), but as I prefer the command line, the goal is to have something
that works without using the mouse.

Alternatives to this that actually works are
[VimWiki](https://github.com/vimwiki/vimwiki#key-bindings) or [Org
Mode](https://orgmode.org/), but those are based and constrained by the usage
of those editors.

## Design goals

* It is a terminal user interface only. There are plenty of note-taking
  applications with GUI's.

* It is not a command line utility. No flags, no piping with other programs.

* It stores data locally.

* It has a single binary, no dependencies.

* It is frictionless creating new and linking notes.

* It is easy to view notes, related notes and all notes.

* It leverages colors, formatting and other modern terminal capabilities.

## Installing and Running

E.g.
```
stack install
~/.local/bin/notes-exe
```

## WIP

This is a work in process. To have a POC, we have to properly design data
model, serialization and rendering.
