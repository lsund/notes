# Notes WIP

This is a notetaking program built inspired by
[Tiddlywiki](https://tiddlywiki.com/) and built with
[brick](https://github.com/jtdaugherty/brick).

I started using tiddlywiki and liked it because of its lightweight feel and
simple way of linking notes.

However, it being a web application means a lot of clicking. Furthermore, its
UI doesn't use any fancy styling or other capabilities that comes with the
browser environment. It seemed like the important features of tiddlywiki could
be implemented in a terminal environment. Because I am more confortable on the
command line and in text editors, I started to do this in this repository.

Rendering is always one of the harder parts of any program that has a UI.
Therefore, it was crucial that I found a good, reliable library to do this in
Brick.

## Design goals

* It has a terminal user interface

* It is not a command line utility. No flags, no piping with other programs.

* It stores data locally in a file.

* It has a single binary, no dependencies.

* It is frictionless creating new and linking notes.

* It is easy to view notes, related notes and all notes.

* It leverages colors, formatting and other modern terminal capabilities.

## Installing and Running

E.g.
```
stack install
~/.local/bin/notes
```
## Current status [WIP]

* Can persist and read whole state onto disk.

* Can render notes

* Can cycle between notes

* Can update the content of notes

## Todo

* Create new notes

* Bullet lists

* Links

* Tags

## Alternatives to Notes.hs

Alternatives to this that actually works are
[VimWiki](https://github.com/vimwiki/vimwiki#key-bindings) or [Org
Mode](https://orgmode.org/), but those are based and constrained by the usage
of those editors.

