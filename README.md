# Notes.hs [WIP]

This is a notetaking program built inspired by
[Tiddlywiki](https://tiddlywiki.com/) and built with
[Brick](https://github.com/jtdaugherty/brick).

Tiddlywiki is nice because of its lightweight feel and uncomplicated way of
taking and linking notes. However, it being a web application means a lot of
clicking. Furthermore, its UI doesn't use any fancy browser capabilities (at
least not out of the box) that couldn't be implemented in a terminal
environment. Because I am more confortable on the command line and in text
editors, I started this repository as an  implementation that would leverage
the core features of tiddlywiki in a terminal environment.

Rendering is always one of the harder parts of any program that has a UI.
Furthermore, terminal input is difficult, so we delegate that responsibility to
Brick, a library that seems to approach these issues well.

## Design goals

* It is simple and easy to creating new notes and linking notes.

* It is easy to view links and follow links.

* It is terminal native: it leverages colors, formatting and other modern terminal capabilities.

* It is not a command line utility. No flags, no piping with other programs.

* It stores data locally in a file in a common format.

* It has a single binary, no dependencies.

## Installing and Running

I use stack for development. To create an executable:
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

