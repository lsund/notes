# Notes.hs [WIP]

This is a notetaking program built inspired by
[Tiddlywiki](https://tiddlywiki.com/) and built with
[Brick](https://github.com/jtdaugherty/brick).

I like Tiddlywiki because of its lightweight feel and uncomplicated way of
taking and linking notes. However, there are some aspects that I do not
particularly like:

1. Due to it being a web-application, there is a lot of clicking and other
  mouse-action. This can be difficult if you are working on a laptop and don't
  have access to a mouse.

2. The UI that comes out of the box is not that good. There doesn't seem to be a
  way to sort notes that you have open except for closing them and reopening
  them in a different order. There are animations, but they are slow and only
  make things difficult. The look and feel isn't that great, and bloated with
  a lot of options that isn't directly helpful to the essential process of
  taking and linking notes.

3. It has the feel of a SPA and it stores its state in the DOM. To use
  tiddlywiki, you have to figure out a way to eventually persisting these
  changes. I tried a simple WebDAV implementation for this. If I accidentally
  refresh the page, or if the web-server goes down while tiddlywiki is in a
  non-saved state, I lose my changes. Furthermore, I could not figure out a way
  to automatically update links in other tiddlers, if I change the name of a
  tiddler, meaning that if I update the name of a tiddler, I potentially break
  multiple links in other tiddlers and have to verify this manually every time.

## What it is

This is an implementation of a note-taking program that addresses the above
points and tries to improve on them. The purpose is to quickly and easily take
and link notes in a bi-directional, non-linear way.

1. As I am more confortable on the command line and text editors, I decided to
   implement the program in a terminal environment. Terminal input and
   terminal rendering is hard, so we use Brick that seems to approach these
   problems in a good way. By working in a terminal environment, we eliminate
   the mouse completely and are free to choose our hotkeys as we want.

2. The program uses the terminal as interface, which has a universal look and
   feel that everybody using it is familiar with. The color and formatting
   options that modern terminals handle are enough for our purpose. No
   animations, no redundant, distracting options.

3. The entire state can be persisted to a file, and this is done locally in a
   common file format. It has a single binary and no external dependencies. A
   note has a unique ID, and changing its name does not change any references
   to it. On the other hand, changing the name of a note updates the names of
   all links in other notes.

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

