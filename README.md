# xbox
`xbox` is a very minimal but roughly usable command-line music player and music library browser written in Haskell using the [brick](https://github.com/jtdaugherty/brick) UI library. The name is a careless portmanteau of [xmonad](https://github.com/xmonad/xmonad) and [rhythmbox](https://github.com/GNOME/rhythmbox).

## Configuration
The application settings are hard-coded, including the path to the root folder that contains the music library. All paths should probably be absolute. The settings are defined at the bottom of [`Main.hs`](https://github.com/kcsmnt0/xbox/blob/2e27c6cd76825f70302d9f3b6d74e84eae2b0eff/src/Main.hs#L199):
- `libraryPath` is a path to the root folder of your music library. MP3, FLAC, OGG, and WAV files are supported (although this is kind of an arbitrary whitelist and should really include all formats that FFMPEG supports).
- `logPath` is a path to a log file, which doesn't need to already exist.
- `cachePath` is a path to a file where the library structure will be cached, which doesn't need to already exist.
- `tempPath` is a path to an existing folder where temporary `.wav` files will be stored while they're being played back. Currently `xbox` never cleans up after itself so you should keep an eye on this folder to make sure it doesn't get too big.
- `ffmpegPath` is a path to the `ffmpeg` binary.

## Installation
Install with [stack](https://www.haskellstack.org) (`stack install` puts it in `~/.local/bin`) unless you have another preference. GHC 8.6.1 or greater is required (which `stack install` should take care of automatically). Building requires system installations of the [OpenAL](https://www.openal.org/) and [ALUT](https://github.com/vancegroup/freealut) development libraries (e.g. the `libopenal-dev` and `libalut-dev` Debian packages). Running requires the [FFMPEG](https://github.com/FFmpeg/FFmpeg) binary.

## User Guide
On first run, `xbox` will scan your music library; this might take a while if you have a lot of music, and there isn't any progress display for this part. Once that's finished, you should see a three-pane layout, with lists for artists, albums, and songs. The focused list has asterisks (`*`) around its title, and the selected item in each list, if any, is prefixed with an asterisk in the focused list or a dash (`-`) in the unfocused lists.

### Keybindings
- `Up`/`j`: move selection up in focused list
- `Down`/`k`: move selection down in focused list
- `Ctrl+Right`: focus next list (clockwise)
- `Ctrl+Left`: focus previous list (counterclockwise)
- `Enter`: play selected track (when tracks list is focused)
- `Esc`: quit

Audio decoding is hard, so `xbox` calls `ffmpeg` to decode the entire selected track to a WAV file before trying to play it, which can sometimes cause a little delay before playback starts. There is currently no support for viewing playback status.

## Code style
The [`BlockArguments`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/glasgow_exts.html#extension-BlockArguments) extension added in GHC 8.6.1 enables a new weird trick to avoid parentheses in Haskell code, briefly mentioned in [the GitLab wiki page for the proposal](https://gitlab.haskell.org/ghc/ghc/wikis/argument-do#multiple-block-arguments), which I'm trying out in this project to see if I like it in practice even though it might turn out to be a bad idea: the `do` keyword effectively gains a new use as a weaker version of the `$` operator that respects line endings as breaks between expressions.

For example, the code

```haskell
borderWithLabel
  do txt $ mconcat [headerDecoration, header, headerDecoration]
  do
    renderList
      do \sel x -> txt $ mconcat [if focus then "* " else "- " | sel] <> f x
      do focus
      do s^.l
```

might normally be written as the equivalent

```haskell
borderWithLabel
  (txt (mconcat [headerDecoration, header, headerDecoration]))
  (renderList
    (\sel x -> txt $ mconcat [if focus then "* " else "- " | sel] <> f x)
    focus
    (s^.l))
```

As far as I know this is technically not overloading the use of `do` in idiomatic Haskell code, since a `do` block containing only one monadic expression is a valid but pointless use of syntax without `BlockArguments` enabled. That feels like kind of a bad excuse but I'm sticking with it for now.
