SummarizeSSHKeys
================

*A utility to produce readable summaries of SSH `authorized_keys` files*

Rationale
---------

The lines in `authorized_keys` files can be very long. This makes it difficult to check for the presence of a particular key, or to get an overview of which keys are enabled. In addition, there can be long strings of options at the start of a line (eg `no-pty` and `command=...`).

Usage
-----

The `summarize-ssh-keys` utility reads from `stdin` and outputs a summary on `stdout`. Typical output looks like this:

```
[0] ecdsa I4SlvC1K+Q neil@windermere
[0] rsa   wfgIuusN3F neil@strauss
[0] rsa   DhPnKnQvew neil@strauss-WIN7-32
[6] rsa   24nvz3x9e3 Synergy client tunnel
[0] rsa   z9t6w/2Ymh kevin@ubuntu-Studio-1747
[0] rsa   rSwHWBXcoQ steve@macbook
[0] rsa   PDFMTvLABw Spice
```

The meaning of the columns is as follows:

1. The number of options present
2. The key type
3. The last 10 characters of the key hash (with trailing `=`s removed)
4. The content of the comment field

**Tip:** When checking for the  presence of a particular key, first run the key itself (the `.pub` file) through `summarize-ssh-keys` to discover its abbreviated value. Then look for that string in the summary of `authorized_keys`.

Building
--------

You can use any of `cabal`, `nix-build` or `debuild` as your system allows.

For Developers
--------------

The code includes a reusable `Parsec` parser for the OpenSSH public key format.
