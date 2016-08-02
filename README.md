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

Examples
--------

Basic usage:

```
summarize-ssh-keys <~/.ssh/authorized_keys
```

Checking the keys for someone else's account:

```
sudo cat ~another/.ssh/authorized_keys | summarize-ssh-keys
```

Checking the keys for an account on a remote machine:

```
ssh someone@somewhere cat .ssh/authorized_keys | summarize-ssh-keys
```

When visually checking for the  presence of a particular key, first run the key itself through `summarize-ssh-keys` to discover its abbreviated value, so you can look for that string in summaries:

```
summarize-ssh-keys <~/.ssh/id_rsa.pub
```

Note that it's not necessary to build and install `summarize-ssh-keys` outside your own account on your own workstation. Because it takes its input from `stdin` you can use it in a pipe with some other command that can read the file in question.

Building
--------

You can use any of `cabal`, `nix-build` or `debuild` as your system allows.

For Developers
--------------

The code includes a reusable `Parsec` parser for the OpenSSH public key format.
