# MacOS setup notes

In addition to nix and home-manager, which can be used to install any
nix-supported software on MacOS, a few other things are required to be
installed manually/through different means.

## Shell

Add nix-installed fish to valid shells:

Add `/Users/jeyj0/.nix-profile/bin/fish` to `/etc/shells`.

Run `$ chsh -s /Users/jeyj0/.nix-profile/bin/fish`.

## Homebrew

Homebrew is a MacOS package manager that is inferior to nix overall, however it
does include support for some packages that nix doesn't support.

Check https://brew.sh/ for how to install Homebrew.

### Homebrew packages

I use the following packages, which can be installed via `$ brew install
PACKAGENAME`:

- syncthing
	+ `$ brew services start syncthing` (to start the syncthing service
	  immediately and on login)
- lxc
