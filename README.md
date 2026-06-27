# Home-Manager Setup

To install nix on Fedora 42-based images, this workaround is currently required: https://github.com/DeterminateSystems/nix-installer/issues/1445#issuecomment-2856334377

## Symbolic-Link based settings

```bash
# Zed
ln -s ~/.config/home-manager/config/zed/keymap.json ~/.config/zed/keymap.json
ln -s ~/.config/home-manager/config/zed/settings.json ~/.config/zed/settings.json

# git
ln -s ~/.config/home-manager/config/git/config ~/.config/git/config
ln -s ~/.config/home-manager/config/git/global.gitignore ~/.config/git/global.gitignore
```

## Fonts

For nix fonts to be available, add this symbolic link:

```bash
ln -s ~/.nix-profile/share/fonts ~/.local/share/fonts/nix
```
