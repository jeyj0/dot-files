# Home-Manager Setup

To install nix on Fedora 42-based images, this workaround is currently required: https://github.com/DeterminateSystems/nix-installer/issues/1445#issuecomment-2856334377

## Fonts

For nix fonts to be available, add this symbolic link:

```bash
ln -s ~/.nix-profile/share/fonts ~/.local/share/fonts/nix
```
