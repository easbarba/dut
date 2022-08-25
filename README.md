# Dot
Yet another simple and opinionated dot files manager.

# Features:
 - `.dotsignore` behaves just like git's one.
 - Folders are not linked but created.
 - dry-run mode.
 - remove faulty symbolic links, if found.
 - backup non-symbolic link files to \$HOME/.backup.
 - argp-like CLI interface.
 - GNU-Linux/BSD distros only.

## Usage

```sh
dot --from /data/dotfiles-repo --deploy
dot --from /data/bin --to $HOME/.local/bin --overwrite
dot --from /data/dotfiles-repo --pretend
dot --info
dot --help
```

## TODO
- purge all symbolic linked files
- keep history of early commands

# LICENSE

[GNU General Public License, Version 3](https://www.gnu.org/licenses/gpl-3.0.en.html)
