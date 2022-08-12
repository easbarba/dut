# Dot
Yet another simple and opinionated dot files manager.

# Features:
 - the dotsignore file at the folder root behaves just like git's one.
 - Folders are not linked but created.
 - dry-run mode.
 - remove faulty symbolic links, if found.
 - backup non-symbolic link files to $HOME/.backup.
 - fully implemented CLI interface.
 - GNU-Linux/BSD distros only.

## Usage

```sh
dot --deploy /data/dotfiles-repo
dot --force /data/dotfiles-repo
dot --pretend /data/dotfiles-repo
dot --info
dot --help
```


# LICENSE

[GNU General Public License, Version 3](https://www.gnu.org/licenses/gpl-3.0.en.html)
