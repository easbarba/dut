# Dot
Yet another simple and opinionated dot files manager.

| lang   | completion |
|--------|------------|
| ruby   | 97%        |
| bash   | 16%        |
| guile  | 12%        |
| golang | 10%        |

# Specification:
 - Exactly mirror target folder structure.
 - Folders are created not linked to avoid common issues.
 - If a `.dutignore` is present are files listed are to be ignored, eg: `LICENSE`.
 - `.git` is ignored by default.
 - Remove all linked files, but preserve folders.
 - Overwrite present links [optional].
 - Pretend mode [optional].
 - Remove faulty symbolic links found.
 - Back up non-symbolic link files to `$HOME/.backup` folder.
 - Unix-like distributions only.

# Command-line options
    
| Option         | Description                          |
|----------------|--------------------------------------|
| --to DIR, -t   | destination folder to deliver links  |
| --from DIR, -f | target folder with dotfiles          |
| --create, c    | create links of dotfiles             |
| --remove, r    | remove links from target folder      |
| --pretend, p   | demonstrate files linking            |
| --overwrite, o | overwrite existent links             |
| --info, i      | provide additional information       |

## Usage

```sh
dot --from /data/dotfiles-repo --create
dot --from /data/bin --to $HOME/.local/bin --overwrite
dot --from /data/bin --to $HOME/.local/bin --clean
dot --from /data/dotfiles-repo --pretend
dot --info
dot --help
```

## TODO
- Do not overwrite symbolic links found
- Keep history of early commands.
- Git commit SHA as source to link.
- Read-only symbolic links.
- `.dutignore` support hash-like folder. eg: .config{foo,bar,meh,forevis}
- Rollback feature.
- Log operations
- JSON output of last files linked operation, useful for rollback
- Back up follows structure of folder `.config/code/user/settings.init` to `$HOME/.backup/.config/code/user/settings.init`
- If there is more than one file on backup folder with same name, prepend current time

# LICENSE

[GPL-v3](https://www.gnu.org/licenses/gpl-3.0.en.html)
