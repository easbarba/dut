# Dot
Yet another simple and opinionated dot files manager.

# Specification:
 - `.dotsignore` list all undesirable to link files, gitignore's syntax is expected.
 - Folders are created not linked to avoid common issues.
 - remove links
 - overwrite links
 - pretend mode.
 - remove faulty symbolic links, if found.
 - backup non-symbolic link files to `$HOME/.backup` folder.
 - argp-like CLI interface.
 - GNU-Linux/BSD distribution only.

# Options

| option      | description                                                |
|-------------+------------------------------------------------------------|
| --to        | folder to deliver symbolic links                           |
| --from      | source folder with all dotfiles                            |
| --overwrite | overwrite existent links, usually good to fix broken links |
| --clean     | remove all links from source folder                        |
| --pretend   | demonstrate files linking                                  |
| --create    | create links of dotfiles                                   |

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
- Purge all symbolic linked files
- Keep history of early commands
- Accept git commit sha as source to symlink creating.
- Read-only symlinks.
- dotsignore to accept hash-like folder. eg: .config{foo,bar,meh,forevis}

# LICENSE

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0)
