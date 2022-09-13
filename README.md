# Dot
Yet another simple and opinionated dot files manager.

# Specification:
 - Exactly mirror target folder structure.
 - Folders are created not linked to avoid common issues.
 - If a `.dotsignore` is present are files listed are to be ignored, eg: `LICENSE`.
 - `.git` is ignored by default.
 - Overwrite present links [optional].
 - Pretend mode [optional].
 - Remove faulty symbolic links found.
 - Back up non-symbolic link files to `$HOME/.backup` folder.
  - Unix-like distributions only.

# Options
    
| option      	| description                         	|
|-------------	|-------------------------------------	|
| --to        	| destination folder to deliver links   |
| --from      	| target folder with dotfiles      	|
| --create    	| create links of dotfiles            	|
| --clean     	| remove links from target folder   	|
| --pretend   	| demonstrate files linking           	|
| --overwrite 	| overwrite existent links            	|

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
- `.dotsignore` support hash-like folder. eg: .config{foo,bar,meh,forevis}
- Rollback feature.

# LICENSE

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0)
