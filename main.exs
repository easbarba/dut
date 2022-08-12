#! /usr/bin/env elixir

# Description: An opitionated dotfile deployer base on guix home and nix homemanager.

# Features:
# - the dotsignore file at the folder root behaves just like git's one ignoring undesired dotfiles.
# - Folders are not symlinked but created.
# - dry-run mode
# - remove faulty symlinks if found
# - backup non-symlink files to $HOME/.backup
# - fully implemented cli interface
# - GNU-Linux/BSD distros only

# TODO: walk through directories and perform actions per folder
# TODO: Accept git commit sha as source to symlink deployment.
# TODO: Read-only symlinks.
# TODO: dotsignore to accept hash-like folder. eg: .config{foo,bar,meh,forevis}
# TODO: set minimal permission to 0744

defmodule Dots do
  defp ignored(root, {:ok, files}) do
    File.stream!(files)
    |> Enum.map(&String.trim(&1))
    |> Enum.concat([".dotsignore"])
    |> Enum.map(&Path.join(root, &1))
    |> MapSet.new()
  end

  defp ignored(_root, {:error, nil}) do
    []
  end

  defp ignored_exist?(root) do
    root = Path.join(root, ".dotsignore")
    if File.exists?(root), do: {:ok, root}, else: {:error, nil}
  end

  defp ignore_me?(root, item) do
    ignored(root, ignored_exist?(root))
    |> Enum.any?(&String.starts_with?(item, &1))
  end

  def ls_r(path) do
    cond do
      File.regular?(path) ->
        [path]

      File.dir?(path) ->
        File.ls!(path)
        |> Enum.map(&Path.join(path, &1))
        |> Enum.map(&ls_r/1)
        |> Enum.concat()

      true ->
        []
    end
  end

  def run(root) do
    items = ls_r(root)

    for item <- items do
      unless ignore_me?(root, item) do
        target = item
        link_name = to_home(item, root)

        make_folder(link_name)
        link_file(target, link_name)
      end
    end
  end

  def to_home(item, root) do
    # /data/dots/.config/mpd/mpd.conf to $HOME/.config/mpd/mpd.conf
    String.replace(item, root, System.user_home())
  end

  def make_folder(link_name) do
    link_dir = Path.dirname(link_name)

    unless File.exists?(link_dir) do
      File.mkdir_p!(link_dir)
    end
  end

  def link_file(target, link_name) do
    unless File.exists?(link_name) do
      IO.puts("#{target} -> #{link_name}")
      File.ln_s!(target, link_name)
    end
  end

  def deploy(root) do
    run(root)
  end

  def pretend(root) do
    IO.puts("pretend-mode")
    IO.inspect(root)
  end

  def info(root) do
    IO.puts("root: #{root}")
  end
end

defmodule CLI do
  def main(args) do
    args
    |> OptionParser.parse(
      switches: [deploy: :string, pretend: :string, help: :boolean],
      aliases: [D: :deploy, P: :pretend, H: :help]
    )
    |> elem(0)
    |> run()
  end

  def help do
    IO.puts("Usage: dots [options]
  -D, --deploy                    symlink all dotfiles
  -P, --pretend                   pretend to symlink all dotfiles
  -H, --help                      cli options information")

    System.halt(0)
  end

  def run(deploy: root) do
    root = Path.dirname(IO.chardata_to_string(root))
    Dots.deploy(root)
  end

  def run(pretend: root) do
    root = Path.dirname(IO.chardata_to_string(root))
    Dots.pretend(root)
  end

  def run(help: true) do
    help()
  end

  def run(_) do
    help()
  end
end

CLI.main(System.argv())
