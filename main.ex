#! /usr/bin/env elixir

# Description: An opitionated dotfile deployer base on guix home and nix homemanager.

# TODO: walk through directories and perform actions per folder
# TODO: Accept git commit sha as source to symlink deployment.
# TODO: Read-only symlinks.
# TODO: dotsignore to accept hash-like folder. eg: .config{foo,bar,meh,forevis}
# TODO: set minimal permission to 0744


# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.


defmodule Dots do
  defp ignored(root, {:ok, files}) do
    File.stream!(files)
    |> Enum.map(&String.trim(&1))
    |> Enum.concat([".dutignore"])
    |> Enum.map(&Path.join(root, &1))
    |> MapSet.new()
  end

  defp ignored(_root, {:error, nil}) do
    []
  end

  defp ignored_exist?(root) do
    root = Path.join(root, ".dutignore")
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
