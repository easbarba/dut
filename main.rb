#!/usr/bin/env ruby
# coding: utf-8
# frozen_string_literal: true

# Description: An opitionated dotfile deployer base on guix home and nix homemanager.

# Features:
# - the dotsignore file at the folder root behaves just like git's one ignoring undesired dotfiles.
# - Folders are not symlinked but created.
# - dry-run mode
# - remove faulty symlinks if found
# - backup non-symlink files to $HOME/.backup
# - fully implemented cli interface
# - GNU-Linux/BSD distros only
#
# TODO: Accept git commit sha as source to symlink deployment.
# TODO: Read-only symlinks.
# TODO: dotsignore to accept hash-like folder. eg: .config{foo,bar,meh,forevis}

require 'pathname'
require 'find'
require 'optparse'
require 'fileutils'

# Mirrors, by symlinking, a dotfiles repository to $HOME.
class Main
  HOME = Pathname.new Dir.home

  attr_reader :root, :home, :farm, :pretend

  def initialize(root)
    @root = Pathname.new(root)
    @farm = {}.tap { |f| all_items[:files].each { |t| f.store(t, to_home(t)) } }
  end

  # ignore these dotfiles
  def dotignored
    dots = root.join('.dotsignore').read.split "\n"
    dots.append '.dotsignore' # ignore itself too, ofc!
    # dots.append '.git' if root.join('.git').exist? # ignore the .git folder.
    dots.uniq # users may not notice duplicated dotfiles.
  end

  # is ITEM included in root folder?
  def ignore?(item)
    dotignored.map { |x| item.to_path.include? root.join(x).to_path }.any?
  end

  # organize listed items in .dotsignore as pathname
  def all_items
    { files: [], folders: [] }.tap do |x|
      Find.find(root) do |item|
        item = Pathname.new item

        next if item == root # skip the root folder itself
        next if ignore? item

        item.file? ? x[:files] << item : x[:folders] << item
      end
    end
  end

  # transform  stringfied origin item's root absolute path to home
  # /a/b/c.tar --> /home/b/c.tar
  def to_home(item)
    home_path = HOME.to_path.concat('/') # / is needed to crop enterily item_path root path
    item_homed = item.to_path.gsub(root.to_path, home_path)

    Pathname.new item_homed
  end

  # do not symlink but create top folders of files if it does not exist
  def make_folder(link)
    folder = link.dirname
    return if folder.exist?

    # return if link == HOME # do not create the $HOME folder :/

    puts "Creating folder: #{folder}"
    FileUtils.mkdir_p folder unless pretend
  end

  # move file from home to a /home/backup/{file}
  def backup_item(link)
    return unless link.exist?
    return if link.symlink?

    warn "backup: #{link} ❯ $HOME/.backup."
    FileUtils.mv link, HOME.join('.backup') unless pretend
  end

  # delete symlink if symlink's target does not exist
  def rm_faulty_link(link)
    return if link.exist?
    return unless link.symlink? # skip as link is a symlink and aint faulty

    warn "removing: #{link} is a faulty simlink"
    link.delete unless pretend
  end

  def link_file(target, link, force)
    link.delete if force && link.exist?

    # unless forced to, skip linking file as it does exist and is a symbolic link.
    return if link.symlink?

    puts "linking: #{target} ❯ #{link}"
    link.make_symlink target unless pretend
  end

  def fix_perm(link)
    return if link.symlink?

    puts "updating permission of #{link}"
    link.chmod 0o744
  end

  def deploy(force: false)
    farm.each do |target, link| # As enumerator yielding folder to symlink
      make_folder link
      backup_item link
      rm_faulty_link link
      link_file target, link, force
      fix_perm link
    end
  end

  def info
    puts @root
  end

  def dry_run
    puts 'running on pretend mode'

    @pretend = true

    deploy
  end
end

def cli
  OptionParser.new do |opts|
    opts.banner = 'Usage: dots [options]'

    main = Main.new(ARGV[1]) unless ARGV.empty?

    opts.on('-d', '--deploy', 'deploy dotfiles links') do
      main.deploy
    end

    opts.on('-f', '--force', 'force redeployment of dotfiles links') do
      main.deploy(force: true)
    end

    opts.on('-p', '--pretend', 'mimic deployment of symbolic links') do
      main.dry_run
    end

    opts.on('-i', '--info', 'general information of internals commands') do
      main.info
    end
  end
end

cli.parse! ['--help'] if ARGV.empty?
cli.parse!
