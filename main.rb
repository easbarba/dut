#!/usr/bin/env ruby
# frozen_string_literal: true

require 'pathname'
require 'find'
require 'optparse'
require 'fileutils'

# list of filtered files/folders to link
class Farm
  HOME = Pathname.new Dir.home

  attr_reader :all

  def initialize(options)
    @from = options[:from]
    @destination = options[:destination].nil? ? HOME : options[:destination]
    @all = {}.tap { |f| all_items[:files].each { |t| f.store(t, destination_dir(t)) } }
  end

  # ignore these dotfiles
  def dotignored
    dots = @from.join('.dotsignore').read.split "\n"
    dots.append '.dotsignore' # ignore itself too, ofc!
    dots.uniq # users may not notice duplicates.
  end

  # is ITEM included in from folder?
  def ignore?(item)
    dotignored.map { |x| item.to_path.include? @from.join(x).to_path }.any?
  end

  # organize listed items in .dotsignore as pathname
  def all_items
    { files: [], folders: [] }.tap do |x|
      Find.find(@from) do |item|
        item = Pathname.new item

        next if item == @from # skip the from folder itself
        next if ignore? item

        item.file? ? x[:files] << item : x[:folders] << item
      end
    end
  end

  # transform  stringfied origin item's from absolute path to home
  # /a/b/c.tar --> /home/b/c.tar
  def destination_dir(item)
    Pathname.new(item.to_path.gsub(@from.to_path, @destination.to_path))
  end
end

# Mirrors, by symlinking, a dotfiles repository to $HOME or selected folder.
class Core
  HOME = Pathname.new Dir.home

  def initialize(options)
    @options = options
  end

  # do not symlink but create top folders of files if it does not exist
  def make_folder(link)
    folder = link.dirname
    return if folder.exist?

    # return if link == HOME # do not create the $HOME folder :/

    puts "Creating folder: #{folder}"
    FileUtils.mkdir_p folder unless @options[:pretend]
  end

  # move file from home to a /home/backup/{file}
  def backup_item(link)
    return unless link.exist?
    return if link.symlink?

    warn "backup: #{link} ❯ $HOME/.backup."
    home_backup_folder = HOME.join('.backup')
    FileUtils.mkdir_p home_backup_folder
    FileUtils.mv link, home_backup_folder unless @options[:pretend]
  end

  # delete symlink if symlink's target does not exist
  def remove_faulty_link(link)
    return if link.exist?
    return unless link.symlink? # skip as link is a symlink and aint faulty

    warn "purging broken link: #{link}"
    link.delete unless @options[:pretend]
  end

  def link_file(target, link)
    link.delete if @options[:overwrite] && link.exist?

    # unless forced to, skip linking file as it does exist and is a symbolic link.
    return if link.symlink?

    puts "linking: #{target} ❯ #{link}"
    link.make_symlink target unless @options[:pretend]
  end

  def fix_perm(link)
    return if link.symlink?
    return if @options[:pretend]

    puts "updating permission of #{link}"
    link.chmod 0o744
  end
end

# All actions operations
class Actions
  def initialize(options)
    @options = options
    @farm = Farm.new(options).all
    @core = Core.new(options)
  end

  def remove
    @farm.each do |_, link|
      link.delete if link.exist? && link.symlink?
    end
  end

  def pretend
    @farm.each do |target, link|
    end
  end

  def overwrite
    @farm.each do |target, link|
    end
  end

  def create
    @farm.each do |target, link| # As enumerator yielding folder to symlink
      @core.make_folder link
      @core.backup_item link
      @core.remove_faulty_link link
      @core.link_file target, link
      @core.fix_perm link
    end
  end

  def info
    puts <<~EOL
      ... General information ...

      from: #{@options[:from]}
      destination: #{@options[:destination]}
    EOL

    exit
  end
end

options = {}
oparser = OptionParser.new do |parser|
  parser.banner = 'Usage: dots [options]'

  parser.on('-f', '--from DIR', String, 'folder with dotfiles') do |from|
    options[:from] = Pathname.new(from).expand_path
  end

  parser.on('-t', '--to DIR', String, 'location where to link files') do |destination|
    options[:destination] = Pathname.new(destination).expand_path
  end

  parser.on('-c', '--create', 'create dotfiles links') do
    options[:create] = true
  end

  parser.on('-r', '--remove', 'remove created dotfiles links') do
    options[:remove] = true
  end

  parser.on('-o', '--overwrite', 'force recreating of dotfiles links') do
    options[:force] = true
  end

  parser.on('-p', '--pretend', 'mimic creating of symbolic links') do
    options[:pretend] = true
  end

  parser.on('-i', '--info', 'general information of internals commands') do
    options[:info] = true
  end
end

oparser.parse! ['--help'] if ARGV.empty?
oparser.parse!

# RUN
actions = Actions.new(options)
actions.info if options[:info]
actions.remove if options[:remove]
actions.create if options[:create]
actions.pretend if options[:pretend]
actions.overwrite if options[:overwrite]
