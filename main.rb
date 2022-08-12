#!/usr/bin/env ruby
# coding: utf-8
# frozen_string_literal: true

# TODO: Accept git commit sha as source to symlink deployment.
# TODO: Read-only symlinks.
# TODO: dotsignore to accept hash-like folder. eg: .config{foo,bar,meh,forevis}

require 'pathname'
require 'find'
require 'optparse'
require 'fileutils'

# Mirrors, by symlinking, a dotfiles repository to $HOME or selected folder.
class Main
  HOME = Pathname.new Dir.home

  attr_reader :pretend, :to, :from, :overwrite

  def initialize(options)
    @options = options

    @from = options[:from]
    @to = options[:to].nil? ? HOME : options[:to]
    @pretend = options[:pretend]
    @overwrite = options[:overwrite]

    @farm = {}.tap { |f| all_items[:files].each { |t| f.store(t, to_dir(t)) } }
  end

  # ignore these dotfiles
  def dotignored
    dots = from.join('.dotsignore').read.split "\n"
    dots.append '.dotsignore' # ignore itself too, ofc!
    dots.uniq # users may not notice duplicates.
  end

  # is ITEM included in from folder?
  def ignore?(item)
    dotignored.map { |x| item.to_path.include? from.join(x).to_path }.any?
  end

  # organize listed items in .dotsignore as pathname
  def all_items
    { files: [], folders: [] }.tap do |x|
      Find.find(from) do |item|
        item = Pathname.new item

        next if item == from # skip the from folder itself
        next if ignore? item

        item.file? ? x[:files] << item : x[:folders] << item
      end
    end
  end

  # transform  stringfied origin item's from absolute path to home
  # /a/b/c.tar --> /home/b/c.tar
  def to_dir(item)
    home_path = to.to_path.concat('/') # / is needed to crop enterily item_path from path

    Pathname.new(item.to_path.gsub(from.to_path, home_path))
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
    FileUtils.mv link, HOME.join('.backup') unless @options[:pretend]
  end

  # delete symlink if symlink's target does not exist
  def rm_faulty_link(link)
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

    puts "updating permission of #{link}"
    link.chmod 0o744
  end

  def deploy
    @farm.each do |target, link| # As enumerator yielding folder to symlink
      make_folder link
      backup_item link
      rm_faulty_link link
      link_file target, link
      fix_perm link
    end
  end

  def info
    puts <<~EOL
      ... General information ...

      from: #{@options[:from]}
      to: #{@options[:to]}
    EOL

    exit
  end

  def run
    info if @options[:info]

    puts 'pretend mode' if @options[:pretend]
    deploy if @options[:deploy] || @options[:overwrite] || @options[:pretend]
  end
end

options = {}
oparser = OptionParser.new do |parser|
  parser.banner = 'Usage: dots [options]'

  parser.on('-f', '--from DIR', String, 'folder with dotfiles') do |from|
    options[:from] = Pathname.new(from)
  end

  parser.on('-t', '--to DIR', String, 'location where to link files') do |to|
    options[:to] = Pathname.new(to)
  end

  parser.on('-d', '--deploy', 'deploy dotfiles links') do
    options[:deploy] = true
  end

  parser.on('-o', '--overwrite', 'force redeployment of dotfiles links') do
    options[:force] = true
  end

  parser.on('-p', '--pretend', 'mimic deployment of symbolic links') do
    options[:pretend] = true
  end

  parser.on('-i', '--info', 'general information of internals commands') do
    options[:info] = true
  end
end

oparser.parse! ['--help'] if ARGV.empty?
oparser.parse!

# Gooo
p Main.new(options).run
