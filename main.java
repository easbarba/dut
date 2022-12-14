///usr/bin/env jbang "$0" "$@" ; exit $?
//DEPS info.picocli:picocli:4.6.3

// Instalation: curl -Ls https://sh.jbang.dev | bash -s - app setup

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;

@Command(name = "Dot", mixinStandardHelpOptions = true, version = "Dot 0.1", description = "create symbolic links of a folder mirroring its tree structure into $HOME or custom folder")
class Main implements Callable<Integer> {

  @Option(names = { "-o", "--overwrite" }, description = "overwrite existent links.")
  private boolean overwrite;

  @Option(names = { "-p", "--pretend" }, description = "demonstrate files linking.")
  private boolean pretend;

  @Option(names = { "-c", "--create" }, description = "create links of dotfiles.")
  private boolean create;

  @Option(names = { "-i", "--info" }, description = "provide more information.")
  private boolean information;

  @Option(names = { "-f",
      "--from" }, paramLabel = "FOLDER", description = "source folder with all dotfiles.", required = true)
  String source;

  @Option(names = { "-t", "--to" }, paramLabel = "FOLDER", description = "folder to deliver symbolic links.")
  File destination;

  public static void main(String... args) {
    int exitCode = new CommandLine(new Main()).execute(args);
    System.exit(exitCode);
  }

  String infoList() {
    var result = String.format("-- information -- \n from: %s - to: %s - over: %s - pret: %s - create: %s\n", source,
        destination, overwrite, pretend, create);

    return result;
  }

  @Override
  public Integer call() throws Exception {
    // if (source.isEmpty()) {
    // }

    if (information) {
      System.out.println(infoList());
    }

    apply();

    return 0;
  }

  void apply() {
    Path start = FileSystems.getDefault().getPath(source);

    try {
      Files.walk(start)
          .filter(
              path -> ignore.ignoredOnes.stream().map(i -> Path.of(source, i).toString().startsWith(path.toString())))
          .forEach(link -> {
            link.filter(path -> path.toFile().isFile());

            var actions = new Actions(source);
            var ignore = new Ignored(source);
            // System.out.println(String.format("Ignored: %s", ignore.finaList()));

            if (create) {
              Actions.create(link);
            }

            if (remove) {
              Actions.remove(link);
            }

          });
    } catch (IOException e) {
      System.out.println(e);
    }
  }
}

class Ignored {
  String source;

  public Ignored(String source) {
    this.source = source;
  }

  final String[] defaultOnes = { ".git", ".dutignore" };

  List<String> ignoredOnes() {
    var dotsFile = Path.of(this.source, ".dutignore");
    List<String> dots = null;

    try {
      Stream<String> listedDots = Files.lines(dotsFile);
      dots = listedDots.distinct().sorted(Comparator.reverseOrder()).collect(Collectors.toList());

      listedDots.close();
    } catch (IOException e) {
      System.out.println("Caught " + e);
    }

    return dots;
  }

  public List<String> finaList() {
    List<String> result = new ArrayList<>(Arrays.asList(defaultOnes));
    result.addAll(ignoredOnes());

    return result;
  }
}

class Actions {
  String source;
  String destination;
  String home = System.getProperty("user.home");
  Helpers helpers = new Helpers();

  public Actions(String source, String destination) {
    this.source = source;
    this.destination = destination;
  }

  public Actions(String source) {
    this.source = source;
  }

  public void create(String link) {
    helpers.make_folder(link);
    helpers.backup_item(link);
    helpers.link_file(link, link); // target
    helpers.fix_perm(link);
  }

  public void clean(String link) {
    helpers.rm_faulty_link(link);
  }

  public void overwrite(String link) {
    helpers.link_file(link, link); // target
    helpers.fix_perm(link);
  }

  public void pretend(String link) {
    System.out.println(link, "->", "homey");
  }
}

class Helpers {
  public void make_folder(String link) {
    throw new UnsupportedOperationException("not implemented");
  }

  public void backup_item(String link) {
    throw new UnsupportedOperationException("not implemented");
  }

  public void remove_faulty_link(String link) {
    throw new UnsupportedOperationException("not implemented");
  }

  public void link_file(String target, String link) {
    throw new UnsupportedOperationException("not implemented");
  }

  public void fix_permission(String link) {
    throw new UnsupportedOperationException("not implemented");
  }
}
