///usr/bin/env jbang "$0" "$@" ; exit $?
//DEPS info.picocli:picocli:4.6.3

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
import java.io.File;
import java.io.IOException;
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

  @Option(names = { "-f", "--from" }, paramLabel = "FOLDER", description = "source folder with all dotfiles.")
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
  public Integer call() throws Exception { // your business logic goes here...

    if (information)
      System.out.println(infoList());

    var ignore = new Ignored(source);
    System.out.println(String.format("Ignored: %s", ignore.finaList()));

    return 0;
  }
}

class Ignored {
  String source;

  public Ignored(String source) {
    this.source = source;
  }

  final String[] defaultOnes = { ".git", ".dotsignore" };

  List<String> ignoredOnes() {
    var dotsFile = Path.of(this.source, ".dotsignore");
    List<String> dots = null;

    try {
      var listedDots = Files.readAllLines(dotsFile);
      dots = new ArrayList<>(new HashSet<>(listedDots))
        .stream()
        .sorted(Comparator.reverseOrder())
        .collect(Collectors.toList());
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
