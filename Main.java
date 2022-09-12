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

    // var create = new Create(source, null);
    // create.run();

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
      Stream<String> listedDots = Files.lines(dotsFile);
      dots = listedDots
        .distinct()
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

class Create {
  List<String> ignoredOnes;
  String source;
  String destination;
  String home = System.getProperty("user.home");

  public Create(String source, String destination, List<String> ignoredOnes) {
    this.source = source;
    this.destination = destination;
    this.ignoredOnes = ignoredOnes;
  }

  public void clean() {
    throw new UnsupportedOperationException("not implemented");
  }

  public void overwrite() {
    throw new UnsupportedOperationException("not implemented");
  }

  public void pretend() {
    throw new UnsupportedOperationException("not implemented");

  }

  void apply() {
    throw new UnsupportedOperationException("not implemented");
    // Path start = FileSystems.getDefault().getPath(source);

    // try {
    // Files.walk(start)
    // .filter(path -> ignoredOnes.stream().map(i -> Path.of(source,
    // i).toString().startsWith(path.toString())))
    // .forEach(link -> {
    // // .filter(path -> path.toFile().isFile())
    // make_folder(link);
    // backup_item(link);
    // rm_faulty_link(link);
    // link_file(link); // target
    // fix_perm(link);
    // });
    // } catch (IOException e) {
    // System.out.println(e);
    // }
  }

  private void make_folder(String link) {
    throw new UnsupportedOperationException("not implemented");
  }

  private void backup_item(String link) {
    throw new UnsupportedOperationException("not implemented");
  }

  private void remove_faulty_link(String link) {
    throw new UnsupportedOperationException("not implemented");
  }

  private void link_file(String target, String link) {
    throw new UnsupportedOperationException("not implemented");
  }

  private void fix_permission(String link) {
    throw new UnsupportedOperationException("not implemented");
  }
}
