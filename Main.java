///usr/bin/env jbang "$0" "$@" ; exit $?
//DEPS info.picocli:picocli:4.6.3

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;

import java.util.concurrent.Callable;
import java.io.File;

@Command(
    name = "Dot", mixinStandardHelpOptions = true, version = "Dot 0.1",
    description =
        "create symbolic links of a folder mirroring its tree structure into $HOME or custom folder")
class Main implements Callable<Integer> {

  @Option(names = { "-o", "--overwrite" }, description = "overwrite existent links")
  private boolean overwrite;

  @Option(names = { "-p", "--pretend" }, description = "demonstrate files linking")
  private boolean pretend;

  @Option(names = { "-c", "--create" }, description = "create links of dotfiles")
  private boolean create;

  @Option(names = { "-f", "--from" }, paramLabel = "FOLDER", description = "source folder with all dotfiles")
  File source;

  @Option(names = { "-t", "--to" }, paramLabel = "FOLDER", description = "folder to deliver symbolic links")
  File destination;

  public static void main(String... args) {
    int exitCode = new CommandLine(new Main()).execute(args);
    System.exit(exitCode);
  }

  @Override
  public Integer call() throws Exception { // your business logic goes here...
    System.out.println("FROM: " + source);
    System.out.println("TO: " + destination);
    return 0;
  }
}
