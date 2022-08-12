// #!/usr/bin/java --source 17

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class Dots {
  public static void main(String[] args) {
    Cli cli = new Cli(args);
    cli.runOption();
  }
}

// TODO: use run() to manage trivial tasks while actions to properly handle
// unique task!
class Actions {
  interface MyNumber {
    void getValue(MyNumber n);
  }

  static Stream<Path> run(Path sDir, MyNumber func) {
    try {
      Files.walk(sDir, 999).forEach(func);
    } catch (IOException e) {
      System.out.println(e);
    }
  }

  public static void pretend(Path sDir) {
    MyNumber m = (n) -> {
      System.out.print("PRETENDING: ");
      System.out.println(n.toString());
    };

    run(sDir, m.getValue(m));
  }

  public static void force(Path sDir) {
    MyNumber m = (n) -> {
      System.out.print("FORCING: ");
      System.out.println(n.toString());
    };

    run(sDir, m.getValue(m));
  }

  public static void deploy(Path sDir) {
    MyNumber m = (n) -> {
      System.out.print("DEPLOYING: ");
      System.out.println(n.toString());
    };

    run(sDir, m.getValue(m));
  }
}

class Cli {
  public String option;
  public Path folder;

  public Cli(String[] m) {
    this.option = m[0];
    this.folder = getFolderPath(m[1]);
  }

  public Path getFolderPath(String fld) { return Paths.get(fld); }

  public String usage() {
    return "" "
        Usage : dots[options] -
                d,
        --deploy deploy dotfiles links - f,
        --force force redeployment of dotfiles links - p,
        --pretend mimic deployment of symbolic links - i,
        --info general information of internals commands "" ";
  }

  public void runOption() {
    switch (this.option) {
    case "-d":
      Actions.deploy(this.folder);
      break;
    case "-f":
      Actions.force(this.folder);
      break;
    case "-p":
      Actions.pretend(this.folder);
      break;
    default:
      System.out.println(usage());
    }
  }
}
