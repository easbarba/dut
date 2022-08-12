package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	root := flag.String("deploy", "", "deploy dotfiles links")
	force := flag.Bool("force", false, "force redeployment of dotfiles links")
	flag.Parse()

	if *root == "" {
		flag.Usage()
		os.Exit(1)
	}

	ignore, err := ioutil.ReadFile(filepath.Join(*root, ".dotsignore"))

	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	fixed_ignored := strings.Split(string(ignore), "\n")
	fixed_root := filepath.Clean(*root)
	crawler(fixed_root, fixed_ignored, *force)
}

func crawler(root string, ignored []string, force bool) {
	filepath.Walk(root,
		func(path string, info os.FileInfo, err error) error {
			if err != nil {
				fmt.Println(err)
				return err
			}

			// check if it is to ignore file
			if filterOut(root, ignored) {
				return nil
			}

			linkFile(path, force)
			return nil
		})

	println("")
	fmt.Println("Ignored: ", ignored)
	fmt.Print("Root: ", root)
}

// ignore file if its is in .dotsignored
func filterOut(root string, ignored []string) bool {
	for _, item := range ignored {
		rooted_item := filepath.Join(root, item)
		if item == "" || strings.HasPrefix(root, rooted_item) {
			return true
		}
	}

	return false
}

func linkFile(path string, force bool) {
	if force {
		fmt.Print("Forcing re-linking")
	}

	// finally, print file to be linked
	fmt.Println(path)
}
