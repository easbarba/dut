package main

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

import (
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
)

var root, destination string

func homeDir() string {
	home, err := os.UserHomeDir()

	if err != nil {
		fmt.Println(err)
	}

	return home
}

func main() {
	actions, to, from := parse()

	root = filepath.Clean(*from) // remove trailing /
	destination = filepath.Clean(*to)

	crawler(root, ignoredFiles(), actions)
}

// HELPERS

func homey(file string) string {
	return strings.Replace(file, root, homeDir(), 1)
}

func mkdirp(file, newfile string) {
	rootfile, err := os.Open(file)
	if err != nil {
		println(err)
	}

	defer rootfile.Close()

	fileInfo, err := rootfile.Stat()
	if err != nil {
		println(err)
	}

	if fileInfo.IsDir() {
		// if homey folder does not exist, create
		if _, err := os.Stat(newfile); err != nil {
			println("Directory not found, creating!")

			err = os.Mkdir(newfile, 0755)
			if err != nil {
				panic(err)
			}
		}
	}
}

// ACTIONS

func info(ignored []string, root string) {
	println("")
	fmt.Println("Ignored: ", ignored)
	fmt.Println("Root: ", root)
	fmt.Println()
}

func create(file string) {
	// mkdirp(file, homey(file))
	fmt.Println(file, "-->", homey(file))
}

func overwrite(file string) {
	errors.New("Not implemented")
}

func pretend(file string) {
	errors.New("Not implemented")
}

func remove(file string) {
	errors.New("Not implemented")
}

// INTERNALS

// get all files to be ignored
func ignoredFiles() []string {
	var result []string
	var ignoreFileName = ".dutignore"
	ignoreList, err := ioutil.ReadFile(filepath.Join(root, ignoreFileName))

	// no .dutignore file found,
	// then set some sensible default files to ignore
	if err != nil {
		return []string{".git", ignoreFileName}
	}

	result = strings.Split(string(ignoreList), "\n")

	return append(result, ".git", ignoreFileName)
}

// command line arguments parser
func parse() (map[string]*bool, *string, *string) {
	create := flag.Bool("create", false, "create links of dotfiles")
	remove := flag.Bool("remove", false, "remove links from target folder")
	pretend := flag.Bool("pretend", false, "demonstrate files linking")
	overwrite := flag.Bool("overwrite", false, "overwrite existent links")
	info := flag.Bool("info", false, "provide additional information")

	actions := map[string]*bool{
		"create":    create,
		"remove":    remove,
		"pretend":   pretend,
		"overwrite": overwrite,
		"info":      info,
	}

	to := flag.String("to", "", "destination folder to deliver links")
	from := flag.String("from", "", "target folder with dotfiles")

	flag.Parse()

	flag.Usage = func() {
		fmt.Fprintf(flag.CommandLine.Output(), "dut, yet another simple dotfiles manager. \n\n")
		fmt.Fprintln(flag.CommandLine.Output(), "Usage information:")
		flag.PrintDefaults()
	}

	if *from == "" {
		flag.Usage()
		os.Exit(0)
	}

	return actions, to, from
}

// picker pick action selected
func picker(cFile string, actions map[string]*bool) {
	switch {
	case *actions["create"]:
		create(cFile)
	case *actions["remove"]:
		remove(cFile)
	case *actions["pretend"]:
		pretend(cFile)
	case *actions["overwrite"]:
		overwrite(cFile)
	default:
		fmt.Println("No action selected!")
	}
}

func crawler(root string, ignored []string, actions map[string]*bool) {
	if *actions["info"] {
		info(ignored, root)
	}

	filepath.Walk(root, func(current_file string, info os.FileInfo, err error) error {
		if err != nil {
			fmt.Println(err)
			return err
		}

		// ignore root folder
		if current_file == root {
			return nil
		}

		// ignore files listed in .dutignore
		if ignore_this(current_file, ignored) == false {
			picker(current_file, actions)
		}

		return nil
	})
}

// ignore_this ignores current file if either its absolutely path
// or its parent folder is listed in the .dutignored
func ignore_this(current_file string, ignored []string) bool {
	_, file_wo_root, _ := strings.Cut(current_file, root+"/")

	for _, item := range ignored {
		// ignore empty string
		if item == "" {
			break
		}

		if strings.HasPrefix(file_wo_root, item) == true {
			return true
		}
	}

	return false
}
