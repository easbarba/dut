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
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	create, remove, pretend, overwrite, info, destination, root := parse()
	fmt.Println(*create, *remove, *pretend, *overwrite, *destination, *info, *root)

	*root = filepath.Clean(*root) // remove trailing /
	ignored := ignoredFiles(root)

	crawler(*root, ignored)
}

// ACTIONS

func info(ignored []string, root string) {
	println("")
	fmt.Println("Ignored: ", ignored)
	fmt.Print("Root: ", root)
}

func create(file string) {
	fmt.Println("linking:", file)
}

func overwrite() {
	fmt.Print("overwriting")
}

func pretend() {}

func remove() {}

// INTERNALS

// get all files to be ignored
func ignoredFiles(root *string) []string {
	var result []string
	ignore, err := ioutil.ReadFile(filepath.Join(*root, ".dutignore"))

	// no .dutignore file found,
	// then set some sensible default files to ignore
	if err != nil {
		return []string{".git", ".dutignore"}
	}

	result = strings.Split(string(ignore), "\n")

	return result
}

func parse() (*bool, *bool, *bool, *bool, *bool, *string, *string) {
	create := flag.Bool("create", false, "create links of dotfiles")
	remove := flag.Bool("remove", false, "remove links from target folder")
	pretend := flag.Bool("pretend", false, "demonstrate files linking")
	overwrite := flag.Bool("overwrite", false, "overwrite existent links")
	info := flag.Bool("info", false, "provide additional information")
	to := flag.String("to", "", "destination folder to deliver links")
	from := flag.String("from", "", "target folder with dotfiles")

	flag.Parse()

	if *to == "" {
		flag.Usage()
		os.Exit(1)
	}

	return create, remove, pretend, overwrite, info, to, from
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

// ignore file if its is in .dutignored
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
