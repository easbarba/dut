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

var root, destination string
var homeDir, _ = os.UserHomeDir()

func main() {
	create, remove, pretend, overwrite, info, to, from := parse()
	fmt.Println(*create, *remove, *pretend, *overwrite, *to, *info, *from)

	root = filepath.Clean(*from) // remove trailing /
	destination = filepath.Clean(*to)

	crawler(root, ignoredFiles())
}

// ACTIONS

func info(ignored []string, root string) {
	println("")
	fmt.Println("Ignored: ", ignored)
	fmt.Print("Root: ", root)
}

func create(file string) {
	homey_file := strings.Replace(file, root, homeDir, 1)
	fmt.Println(file, "-->", homey_file)
}

func overwrite() {
	fmt.Print("overwriting")
}

func pretend() {}

func remove() {}

// INTERNALS

// get all files to be ignored
func ignoredFiles() []string {
	var result []string
	var ignoreFile = ".dutignore"
	ignore, err := ioutil.ReadFile(filepath.Join(root, ignoreFile))

	// no .dutignore file found,
	// then set some sensible default files to ignore
	if err != nil {
		return []string{".git", ignoreFile}
	}

	result = strings.Split(string(ignore), "\n")
	result = append(result, ignoreFile)

	return result
}

// command line arguments parser
func parse() (*bool, *bool, *bool, *bool, *bool, *string, *string) {
	create := flag.Bool("create", false, "create links of dotfiles")
	remove := flag.Bool("remove", false, "remove links from target folder")
	pretend := flag.Bool("pretend", false, "demonstrate files linking")
	overwrite := flag.Bool("overwrite", false, "overwrite existent links")
	info := flag.Bool("info", false, "provide additional information")

	to := flag.String("to", "", "destination folder to deliver links")
	from := flag.String("from", "", "target folder with dotfiles")

	flag.Parse()

	if *from == "" {
		flag.Usage()
		os.Exit(1)
	}

	return create, remove, pretend, overwrite, info, to, from
}

func crawler(root string, ignored []string) {
	filepath.Walk(root, func(current_file string, info os.FileInfo, err error) error {
		if err != nil {
			fmt.Println(err)
			return err
		}

		if ignore_this(current_file, ignored) {
			return nil
		}

		create(current_file)

		return nil
	})

	info(ignored, root)
}

// ignore file if its is in .dutignored
func ignore_this(current_file string, ignored []string) bool {
	_, ignored_prefix, _ := strings.Cut(current_file, root+"/")

	for _, item := range ignored {
		if strings.HasPrefix(item, ignored_prefix) {
			// fmt.Println("ignoring:", item)
			return true
		}
	}

	return false
}
