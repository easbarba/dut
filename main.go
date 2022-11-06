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
	root := flag.String("deploy", "", "deploy dotfiles links")
	force := flag.Bool("force", false, "force redeployment of dotfiles links")
	flag.Parse()

	if *root == "" {
		flag.Usage()
		os.Exit(1)
	}

	ignore, err := ioutil.ReadFile(filepath.Join(*root, ".dutignore"))

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
