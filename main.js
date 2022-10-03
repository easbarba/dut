#!/usr/bin/env node

const process = require('process');

const allOptions = {
    '-c': '--create',
}

function getOptions(options) {
    let arguments = options.splice(2);
    let longOptions = arguments.filter(option => option.startsWith('--')); // TODO regex to identify if a word follows --
    let shortOptions = arguments.filter(option => option.startsWith('-')); // TODO regex to identify if a single letter follows -

    if (longOptions == !0) return longOptions;

    let finalOptions = longOptions;

    return finalOptions;
}

function run() {
    // switch (arguments[1]) { // check that `say` is the first "command"
    //     case 'say':
    //         let options = arguments.slice(1); // get the stuff after `say`
    //         let optionsObject = {} // key-value representation
    //         if (options.indexOf("--user") != -1) { // if it exists
    //             optionsObject.user = options[options.indexOf("--user") + 1]
    //         }
    //         else {
    //             // you can throw an error here
    //         }
    //         if (options.indexOf("--msg") != -1) { // if it exists
    //             optionsObject.msg = options[options.indexOf("--msg") + 1]
    //         }
    //         if (options.indexOf("--random") != -1) { // if it exists
    //             optionsObject.random = true
    //         }
    //         console.log(optionsObject) // you can use optionsObject for your program
    //         break;
    //     default:
    //         console.log("Invalid command");
    // }
}

options = process.argv

if (!options.slice(2).length) {
    console.warn('Error: Not enough options has been passed');
    process.exit(1);
}

// console.log(getOptions(options));
