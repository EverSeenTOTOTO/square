#!/usr/bin/env node
/* eslint-disable @typescript-eslint/no-var-requires */
const readline = require('readline');
const fs = require('fs');
const { evaluate, createGlobalEnv } = require('./dist/index.js');

const files = process.argv.slice(2);

if (files.length === 0) {
  console.log('Welcome to [square].');

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: 'sq> ',
  });
  const env = createGlobalEnv(fs);

  rl.prompt();
  rl.on('line', (line) => {
    try {
      const content = evaluate(line, env)[0];

      console.log(content);
    } catch (e) {
      console.error(e);
    } finally {
      rl.prompt();
    }
  });
} else {
  for (const file of files) {
    try {
      const content = fs.readFileSync(file, 'utf8');

      evaluate(content, createGlobalEnv(fs));
    } catch (e) {
      console.error(e);
    }
  }
}
