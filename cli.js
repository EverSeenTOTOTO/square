#!/usr/bin/env node
/* eslint-disable @typescript-eslint/no-var-requires */
const fs = require('fs');
const { evaluate, createGlobalEnv } = require('./dist/index.js');

const files = process.argv.slice(2);

for (const file of files) {
  const content = fs.readFileSync(file, 'utf8');
  evaluate(content, createGlobalEnv());
}
