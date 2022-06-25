#!/usr/bin/env node
/* eslint-disable @typescript-eslint/no-var-requires */
const fs = require('fs');
const { evaluate, createGlobalEnv } = require('./dist/index.js');

const files = process.argv.slice(2);
const errors = [];

for (const file of files) {
  try {
    const content = fs.readFileSync(file, 'utf8');
    evaluate(content, createGlobalEnv());
  } catch (e) {
    console.error(e);
    errors.push(e instanceof Error ? e : new Error(`Read and eval ${file} failed.`));
  }
}

console.log(`\n${files.length} scripts evaluated, ${files.length - errors.length} success, ${errors.length} fail.\n`);
