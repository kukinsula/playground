import * as fs from 'fs';

import * as json from 'streaming/json';
import * as mapper from 'streaming/mapper';

const program = require('commander');
const TailStream = require('tail-stream');
const chalk = require('chalk');

program
  .version('1.0.0')
  .option('-f', 'Path of the file to read')
  .parse(process.argv);

console.log('000', process.argv);
console.log('AAA', program.filepath);
console.log('BBB', program.f);

let input = TailStream.createReadStream(
  '/home/kuk/info/api.sirene.freelance.com/log/test.log.2019-02-10',

  {
    beginAt: 0,
    detectTruncate: true,
    onMove: 'follow', // or 'end' or 'exit' or 'stay'
    onTruncate: 'end', // or 'reset' to seek to beginning of file
    endOnError: false,
    useWatch: !!fs.watch,
    waitForCreate: true
  });
let parser = new json.Parser();
let map = new mapper.Mapper((log: any) => {
  let date = chalk.white.bold(log.timestamp);
  let level = log.level.toUpperCase();
  let message = chalk.white(log.message);

  switch (level) {
    case 'DEBUG': level = chalk.bold.hex('#009FFF')(level); break;
    case 'INFO': level = chalk.bold.green(level); break;
    case 'WARN': level = chalk.bold.hex('#FFB400')(level); break;
    case 'ERROR': level = chalk.bold.red(level); break;
    default: break;
  }

  return `${date} [${level}] ${message}\n`;
});

input

  .on('error', (err: any) => { console.log('ERROR INPUT', err); })

  .pipe(parser)

  .on('error', (err: any) => { console.log('ERROR PARSE', err); })

  .pipe(map)

  .on('error', (err: any) => { console.log('ERROR MAP', err); })

  .pipe(process.stdout)

  .on('error', (err: any) => { console.log('ERROR', err); })

  .on('finish', () => { console.log('Done!'); });
