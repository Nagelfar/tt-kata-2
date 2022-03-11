
const { Elm } = require('./elm.js');
const readline = require('readline');
var fs = require('fs');


module.exports = (name, opts) => {
  const program = Elm[name].init({
    flags: { argv: process.argv, versionMessage: "1.2.3" }
  });

  // program.ports.print.subscribe(message => {
  //   console.log(message)
  // });
  program.ports.load.subscribe (message => {
      console.debug("Loading ", message);
      fs.readFile(message, 'utf8', (err, data) => {
          if(err) throw err;
          program.ports.onFileLoaded.send(data);
      })
  });
  program.ports.printAndExitFailure.subscribe(message => {
    console.error(message);
    process.exit(1);
  });
  program.ports.printAndExitSuccess.subscribe(message => {
    console.log(message);
    process.exit(0);
  });

  if (opts && opts.stdin) {
    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      terminal: false
    });

    rl.on('line', function (line) {
      program.ports.onStdinLine.send(line);
    });

    rl.on('close', function (line) {
      program.ports.onStdinClosed.send(null);
    });
  }
};