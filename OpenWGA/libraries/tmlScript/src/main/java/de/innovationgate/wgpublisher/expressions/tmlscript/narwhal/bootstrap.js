global = {
}

system = {
    global: global,
    engine: 'rhino',
    engines: ['rhino'],
    os: "",
    print: function (string) {
        Packages.java.lang.System.out.println(String(string));
    },
    evaluate: function (text) {
        return eval(
            "(function(require,exports,module,system,print){" +
            text +
            "/**/\n})"
        );
    },
    prefix: "",
    prefixes: [""],
    enginePrefix: "",
    debug: false,
    verbose: false
}

global.system = system;
global.global = global;
global.print = system.print;

require("global");
require("file");
require("file-engine");
require("system");

