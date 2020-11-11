var system = require('system');
var args = system.args;
if (args.length === 3) {
    console.log("Argument passed");
    var url = args[1];
    var filelocation = args[2];
    console.log("url-->" + url);
    var booleanCorrectInput = true;
} else {
    var booleanCorrectInput = false;
    console.log("Pass some arguments");
    throw new Error("Pass some arguments!");
}

if(booleanCorrectInput === true){
    var page = require('webpage').create();
    page.viewportSize = { width: 1000, height: 800 };
    page.paperSize = {
        width: 1100, height: 1000,
        margin: '0px'
    };
    page.settings.dpi = "120";

    page.open(url, function (status) {
        if (status !== 'success') {
            console.log('Unable to load the address!');
            phantom.exit();
        } else {
            window.setTimeout(function () {
                page.render(filelocation, { format: 'pdf', quality: '100' });
                phantom.exit();
            }, 2000); // Change timeout as required to allow sufficient time
        }
    });
}
