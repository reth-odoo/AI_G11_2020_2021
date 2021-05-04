messages = []
LOGNB = 8
inpuBox = null;

function getInputBox() {
    return document.getElementById("textbox")
}


function getBotResponse() {
    return "not connected"
}

function send() {
    let inputBox = getInputBox();
    if (inputBox.value != "") {

        let userInput = inputBox.value;
        inputBox.value = "";
        messages.push("<b>You</b>: " + userInput);

        let botResponse = getBotResponse();
        messages.push("<b>quoridabot</b>: " + botResponse);

        for (let i = 1; i < LOGNB; i++) {
            if (messages[messages.length - i]) {
                document.getElementById("log" + i).innerHTML = messages[messages.length - i];
            }
        }
    }
}

document.onkeypress = (e) => {
    var x = e || window.event;
    var key = (x.keyCode || x.which);
    if (key == 13 || key == 3) {
        send();
    }
};