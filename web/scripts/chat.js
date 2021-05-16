messages = []
LOGNB = 8
inpuBox = null;

function getInputBox() {
    return document.getElementById("textbox")
}


function getBotResponse(field, userInput) {
    let oReq = new XMLHttpRequest();
    oReq.overrideMimeType("application/json");
    oReq.addEventListener('loadend', (event) => {
        let resp_event = event.currentTarget;
        if (resp_event.status != 200) {
            field.nodeValue = `The server could not provide a response ${resp_event.status} ${oReq.response.message}`;
            return;
        }

        let resp = resp_event.response;
        if (resp.message) {
            field.nodeValue = resp.message;
            return;
        }

        field.nodeValue = 'The response did not contain a message';
        return;

    });

    oReq.responseType = "json";
    oReq.open("POST", "/chat");
    oReq.setRequestHeader('Content-Type', 'application/json');
    let stateJson = constructStateJson();
    stateJson["message"] = userInput;
    console.log(stateJson)
    oReq.send(JSON.stringify(stateJson));

}

function sendChat() {
    let inputBox = getInputBox();
    if (inputBox.value != "") {

        let userInput = inputBox.value;
        inputBox.value = "";

        //user message
        let userLogP = document.createElement('p');
        //bold
        let idb = document.createElement('b');
        let idText = document.createTextNode("You: ");
        idb.appendChild(idText);
        userLogP.appendChild(idb);
        //message
        let userLog = document.createTextNode(userInput);
        userLogP.appendChild(userLog);

        //bot message
        let botLogP = document.createElement('p');
        //bold
        idb = document.createElement('b');
        idText = document.createTextNode("QuoridAi: ");
        idb.appendChild(idText);
        botLogP.appendChild(idb);
        //message
        let botLog = document.createTextNode("fetching...");
        botLogP.appendChild(botLog);

        getBotResponse(botLog, userInput);

        let chatLog = document.getElementById("chatlog");
        chatlog.appendChild(userLogP);
        chatlog.appendChild(botLogP);
        chatlog.scrollTop = chatlog.scrollHeight;
    }
}

let sendChatHandler = (e) => {
    var x = e || window.event;
    var key = (x.keyCode || x.which);
    if (key == 13 || key == 3) {
        if (getInputBox() == document.activeElement)
            sendChat();
    }
}