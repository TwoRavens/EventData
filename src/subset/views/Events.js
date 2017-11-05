function addEvents() {
    let events = $('#eventElements');

    events.empty();
    for (let event in eventData) {
        let eventElem = document.createElement("div");
        eventElem.innerHTML = event + eventData[event];
        events.append(eventElem);
    }
}