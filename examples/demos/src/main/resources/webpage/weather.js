function WeatherJs(target) {
    var xhr = new XMLHttpRequest()

    xhr.open("GET",
        "http://api.openweathermap.org/data/" +
        "2.5/weather?q=Singapore&appid=4ef01dbbb326222af5ec69053f824bde"
    );

    xhr.onload = function (e) {
        if (xhr.status == 200) {
            var pre = document.createElement("pre");
            pre.textContent = xhr.responseText;
            target.appendChild(pre);
        }
    };
    xhr.send();
}