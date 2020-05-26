function WeatherJs(target) {
    /** This is an API key for openweathermap.org based on a free acounnt.
     *  It is meant for demonstration purposes only, in the context of the
     *  Hands-on Scala.js book. Please do not use elsewhere. You can register
     *  for a free account for yourself if you want to use elsewhere.
     */
    var APIKey = "f7a8c8b3591ab63b725c2373a01f474a";

    var xhr = new XMLHttpRequest()

    xhr.open("GET",
        "https://api.openweathermap.org/data/" +
        "2.5/weather?q=Singapore" +
        "&APPID=" + APIKey
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
