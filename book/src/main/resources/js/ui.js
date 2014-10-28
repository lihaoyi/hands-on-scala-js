document.addEventListener("DOMContentLoaded", function(){
    console.log("HIGHLIGHTING")

    var layout   = document.getElementById('layout'),
        menu     = document.getElementById('menu'),
        menuLink = document.getElementById('menuLink');

    function toggleClass(element, className) {
        var classes = element.className.split(/\s+/),
            length = classes.length,
            i = 0;

        for(; i < length; i++) {
          if (classes[i] === className) {
            classes.splice(i, 1);
            break;
          }
        }
        // The className is not found
        if (length === classes.length) {
            classes.push(className);
        }

        element.className = classes.join(' ');
    }

    menuLink.onclick = function (e) {
        var active = 'active';

        e.preventDefault();
        toggleClass(layout, active);
        toggleClass(menu, active);
        toggleClass(menuLink, active);
    };


    var snippets = document.getElementsByClassName("highlight-me");
    for(var i = 0; i < snippets.length; i++){
        console.log("highlighting", snippets[i])
        hljs.highlightBlock(snippets[i])
    }




    // Cache selectors
    var lastId = -1;
    var main = document.getElementById("main");

    scrollItems = document.getElementsByClassName("menu-item");

    scrollHeaders = []

    for(var i = 0; i < scrollItems.length; i++){
        scrollHeaders.push(
            document.getElementById(
                scrollItems[i].getAttribute("href").substring(1)
            )
        )
    }


    function isElementInViewport (el) {
        var rect = el.getBoundingClientRect();

        return (
            rect.top >= 0 &&
            rect.left >= 0 &&
            rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) && /*or $(window).height() */
            rect.right <= (window.innerWidth || document.documentElement.clientWidth) /*or $(window).width() */
        );
    }
    main.addEventListener("scroll", function(){
        // Get container scroll position
        var fromTop = main.scrollTop;

        // Get id of current scroll item
        for(var i = scrollItems.length - 1; i >= 0; i--){
            if (scrollHeaders[i].offsetTop < fromTop + 15 /*fudge factor*/){
                if (lastId != i) {
                    if (lastId != -1) {
                        scrollItems[lastId].parentElement.className = scrollItems[lastId].parentElement.className.replace(
                            " pure-menu-selected",
                            ""
                        );
                    }
                    scrollItems[i].parentElement.className = scrollItems[i].parentElement.className + " pure-menu-selected"
                    if (!isElementInViewport(scrollItems[i].parentElement)){
                        scrollItems[i].parentElement.scrollIntoView(lastId > i)
                    }

                    lastId = i
                }
                break;
            }
        }
    });
});
