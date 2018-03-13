
var _minekoa$project$Native_Mice = function() {

    function doFocus(_id) { 
        const element = document.getElementById(_id); 
        if (element == null) {
            return false;
        }

        element.focus();
        return true;
    }

    function calcTextWidth(_id, txt) {
        const element = document.getElementById(_id); 
        if (element == null) {
            return 0;
        }
        element.textContent = txt;
        const w = element.offsetWidth;
        element.textContent = null;

        return w
    }

    function getBoundingClientRect(_id) {
        const element = document.getElementById(_id); 
        if (element == null) {
            return {"left":0, "top":0, "right":0, "bottom":0, "x":0, "y":0, "width":0, "height":0};
        }
        const rect = element.getBoundingClientRect();
        return rect;
    }

    function elaborateInputAreaEventHandlers(id_input_area) {
        const input_area = document.getElementById(id_input_area);

        if (input_area == null) {
            return false;
        }
        console.log("add event listener");

        input_area.addEventListener( "keydown", e => {
            if (e.ctrlKey && (e.keyCode == 86 || e.keyCode == 67 || e.keyCode == 88)) { /* C-v : pasteイベントは生かしておきたい */
                ;
            }
            else if (e.altKey || e.ctrlKey) {
                e.preventDefault();
            }
            switch (e.keyCode) {
            case 37: /* '←' .. スクロールが発生してしまうことがある */
            case 38: /* '↑' */
            case 39: /* '→' */
            case 40: /* '↓' */ 
                e.preventDefault();
                break;
            }
        });

        input_area.addEventListener( "paste", e => {
            e.preventDefault();

            const data_transfer = (e.clipboardData) || (window.clipboardData);
            const str = data_transfer.getData("text/plain");
            console.log("paste");

            const evt = new CustomEvent("pasted", { "bubbles": true,
                                                    "cancelable": true,
                                                    "detail": str
                                                  }
                                       );
            input_area.dispatchEvent(evt);
        });

        input_area.addEventListener( "copy", e => {
            e.preventDefault();

            const str = input_area.selecteddata
            e.clipboardData.setData('text/plain', str);
            console.log("copy");

            const evt = new CustomEvent("copied", { "bubbles": true,
                                                    "cancelable": true,
                                                    "detail": str
                                                  }
                                       );
            input_area.dispatchEvent(evt);
        });

        input_area.addEventListener( "cut", e => {
            e.preventDefault();

            const str = input_area.selecteddata
            e.clipboardData.setData('text/plain', str);
            console.log("cut");

            const evt = new CustomEvent("cutted", { "bubbles": true,
                                                    "cancelable": true,
                                                    "detail": str
                                                  }
                                       );
            input_area.dispatchEvent(evt);
        });


        return true;
    }


    /* Scrolling */

    function getScrollTop (_id) {
        const element = document.getElementById(_id); 
        if (element == null) {
            return -1;
        }
        return element.scrollTop;
    }

    function setScrollTop (_id, pixels) {
        const element = document.getElementById(_id); 
        if (element == null) {
            return false;
        }
        element.scrollTop = pixels;
        return true
    }

    function getScrollLeft (_id) {
        const element = document.getElementById(_id); 
        if (element == null) {
            return -1;
        }
        return element.scrollLeft;
    }

    function setScrollLeft (_id, pixels) {
        const element = document.getElementById(_id); 
        if (element == null) {
            return false;
        }
        element.scrollLeft = pixels;
        return true
    }

    function getScrollHeight (_id) {
        const element = document.getElementById(_id); 
        if (element == null) {
            return 0;
        }
        return element.scrollHeight;
    }



  return {
      doFocus: doFocus,
      calcTextWidth: F2(calcTextWidth),
      getBoundingClientRect: getBoundingClientRect,
      elaborateInputAreaEventHandlers : elaborateInputAreaEventHandlers,
      getScrollTop: getScrollTop,
      setScrollTop: F2(setScrollTop),
      getScrollLeft: getScrollLeft,
      setScrollLeft: F2(setScrollLeft),
      getScrollHeight: getScrollHeight
  }
}();

