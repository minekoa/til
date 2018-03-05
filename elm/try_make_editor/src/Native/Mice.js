
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

    function elaborateInputAreaEventHandlers(id_input_area, id_paste_area) {
        const input_area = document.getElementById(id_input_area);
        const paste_area = document.getElementById(id_paste_area);

        console.log("elabo");

        if ((input_area == null) || (paste_area == null)) {
            console.log("(f)");
            return false;
        }
        
	    input_area.addEventListener( "keydown", e => {
            if (e.ctrlKey && (e.keyCode == 86)) { /* C-v : pasteイベントは生かしておきたい */
                ;
            }
            else if (e.altKey || e.ctrlKey) {
                e.preventDefault();
            }
        });

        input_area.addEventListener( "paste", e => {
            e.preventDefault();

            const data_transfer = (e.clipboardData) || (window.clipboardData);
            const str = data_transfer.getData("text/plain");

            paste_area.value = str;
            const evt = new Event("input", {"bubbles": true, "cancelable": true});
            paste_area.dispatchEvent(evt);

            console.log(str);
        });

        console.log("(t)");
        return true;
    }

        
  function copyToClipboard (_id, txt) {
	  const element = document.getElementById(_id); /*textarea であることを期待している */
      const range = document.createRange();

      element.value = txt;
      element.select();
      return document.execCommand('copy');
  };

  return {
      doFocus: doFocus,
      calcTextWidth: F2(calcTextWidth),
      getBoundingClientRect: getBoundingClientRect,
      elaborateInputAreaEventHandlers : F2(elaborateInputAreaEventHandlers),
      copyToClipboard: F2(copyToClipboard)
  }
}();

