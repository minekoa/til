
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
      var rect = element.getBoundingClientRect();
      return rect;
  }

  return {
      doFocus: doFocus,
      calcTextWidth: F2(calcTextWidth),
      getBoundingClientRect: getBoundingClientRect
  }
}();

