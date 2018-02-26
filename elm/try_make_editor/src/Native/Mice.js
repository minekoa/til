
var _minekoa$project$Native_Mice = function() {

  function doFocus(_id) { 
	  var element = document.getElementById(_id); 
      if (element == null) {
          return false;
      }

	  element.focus();
      return true;
  } 

  return {
    doFocus: doFocus
  }
}();

