var slideBinding = new Shiny.InputBinding();

$.extend(slideBinding, {
  find: function(scope) {
    return $(scope).find(".slider2");
  },
  getValue: function(el) {
  	//console.log("getValue");
    return parseInt(el.value);
  },
  setValue: function(el, value) {
  	//console.log("setValue");
  	el.value = value;
  },
  subscribe: function(el, callback) {
    $(el).on("change.slideBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".slideBinding");
  },
  receiveMessage: function(el, m){
    el.value = m.value;
  }
});

Shiny.inputBindings.register(slideBinding);
