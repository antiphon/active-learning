
(function() {
  var $ = jQuery;

  var audiosrcOutputBinding = new Shiny.OutputBinding();
  $.extend(audiosrcOutputBinding, {
    find: function(scope) {
      return $(scope).find('#audiosrc');
    },
    renderValue: function(el, data) {
      $(el).attr('src', data['src']);
      $(el).attr('type', data['type']);
    }
  });
  Shiny.outputBindings.register(audiosrcOutputBinding, 'shiny.audiosrcOutput');
})();
