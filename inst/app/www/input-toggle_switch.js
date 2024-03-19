var toggle_switch = new Shiny.InputBinding();
$.extend(toggle_switch, {
  find: function(scope) {
    return $(scope).find(".switchInput");
  },
  getValue: function(el) {
    return $(el).prop("checked");
  },
  setValue: function(el, value) {
    $(el).prop("checked", value).change();
  },
  receiveMessage: function(el, data) {
    this.setValue(el, value);
  },
  subscribe: function(el, callback) {
    $(el).on('change.switchInput', function() {
      callback(true);
    });

  },
  unsubscribe: function(el) {
    $(el).off(".switchInput");
  },

  getRatePolicy: function(){
    return {
      policy: 'throttle',
      delay: 1000
    }
  }
});
// Works without the line but not with the line
//Shiny.inputBindings.register(toggle_switch);
