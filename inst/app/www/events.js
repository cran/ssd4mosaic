// Create input value when the result plot is updated for the first time
// Used as a signal to fire the bootstrap process
$(document).ready(function(){
  $('#p').on('shiny:value', function(event) {
    Shiny.setInputValue('first_plot_done', true);
  });
});
