(function() {
  var readMolecule = function(el, structure) {
    $('#'+el).data('jsme').readMolecule(structure);
  };

  var readMolFile = function(el, structure) {
    $('#'+el).data('jsme').readMolFile(structure);
  };

  var reset = function(el) {
    $('#'+el).data('jsme').reset();
  };

  var smiles = function(el, inputEl) {
    Shiny.onInputChange(inputEl, $('#'+el).data('jsme').smiles());
  };

  var molFile = function(el, inputEl) {
    Shiny.onInputChange(inputEl, $('#'+el).data('jsme').molFile());
  };

  var jmeFile = function(el, inputEl) {
    Shiny.onInputChange(inputEl, $('#'+el).data('jsme').jmeFile());
  };

  Shiny.addCustomMessageHandler('useJME',
    function(message) {
      readMolecule(message.el, message.jmeFile);
    }
  );

  Shiny.addCustomMessageHandler('useMOL',
    function(message) {
      readMolFile(message.el, message.molFile);
    }
  );

  Shiny.addCustomMessageHandler('resetEditor',
    function(message) {
      reset(message.el);
    }
  );

  Shiny.addCustomMessageHandler('smiles',
    function(message) {
      smiles(message.el, message.inputEl);
    }
  );

  Shiny.addCustomMessageHandler('molFile',
    function(message) {
      molFile(message.el, message.inputEl);
    }
  );

  Shiny.addCustomMessageHandler('jmeFile',
    function(message) {
      jmeFile(message.el, message.inputEl);
    }
  );
})();



HTMLWidgets.widget({

  name: 'jsme',

  type: 'output',

  factory: function(el, width, height) {



    if (typeof window.jsmeArray === 'undefined') window.jsmeArray = [];
    window.jsmeArray.push({el: el.id, width: width, height: height});

    jsmeOnLoad = function() {
      if (typeof window.jsmeArray !== 'undefined') {
        window.jsmeArray.forEach(
          function(v) {
            if (typeof $('#'+v.el).data('jsme') === 'undefined') {
              $('#'+v.el).data('jsme', new JSApplet.JSME(v.el, '100%', '100%'));
              $('#'+v.el).data('jsme').setSize(v.width, v.height);
            }
          }
        );
      }
	  };


    return {

      renderValue: function() {

        if ((typeof $('#'+el.id).data('jsme') === 'undefined') && (typeof JSApplet !== 'undefined')) {
          $('#'+el.id).data('jsme', new JSApplet.JSME(el.id, '100%', '100%')); // jsmeOnLoad();
        }

      },

      resize: function(width, height) {

        $('#'+el.id).data('jsme').setSize(width, height);

      }

    };

  }

});
