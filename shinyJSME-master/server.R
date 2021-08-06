server = function(input, output, session) {
    output$jsmeElement <- renderJsme(
        jsme()
    )
    observeEvent(input$smilesButton, {
      smiles(session, 'jsmeElement', 'inputSmiles')
      updateTextInput(session, 'smilesTextInput', value=input$inputSmiles)
    })
    observeEvent(input$inputSmiles, {
      updateTextInput(session, 'smilesTextInput', value=input$inputSmiles)
    })
    observeEvent(input$molFileButton, {
      molFile(session, 'jsmeElement', 'inputMolFile')
      updateTextInput(session, 'molFileTextInput', value=input$inputMolFile)
    })
    observeEvent(input$inputMolFile, {
      updateTextInput(session, 'molFileTextInput', value=input$inputMolFile)
    })
    observeEvent(input$jmeFileButton, {
      jmeFile(session, 'jsmeElement', 'inputJmeFile')
      updateTextInput(session, 'jmeFileTextInput', value=input$inputJmeFile)
    })
    observeEvent(input$inputJmeFile, {
      updateTextInput(session, 'jmeFileTextInput', value=input$inputJmeFile)
    })

    observeEvent(input$useJmeButton, useJME(session, 'jsmeElement', input$jmeFileTextInput))

    observeEvent(input$useMolButton, useMOL(session, 'jsmeElement', input$molFileTextInput))

    observeEvent(input$readSampleMoleculeButton, useJME(session, 'jsmeElement', '16 17 C 7.37 -8.99 C 7.37 -7.59 C 6.16 -6.89 C 4.95 -7.59 C 4.95 -8.99 C 6.16 -9.69 N 8.58 -6.89 C 8.58 -5.49 C 7.37 -4.79 O 6.16 -5.49 C 9.80 -7.59 O 9.80 -8.99 C 11.01 -6.89 Cl 12.22 -7.59 Cl 11.01 -5.49 C 9.80 -4.79 1 2 1 2 3 2 3 4 1 4 5 2 5 6 1 6 1 2 7 8 1 8 9 1 9 10 1 3 10 1 2 7 1 7 11 1 11 12 2 11 13 1 13 14 1 13 15 1 8 16 1'))

    observeEvent(input$readSampleMultipartStructureButton, useJME(session, 'jsmeElement', '9 9 C 6.68 -7.15 C 5.47 -6.45 C 4.26 -7.15 C 4.26 -8.55 C 5.47 -9.25 C 6.68 -8.55 C 5.47 -5.05 O- 6.68 -4.35 O 4.26 -4.35 1 2 1 2 3 2 3 4 1 4 5 2 5 6 1 6 1 2 2 7 1 7 8 1 7 9 2|1 0 Na+ 12.21 -6.61'))

    observeEvent(input$readSampleReactionButton, useJME(session, 'jsmeElement', '3 2 C:1 1.41 -7.12 O:2 1.41 -5.72 Cl 2.63 -7.82 1 2 2 1 3 1|3 2 N:3 5.72 -6.78 C:4 7.12 -6.78 H:5 5.02 -7.99 1 2 1 1 3 1 >> 5 4 C:1 13.51 -6.40 O:2 13.51 -5.00 N:3 14.72 -7.10 C:4 15.94 -6.40 H:5 14.71 -8.50 1 2 2 1 3 1 3 4 1 3 5 1'))

    observeEvent(input$clearEditorButton, {
      resetEditor(session, 'jsmeElement')
    })
    observeEvent(input$clearFieldsButton, {
        updateTextInput(session, 'smilesTextInput', value='')
        updateTextInput(session, 'molFileTextInput', value='')
        updateTextInput(session, 'jmeFileTextInput', value='')

    })

}
