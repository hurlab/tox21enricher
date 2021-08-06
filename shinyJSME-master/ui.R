library(shiny)
library(jsme)


ui = shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(


        actionButton('smilesButton', 'Smiles'),
        actionButton('molFileButton', 'MOL File'),
        actionButton('jmeFileButton', 'JME File'),
        textInput('smilesTextInput', 'Smiles', width='100%'),
        tags$textarea(id='molFileTextInput', rows=6, '', style='width: 100%'),
        textInput('jmeFileTextInput', 'JME string', width='100%'),
        actionButton('readSampleMoleculeButton', 'Read Molecule'),
        actionButton('readSampleMultipartStructureButton', 'Read Multipart Structure'),
        actionButton('readSampleReactionButton', 'Read Reaction'),
        actionButton('useJmeButton', 'Use JME'),
        actionButton('useMolButton', 'Use MOL file'),
        actionButton('clearEditorButton', 'Clear Editor'),
        actionButton('clearFieldsButton', 'Clear Fields')
      ),

      mainPanel(
        jsmeOutput('jsmeElement', '650px', '650px')
      )
    )
  )
)
