$ ->
  loadMathJax()
  $(document).on 'page:load', loadMathJax
  ws = new WebSocket $("body").data("ws-url")
  ws.onmessage = (event) ->
    message = JSON.parse event.data
    switch message.type
      when "addlines"
        addLines(message)
      else
        console.log(message)

  $("#addsymbolform").submit (event) ->
    event.preventDefault()
    # send the message to watch the stock
    ws.send(JSON.stringify({full_text: $("#addsymboltext").val()}))

firstUpdate = true
previewArea = null
updateButton = null
mathJax = null

loadMathJax = ->
  window.MathJax = null
  $.getScript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", ->
    MathJax.Hub.Config
      jax: ["input/TeX","output/HTML-CSS"]
      extensions: ["tex2jax.js"]
      Tex: 
        extensions: ["AMSmath.js","AMSsymbols.js"]
        equationNumbers:
          autoNumber: "AMS"
    mathJax = MathJax
    previewArea  = $('#preview')
    updateButton = $('#update')
    updateButton.click (event) ->
      update()

lineNum = 0
i = 0

addLines = (message) ->
  $("#stocks").empty()
  lineNum = 0
  addLine line for line in message.lines
  

addLine = (line) ->
  i = 0
  lineNum = lineNum + 1
  addSubLine subLine, line.length for subLine in line

addSubLine = (line, certainty) ->
  lineContainer = $("<div>").addClass("lineContainer").click (event) ->
    handleExpand($(this))
  lineContainer.attr('lineNum', lineNum)
  lineContainer.attr('certainty', certainty)
  if i == 0
    lineContainer.addClass("specialLine")
  else
    lineContainer.hide()
  i = i+1
  scannedLine   = $("<div>").addClass("line").html '\\begin{equation}\n' + 
      line + 
      '\\end{equation}\n'
  lineCertainty = $("<p>").text(certainty).addClass("lineCertainty")
  lineContainer.append(scannedLine).append(lineCertainty)
  $("#stocks").append(lineContainer)
  console.log("HI")
  mathJax.Hub.Queue(['Typeset', mathJax.Hub, scannedLine.get()])

handleExpand = (container) ->
  console.log(container.attr('certainty'))
  if container.hasClass("specialLine") and container.attr("certainty") != '1'
    container.removeClass("specialLine")
    ln = container.attr("lineNum")
    $(".specialLine").fadeOut('slow',
     -> $(".lineContainer[lineNum=" + ln + "]").fadeIn('slow')
    )
  else
    container.addClass("specialLine")
    ln = container.attr("lineNum")
    $(".lineContainer[lineNum=" + ln + "]").not(".specialLine").fadeOut('slow',
     -> $(".specialLine").fadeIn('slow')
    )
