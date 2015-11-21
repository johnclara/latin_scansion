$ ->
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
  scannedLine   = $("<p>").text(line).addClass("line")
  lineCertainty = $("<p>").text(certainty).addClass("lineCertainty")
  lineContainer.append(scannedLine).append(lineCertainty)
  $("#stocks").append(lineContainer)

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
