Client = ot.Client
Server = ot.Server
TextOperation = ot.TextOperation
WrappedOperation = ot.WrappedOperation
CodeMirrorAdapter = ot.CodeMirrorAdapter


# View

View =
  appendTo: (el) ->
    @el.appendTo(el)
    @


# Operation

operationToHtml = (operation) ->
  html = ''
  for op, i in operation.ops
    html += ", " if i isnt 0
    html += if TextOperation.isRetain(op)
      '<span class="op-retain">retain(' + op + ')</span>'
    else if TextOperation.isInsert(op)
      '<span class="op-insert">insert("' + op + '")</span>'
    else
      '<span class="op-delete">delete(' + (-op) + ')</span>'
  html

operationPopoverContent = (operation) -> () ->
  [
    '<table class="table table-condensed table-noheader">'
    tr("Author", operation.meta.creator)
    if typeof operation.revision is 'number' then tr("Revision", operation.revision) else ''
    tr("Changeset", operationToHtml(operation.wrapped))
    '</table>'
  ].join('\n')

createOperationElement = (operation) ->
  $('<span class="operation" title="Operation" />')
    .addClass('operation' + operation.meta.id)
    .attr({ 'data-operation-id': operation.meta.id })
    .addClass(operation.meta.creator.toLowerCase())


# Visualization

class Visualization
  constructor: (@str) ->
    @el = $('<div id="visualization" />')
      .delegate '.operation',
        mouseenter: ->
          operationId = $(@).attr('data-operation-id')
          $('.operation' + operationId).addClass('same-operation')
        mouseleave: ->
          $('.same-operation').removeClass('same-operation')

    serverReceive = (o) =>
      oPrime = @server.receiveOperation(o.revision, o)
      delete o.revision
      @server.appendToHistory(oPrime)
      @aliceReceiveChannel.write(oPrime)
      @bobReceiveChannel.write(oPrime)

    clientReceive = (client) -> (o) ->
      if o.meta.creator is client.name
        client.serverAck()
      else
        client.applyServer(o)

    @server = new MyServer(@str).appendTo(@el)
    @aliceSendChannel = new NetworkChannel(true, serverReceive).appendTo(@el)
    @aliceSendChannel.el.attr({ id: 'alice-send-channel' })
    @alice = new MyClient("Alice", @str, 0, @aliceSendChannel)
      .appendTo(@el)
    @alice.el.attr({ id: 'alice' })
    @alice.svg.attr('id', 'alice-diamond-diagram')
    @aliceReceiveChannel = new NetworkChannel(false, clientReceive(@alice)).appendTo(@el)
    @aliceReceiveChannel.el.attr({ id: 'alice-receive-channel' })

    @bobSendChannel = new NetworkChannel(true, serverReceive).appendTo(@el)
    @bobSendChannel.el.attr({ id: 'bob-send-channel' })
    @bob = new MyClient("Bob", @str, 0, @bobSendChannel)
      .appendTo(@el)
    @bob.el.attr({ id: 'bob' })
    @bob.svg.attr('id', 'bob-diamond-diagram')
    @bobReceiveChannel = new NetworkChannel(false, clientReceive(@bob)).appendTo(@el)
    @bobReceiveChannel.el.attr({ id: 'bob-receive-channel' })

  insertBefore: (el) ->
    @el.insertBefore(el)
    @alice.cm.refresh()
    @bob.cm.refresh()
    @

window.Visualization = Visualization
_.extend(Visualization.prototype, View)


# Diamond diagram

LEFT  = 'left'
RIGHT = 'right'

# A crossing point on a diamond diagram is uniquely identified by the number
# of steps left and right from a common starting point
class DiamondPoint
  constructor: (@leftEdges, @rightEdges) ->

  equals: (o) -> @leftEdges is o.leftEdges and @rightEdges is o.rightEdges

  xPos: -> @rightEdges - @leftEdges
  yPos: -> @rightEdges + @leftEdges

  goLeft:  (data) -> new DiamondEdge(@, LEFT, data)
  goRight: (data) -> new DiamondEdge(@, RIGHT, data)

# An edge has a starting point and a direction. The end point is computed.
class DiamondEdge
  constructor: (@startPoint, @direction, data) ->
    @length = 1
    _.extend(@, data) if typeof data is 'object'
    @endPoint = if @direction is LEFT
      new DiamondPoint(@startPoint.leftEdges + @length, @startPoint.rightEdges)
    else
      new DiamondPoint(@startPoint.leftEdges, @startPoint.rightEdges + @length)


# Network channel

class NetworkChannel
  constructor: (@up, @onReceive) ->
    @buffer = []
    @els = []
    @el = $('<div class="network-channel"><div class="connection" /></div>')
      .addClass(if @up then 'up-channel' else 'down-channel')
    arrow = if @up then '&uarr;' else '&darr;'
    @button = $('<a href="#" class="disabled">' + arrow + '</a>')
      .appendTo(@el)
      .click (e) =>
        @receive() unless @button.hasClass('disabled')
        false

  write: (val) ->
    @button.removeClass('disabled') if @buffer.length is 0
    @buffer.push(val)
    @els.push(@createElement(val))

  read: ->
    @button.addClass('disabled') if @buffer.length is 1
    @removeElement(@els.shift())
    @buffer.shift()

  createElement: (operation) ->
    _.defer => @distributeElements()

    @el.css 'overflow', 'hidden'
    finished = => @el.css 'overflow', 'visible'
    setTimeout(finished, 500)

    createOperationElement(operation)
      .popover
        trigger: 'hover'
        html: true
        content: operationPopoverContent(operation)
      .css(if @up then { top: '150px' } else { top: '-24px' })
      .appendTo(@el)

  removeElement: (el) ->
    el.css(if @up then { top: '-24px' } else { top: '150px' })
    @el.css 'overflow', 'hidden'
    remove = =>
      el.remove()
      @el.css 'overflow', 'visible'
    setTimeout(remove, 500)
    @distributeElements()

  distributeElements: ->
    totalHeight = 150
    partLength = totalHeight / (@els.length+1)
    for el, i in @els
      index = if @up then i + 1 else @els.length - i
      el.css({ top: (Math.floor(index*partLength) - 12) + 'px' })

  receive: -> @onReceive.call(null, @read())

_.extend(NetworkChannel.prototype, View)


# MyServer

class MyServer extends Server
  constructor: (doc) ->
    super(doc)
    @el = $('<div id="server" class="well" />')
    $('<h2 />').text("Server").appendTo(@el)
    @stateTable = $('<table class="table table-condensed table-noheader" />').html(
      tr("Content", quote(unescape(@document)), 'server-content') +
      tr("History", "", 'server-history')
    ).appendTo(@el)

  receiveOperation: (revision, operation) ->
    highlight(@el.find('.server-history .operation').slice(operation.revision))
    operationPrime = super(revision, operation)
    @el.find('.server-content td').text(quote(unescape(@document)))
    operationPrime

  appendToHistory: (operation) ->
    @el.find('.server-history td')
      .append(document.createTextNode(" "))
      .append(
        createOperationElement(operation)
          .popover
            trigger: 'hover'
            html: true
            content: operationPopoverContent(operation)
      )

_.extend(MyServer.prototype, View)



# MyClient

class MyClient extends Client
  constructor: (@name, str, revision, @channel) ->
    super(revision)

    @fromServer = false
    @serverStatePoint = @clientStatePoint = new DiamondPoint(0, 0)
    @edges = []

    self = @
    @el = $('<div class="well client" />')
      .popover
        selector: '.operation',
        trigger: 'hover'
        html: true
        content: ->
          operationPopoverContent(
            if $(@).hasClass('buffer')
              self.state.buffer
            else
              self.state.outstanding
          )
    $('<h2 />').text(@name).appendTo(@el)
    @stateEl = $('<p class="state" />')
      .html("<strong>State:</strong> <span>Synchronized</span>")
      .appendTo(@el)
    cmWrapper = $('<div />').appendTo(document.body)
    @cm = CodeMirror cmWrapper.get(0),
      lineNumbers: true
      lineWrapping: true
      value: str
    @cm.on 'changes', (cm, changes) =>
      unless @fromServer
        operation = new WrappedOperation(
          CodeMirrorAdapter.operationFromCodeMirrorChanges(changes, @cm)[0],
          { creator: @name, id: _.uniqueId('operation') }
        )
        console.log(changes, operation)
        @applyClient(operation)
    cmWrapper.detach().appendTo(@el)

    @initD3()

  appendTo: (el) ->
    View.appendTo.call(@, el)
    $(@svg[0]).appendTo(el)
    @

  initD3: ->
    W = 460
    H = 320
    px = W/2
    py = 0

    svg = @svg = d3.select('body').append('svg')
        .attr('class', 'diamond-diagram')
        .attr('width', W)
        .attr('height', H)

    diagram = svg.append('g')
        .attr('transform', 'translate(0, 20)')

    x = @x = d3.scale.linear()
      .domain([0, 20])
      .range([0, W])

    y = @y = d3.scale.linear()
      .domain([0, 16])
      .range([0, H])

    drag = d3.behavior.drag()
      .on 'drag', ->
        py = Math.min(0, py + d3.event.dy)
        px += d3.event.dx

        rulesLayer.attr('transform', 'translate(0,'+py+')')
        edgesLayer.attr('transform', 'translate('+px+','+py+')')

    svg.call(drag)

    # draw horizontal rules
    rulesLayer = diagram.append('g')
    rules = rulesLayer.selectAll('.rule')
        .data(d3.range(200))
      .enter().append('g')
        .attr('class', 'rule')
        .attr('transform', (i) -> "translate(0, #{y(i)})")

    rules.append('line')
      .attr('stroke', '#ddd')
      .attr('y1', '0.5').attr('y2', '0.5')
      .attr('x1', '30').attr('x2', W)

    rules.append('text')
      .text(String)
      .attr('x', 15)
      .attr('dy', '0.35em')
      .attr('text-anchor', 'middle')

    edgesLayer = @edgesLayer = diagram.append('g')
        .attr('transform', 'translate('+px+','+py+')')

  sendOperation: (revision, operation) ->
    operation.revision = revision
    @channel.write(operation)

  applyOperation: (operation) ->
    @fromServer = true
    CodeMirrorAdapter.applyOperationToCodeMirror(operation.wrapped, @cm)
    @fromServer = false

  drawEdges: ->
    @edgesLayer.selectAll('.diamond-edge')
        .data(@edges)
      .enter().append('line')
        .attr('class', 'diamond-edge')
        .attr('stroke', (e) -> if e.direction is LEFT then '#1e488d' else '#d11')
        .attr('stroke-width', '4px')
        .attr('stroke-linecap', 'round')
        .attr('stroke-dasharray', (e) -> if e.dashed then '8, 10' else '1, 0')
        .attr('x1', (e) => @x(e.startPoint.xPos()))
        .attr('y1', (e) => @y(e.startPoint.yPos()))
        .attr('x2', (e) => @x(e.endPoint.xPos()))
        .attr('y2', (e) => @y(e.endPoint.yPos()))

  addEdge: (edge) ->
    @edges.push(edge)
    @drawEdges()

  setAwaitingAndBufferEdge: (awaitingEdge, bufferEdge) ->
    if @awaitingEdge
      @awaitingEdge.remove()
      delete @awaitingEdge

    if @bufferEdge
      @bufferEdge.remove()
      delete @bufferEdge

    drawEdge = (edge, color) =>
      @edgesLayer.append('line')
        .attr('stroke', color)
        .attr('stroke-width', '4px')
        .attr('stroke-linecap', 'round')
        .attr('x1', @x(edge.startPoint.xPos()))
        .attr('y1', @y(edge.startPoint.yPos()))
        .attr('x2', @x(edge.endPoint.xPos()))
        .attr('y2', @y(edge.endPoint.yPos()))

    @awaitingEdge = drawEdge(awaitingEdge, '#ccc') if awaitingEdge
    @bufferEdge   = drawEdge(bufferEdge,   '#999') if bufferEdge

  goLeft: (data) ->
    data ||= {}
    if @clientStatePoint.equals(@serverStatePoint)
      edge = @clientStatePoint.goLeft(data)
      @clientStatePoint = @serverStatePoint = edge.endPoint
      @addEdge(edge)
    else
      serverEdge = @serverStatePoint.goLeft(data)
      @serverStatePoint = serverEdge.endPoint
      @addEdge(serverEdge)

      clientEdge = @clientStatePoint.goLeft(_.extend(data, { dashed: true }))
      @clientStatePoint = clientEdge.endPoint
      @addEdge(clientEdge)

  goRightClient: (data) ->
    edge = @clientStatePoint.goRight(data or {})
    @clientStatePoint = edge.endPoint
    @addEdge(edge)

  goRightServer: (data) ->
    edge = @serverStatePoint.goRight(_.extend(data or {}, { dashed: true }))
    @serverStatePoint = edge.endPoint
    @addEdge(edge)

  setState: (state) ->
    oldState = @state
    @state = state

    for transition in stateTransitions
      if oldState instanceof transition[0] and state instanceof transition[1]
        transition[2].call(@)
        break

  applyClient: (operation) ->
    @state.beforeApplyClient(@)
    super(operation)
    @drawAwaitingAndBufferEdges()

  applyServer: (operation) ->
    @state.beforeApplyServer(@)
    super(operation)
    @drawAwaitingAndBufferEdges()

  serverAck: ->
    @state.beforeServerAck(@)
    super()
    @drawAwaitingAndBufferEdges()

  drawAwaitingAndBufferEdges: ->
    @state.drawAwaitingAndBufferEdges(@)


# Client monkey-patching

Client.Synchronized::beforeApplyClient = (client) ->
  client.goRightClient()
  client.awaitingLength = 1

Client.Synchronized::beforeApplyServer = (client) ->
  client.goLeft()

Client.Synchronized::beforeServerAck = ->

Client.Synchronized::drawAwaitingAndBufferEdges = (client) ->
  client.setAwaitingAndBufferEdge(null, null)


Client.AwaitingConfirm::beforeApplyClient = (client) ->
  client.goRightClient()
  client.bufferLength = 1

Client.AwaitingConfirm::beforeApplyServer = (client) ->
  highlight($('.operation', client.stateEl))
  client.goLeft()

Client.AwaitingConfirm::beforeServerAck = (client) ->
  client.goRightServer({ length: client.awaitingLength })
  delete client.awaitingLength

Client.AwaitingConfirm::drawAwaitingAndBufferEdges = (client) ->
  client.setAwaitingAndBufferEdge(
    client.serverStatePoint.goRight({ length: client.awaitingLength }),
    null
  )


Client.AwaitingWithBuffer::beforeApplyClient = (client) ->
  client.bufferLength++
  client.goRightClient()
  highlight($('.operation', client.stateEl).eq(1))

Client.AwaitingWithBuffer::beforeApplyServer = Client.AwaitingConfirm::beforeApplyServer

Client.AwaitingWithBuffer::beforeServerAck = (client) ->
  client.goRightServer({ length: client.awaitingLength })
  client.awaitingLength = client.bufferLength
  delete client.bufferLength

Client.AwaitingWithBuffer::drawAwaitingAndBufferEdges = (client) ->
  awaitingEdge = client.serverStatePoint.goRight({ length: client.awaitingLength })
  bufferEdge = awaitingEdge.endPoint.goRight({ length: client.bufferLength })
  client.setAwaitingAndBufferEdge(awaitingEdge, bufferEdge)


stateTransitions = [
  [
    Client.Synchronized
    Client.AwaitingConfirm
    ->
      $('> span', @stateEl)
        .text("Awaiting ")
        .append(createOperationElement(@state.outstanding).addClass('outstanding'))
        .append(document.createTextNode(" "))
  ]
  [
    Client.AwaitingConfirm
    Client.AwaitingWithBuffer
    ->
      $('<span>with buffer </span>')
        .append(createOperationElement(@state.buffer).addClass('buffer'))
        .fadeIn()
        .appendTo(@stateEl)
  ]
  [
    Client.AwaitingWithBuffer
    Client.AwaitingConfirm
    ->
      spans = $('> span', @stateEl)
      hideSpan(spans.eq(0))
      spans.get(1).firstChild.data = "Awaiting "
      spans.eq(1).append(document.createTextNode(" "))
      createOperationElement(@state.outstanding)
        .addClass('outstanding')
        .replaceAll($('.operation', @stateEl).eq(1))
  ]
  [
    Client.AwaitingConfirm
    Client.Synchronized
    ->
      $('> span', @stateEl).text("Synchronized")
  ]
]


# Helper functions

tr = (th, td, klass) ->
  klass = if klass then ' class="' + klass + '"' else ''
  "<tr#{klass}><th>#{th}</th><td>#{td}</td></tr>"

unescape = (str) -> str.replace(/\n/g, '\\n').replace(/\t/g, '\\t')
quote = (str) -> '"' + str + '"'

hideSpan = (span) -> span.animate({ width: 0 }, 500, -> span.remove())

highlight = (el) ->
  # hacky!
  el.removeClass('animate').addClass('highlighted')
  _.defer ->
    el.addClass('animate')
    _.defer -> el.removeClass('highlighted')
