$(document).ready(function () {
  var Client = ot.Client;
  var Server = ot.Server;
  var TextOperation = ot.TextOperation;
  var WrappedOperation = ot.WrappedOperation;
  var CodeMirrorAdapter = ot.CodeMirrorAdapter;


  // View

  var View = {
    appendTo: function (el) {
      this.el.appendTo(el);
      return this;
    },
    insertBefore: function (el) {
      this.el.insertBefore(el);
      return this;
    },
    $: function (sel) {
      return $(sel, this.el);
    }
  };


  // Operation

  function operationToHtml (operation) {
    var html = '';
    var ops = operation.ops;
    for (var i = 0; i < ops.length; i++) {
      if (i !== 0) { html += ", "; }
      var op = ops[i];
      if (TextOperation.isRetain(op)) {
        html += '<span class="op-retain">retain(' + op + ')</span>';
      } else if (TextOperation.isInsert(op)) {
        html += '<span class="op-insert">insert("' + op + '")</span>';
      } else {
        html += '<span class="op-delete">delete(' + (-op) + ')</span>';
      }
    }
    return html;
  }

  function operationPopoverContent (operation) {
    return function () {
      return [
        '<table class="table table-condensed table-noheader">',
        tr("Author", operation.meta.creator),
        typeof operation.revision === 'number' ? tr("Revision", operation.revision) : '',
        tr("Changeset", operationToHtml(operation.wrapped)),
        '</table>'
      ].join('\n');
    };
  }

  function createOperationElement (operation) {
    return $('<span class="operation" title="Operation" />')
      .addClass('operation' + operation.meta.id)
      .attr({ 'data-operation-id': operation.meta.id })
      .addClass(operation.meta.creator.toLowerCase());
  }


  // Visualization

  function Visualization (str) {
    this.str = str;
    this.el = $('<div id="visualization" />')
      .delegate('.operation', {
        mouseenter: function () {
          var operationId = $(this).attr('data-operation-id');
          $('.operation' + operationId).addClass('same-operation');
        },
        mouseleave: function () {
          $('.same-operation').removeClass('same-operation');
        }
      });

    var self = this;
    function serverReceive (o) {
      var oPrime = self.server.receiveOperation(o.revision, o);
      delete o.revision;
      self.server.appendToHistory(oPrime);
      self.aliceReceiveChannel.write(oPrime);
      self.bobReceiveChannel.write(oPrime);
    }
    function clientReceive (client) {
      return function (o) {
        if (o.meta.creator === client.name) {
          client.serverAck();
        } else {
          client.applyServer(o);
        }
      };
    }
    this.server = new MyServer(str, function (operation) {
    }).appendTo(this.el);
    this.aliceSendChannel = new NetworkChannel(true, serverReceive).appendTo(this.el);
    this.aliceSendChannel.el.attr({ id: 'alice-send-channel' });
    this.alice = new MyClient("Alice", str, 0, this.aliceSendChannel)
      .appendTo(this.el);
    this.alice.el.attr({ id: 'alice' });
    this.alice.svg.attr('id', 'alice-diamond-diagram');
    this.aliceReceiveChannel = new NetworkChannel(false, clientReceive(this.alice)).appendTo(this.el);
    this.aliceReceiveChannel.el.attr({ id: 'alice-receive-channel' });

    this.bobSendChannel = new NetworkChannel(true, serverReceive).appendTo(this.el);
    this.bobSendChannel.el.attr({ id: 'bob-send-channel' });
    this.bob = new MyClient("Bob", str, 0, this.bobSendChannel)
      .appendTo(this.el);
    this.bob.el.attr({ id: 'bob' });
    this.bob.svg.attr('id', 'bob-diamond-diagram');
    this.bobReceiveChannel = new NetworkChannel(false, clientReceive(this.bob)).appendTo(this.el);
    this.bobReceiveChannel.el.attr({ id: 'bob-receive-channel' });
  }

  extend(Visualization.prototype, View);

  Visualization.prototype.insertBefore = function (el) {
    View.insertBefore.call(this, el);
    this.alice.cm.refresh();
    this.bob.cm.refresh();
    return this;
  };


  // Diamond diagram

  var LEFT = 'left', RIGHT = 'right';

  // A crossing point on a diamond diagram is uniquely identified by the number
  // of steps left and right from a common starting point
  function DiamondPoint (leftEdges, rightEdges) {
    this.leftEdges = leftEdges;
    this.rightEdges = rightEdges;
  }

  DiamondPoint.prototype.equals = function (other) {
    return this.leftEdges == other.leftEdges &&
           this.rightEdges == other.rightEdges;
  };

  DiamondPoint.prototype.xPos = function () {
    return this.rightEdges - this.leftEdges;
  };

  DiamondPoint.prototype.yPos = function () {
    return this.rightEdges + this.leftEdges;
  };

  DiamondPoint.prototype.goLeft = function (data) {
    return new DiamondEdge(this, LEFT, data);
  };

  DiamondPoint.prototype.goRight = function (data) {
    return new DiamondEdge(this, RIGHT, data);
  };

  // An edge has a starting point and a direction. The end point is computed.
  function DiamondEdge (startPoint, direction, data) {
    this.startPoint = startPoint;
    this.direction = direction;

    this.length = 1;
    if (typeof data == 'object') { extend(this, data); }

    if (direction == LEFT) {
      this.endPoint = new DiamondPoint(
        startPoint.leftEdges + this.length,
        startPoint.rightEdges
      );
    } else {
      this.endPoint = new DiamondPoint(
        startPoint.leftEdges,
        startPoint.rightEdges + this.length
      );
    }
  }


  // Network channel

  function NetworkChannel (up, onReceive) {
    this.buffer = [];
    this.els = [];
    this.up = up;
    this.onReceive = onReceive;
    this.el = $('<div class="network-channel"><div /></div>')
      .addClass(up ? 'up-channel' : 'down-channel');
    var that = this;
    var arrow = up ? '&uarr;' : '&darr;';
    this.button = $('<a href="#" class="disabled">' + arrow + '</a>')
      .appendTo(this.el)
      .click(function (e) {
        e.preventDefault();
        if ($(this).hasClass('disabled')) { return; }
        that.receive();
      });
  }

  extend(NetworkChannel.prototype, View);

  NetworkChannel.prototype.write = function (val) {
    if (this.buffer.length === 0) {
      this.button.removeClass('disabled');
    }
    this.buffer.push(val);
    this.els.push(this.createElement(val));
  };

  NetworkChannel.prototype.createElement = function (operation) {
    var self = this;
    async(function () { self.distributeElements(); });
    return createOperationElement(operation)
      .popover({ content: operationPopoverContent(operation) })
      .css(this.up ? { top: '150px' } : { top: '-24px' })
      .appendTo(this.el);
  };

  NetworkChannel.prototype.read = function () {
    if (this.buffer.length === 1) {
      this.button.addClass('disabled');
    }
    var val = this.buffer.shift();
    this.removeElement(this.els.shift());
    return val;
  };

  NetworkChannel.prototype.removeElement = function (el) {
    el.css(this.up ? { top: '-24px' } : { top: '150px' });
    setTimeout(function () {
      el.remove();
    }, 500);
    this.distributeElements();
  };

  NetworkChannel.prototype.distributeElements = function () {
    var totalHeight = 150;
    var els = this.els;
    var partLength = 150 / (els.length+1);
    for (var i = 0; i < els.length; i++) {
      var el = els[i];
      var index = this.up ? i + 1 : els.length - i;
      el.css({ top: (Math.floor(index*partLength) - 12) + 'px' });
    }
  };

  NetworkChannel.prototype.receive = function () {
    this.onReceive.call(null, this.read());
  };


  // MyServer

  function MyServer (doc) {
    Server.call(this, doc);
    this.el = $('<div id="server" class="well" />');
    $('<h2 />').text("Server").appendTo(this.el);
    this.stateTable = $('<table class="table table-condensed table-noheader" />').html(
      tr("Content", quote(unescape(this.document)), 'server-content') +
      tr("History", "", 'server-history')
    ).appendTo(this.el);
  }

  inherit(MyServer, Server);
  extend(MyServer.prototype, View);

  MyServer.prototype.receiveOperation = function (revision, operation) {
    highlight(this.$('.server-history .operation').slice(operation.revision));
    var operationPrime = Server.prototype.receiveOperation.call(this, revision, operation);
    this.$('.server-content td').text(quote(unescape(this.document)));
    return operationPrime;
  };

  MyServer.prototype.appendToHistory = function (operation) {
    this.$('.server-history td')
      .append(document.createTextNode(" "))
      .append(
        createOperationElement(operation)
          .popover({ content: operationPopoverContent(operation) })
      );
  };


  // MyClient

  function MyClient (name, str, revision, channel) {
    Client.call(this, revision);
    this.name = name;
    this.channel = channel;
    this.fromServer = false;

    this.serverStatePoint = this.clientStatePoint = new DiamondPoint(0, 0);
    this.edges = [];

    this.oldValue = str;

    var self = this;
    this.el = $('<div class="well client" />')
      .popover({
        selector: '.operation',
        content: function () {
          if ($(this).hasClass('buffer')) {
            return operationPopoverContent(self.state.buffer)();
          } else {
            return operationPopoverContent(self.state.outstanding)();
          }
        }
      });
    $('<h2 />').text(name).appendTo(this.el);
    this.stateEl = $('<p class="state" />')
      .html("<strong>State:</strong> <span>Synchronized</span>")
      .appendTo(this.el);
    var cmWrapper = $('<div />').appendTo(document.body);
    this.cm = CodeMirror(cmWrapper.get(0), {
      lineNumbers: true,
      lineWrapping: true,
      value: str
    });
    this.cm.setOption('onChange', function (cm, change) {
      if (!self.fromServer) {
        var operation = new WrappedOperation(
          CodeMirrorAdapter.operationFromCodeMirrorChange(change, self.oldValue),
          { creator: self.name, id: generateOperationId() }
        );
        console.log(change, operation);
        self.applyClient(operation);
      }
      self.oldValue = self.cm.getValue();
    });
    cmWrapper.detach().appendTo(this.el);

    this.initD3();
  }

  inherit(MyClient, Client);
  extend(MyClient.prototype, View);

  MyClient.prototype.appendTo = function (el) {
    View.appendTo.call(this, el);
    $(this.svg[0]).appendTo(el);
    return this;
  };

  MyClient.prototype.initD3 = function () {
    var W = 460;
    var H = 320;
    var px = W/2;
    var py = 0;

    var svg = this.svg = d3.select('body').append('svg')
        .attr('class', 'diamond-diagram')
        .attr('width', W)
        .attr('height', H);

    var diagram = svg.append('g')
        .attr('transform', 'translate(0, 20)');

    var x = this.x = d3.scale.linear()
      .domain([0, 20])
      .range([0, W]);

    var y = this.y = d3.scale.linear()
      .domain([0, 16])
      .range([0, H]);

    var drag = d3.behavior.drag()
      .on('drag', function () {
        py = Math.min(0, py + d3.event.dy);
        px += d3.event.dx;

        rulesLayer.attr('transform', 'translate(0,'+py+')');
        edgesLayer.attr('transform', 'translate('+px+','+py+')');
      });

    svg.call(drag);

    // draw horizontal rules
    var rulesLayer = diagram.append('g');
    var rules = rulesLayer.selectAll('.rule')
        .data(d3.range(200))
      .enter().append('g')
        .attr('class', 'rule')
        .attr('transform', function (i) { return 'translate(0,'+y(i)+')'; });

    rules.append('line')
      .attr('stroke', '#ddd')
      .attr('y1', '0.5').attr('y2', '0.5')
      .attr('x1', '30').attr('x2', W);

    rules.append('text')
      .text(String)
      .attr('x', 15)
      .attr('dy', '0.35em')
      .attr('text-anchor', 'middle');

    var edgesLayer = this.edgesLayer = diagram.append('g')
        .attr('transform', 'translate('+px+','+py+')');
  };

  MyClient.prototype.sendOperation = function (revision, operation) {
    operation.revision = revision;
    this.channel.write(operation);
  };

  MyClient.prototype.applyOperation = function (operation) {
    this.fromServer = true;
    CodeMirrorAdapter.applyOperationToCodeMirror(operation.wrapped, this.cm);
    this.fromServer = false;
  };

  MyClient.prototype.drawEdges = function () {
    var x = this.x, y = this.y;
    var lines = this.edgesLayer.selectAll('.diamond-edge')
      .data(this.edges);

    lines.enter().append('line')
      .attr('class', 'diamond-edge')
      .attr('stroke', function (e) {
        return e.direction == LEFT ? '#1e488d' : '#d11';
      })
      .attr('stroke-width', '4px')
      .attr('stroke-linecap', 'round')
      .attr('stroke-dasharray', function (e) {
        return e.dashed ? '8, 10' : '1, 0';
      })
      .attr('x1', function (e) { return x(e.startPoint.xPos()); })
      .attr('y1', function (e) { return y(e.startPoint.yPos()); })
      .attr('x2', function (e) { return x(e.endPoint.xPos()); })
      .attr('y2', function (e) { return y(e.endPoint.yPos()); });
  };

  MyClient.prototype.addEdge = function (edge) {
    this.edges.push(edge);
    this.drawEdges();
  };

  MyClient.prototype.setAwaitingAndBufferEdge = function (awaitingEdge, bufferEdge) {
    if (this.awaitingEdge) {
      this.awaitingEdge.remove();
      delete this.awaitingEdge;
    }

    if (this.bufferEdge) {
      this.bufferEdge.remove();
      delete this.bufferEdge;
    }

    var x = this.x, y = this.y;
    var edgesLayer = this.edgesLayer;
    function drawEdge (edge, color) {
      return edgesLayer.append('line')
        .attr('stroke', color)
        .attr('stroke-width', '4px')
        .attr('stroke-linecap', 'round')
        .attr('x1', x(edge.startPoint.xPos()))
        .attr('y1', y(edge.startPoint.yPos()))
        .attr('x2', x(edge.endPoint.xPos()))
        .attr('y2', y(edge.endPoint.yPos()));
    }

    if (awaitingEdge) {
      this.awaitingEdge = drawEdge(awaitingEdge, '#ccc');
    }

    if (bufferEdge) {
      this.bufferEdge = drawEdge(bufferEdge, '#999');
    }
  };

  MyClient.prototype.goLeft = function (data) {
    data = data || {};
    if (this.clientStatePoint.equals(this.serverStatePoint)) {
      var edge = this.clientStatePoint.goLeft(data);
      this.clientStatePoint = this.serverStatePoint = edge.endPoint;
      this.addEdge(edge);
    } else {
      var serverEdge = this.serverStatePoint.goLeft(data);
      this.serverStatePoint = serverEdge.endPoint;
      this.addEdge(serverEdge);

      var clientEdge = this.clientStatePoint.goLeft(extend(data, { dashed: true }));
      this.clientStatePoint = clientEdge.endPoint;
      this.addEdge(clientEdge);
    }
  };

  MyClient.prototype.goRightClient = function (data) {
    data = data || {};
    var edge = this.clientStatePoint.goRight(data);
    this.clientStatePoint = edge.endPoint;
    this.addEdge(edge);
  };

  MyClient.prototype.goRightServer = function (data) {
    data = data || {};
    var edge = this.serverStatePoint.goRight(extend(data, { dashed: true }));
    this.serverStatePoint = edge.endPoint;
    this.addEdge(edge);
  };

  MyClient.prototype.setState = function (state) {
    var oldState = this.state;
    this.state = state;

    for (var i = 0; i < stateTransitions.length; i++) {
      var transition = stateTransitions[i];
      if (oldState instanceof transition[0] && state instanceof transition[1]) {
        transition[2].call(this);
        break;
      }
    }
  };

  MyClient.prototype.applyClient = function (operation) {
    this.state.beforeApplyClient(this);
    Client.prototype.applyClient.call(this, operation);
    this.drawAwaitingAndBufferEdges();
  };

  MyClient.prototype.applyServer = function (operation) {
    this.state.beforeApplyServer(this);
    Client.prototype.applyServer.call(this, operation);
    this.drawAwaitingAndBufferEdges();
  };

  MyClient.prototype.serverAck = function () {
    this.state.beforeServerAck(this);
    Client.prototype.serverAck.call(this);
    this.drawAwaitingAndBufferEdges();
  };

  MyClient.prototype.drawAwaitingAndBufferEdges = function () {
    this.state.drawAwaitingAndBufferEdges(this);
  };


  Client.Synchronized.prototype.beforeApplyClient = function (client) {
    client.goRightClient();
    client.awaitingLength = 1;
  };

  Client.Synchronized.prototype.beforeApplyServer = function (client) {
    client.goLeft();
  };

  Client.Synchronized.prototype.beforeServerAck = function () {};

  Client.Synchronized.prototype.drawAwaitingAndBufferEdges = function (client) {
    client.setAwaitingAndBufferEdge(null, null);
  };


  Client.AwaitingConfirm.prototype.beforeApplyClient = function (client) {
    client.goRightClient();
    client.bufferLength = 1;
  };

  Client.AwaitingConfirm.prototype.beforeApplyServer = function (client) {
    highlight($('.operation', client.stateEl));
    client.goLeft();
  };

  Client.AwaitingConfirm.prototype.beforeServerAck = function (client) {
    client.goRightServer({ length: client.awaitingLength });
    delete client.awaitingLength;
  };

  Client.AwaitingConfirm.prototype.drawAwaitingAndBufferEdges = function (client) {
    client.setAwaitingAndBufferEdge(
      client.serverStatePoint.goRight({ length: client.awaitingLength }),
      null
    );
  };


  Client.AwaitingWithBuffer.prototype.beforeApplyClient = function (client) {
    client.bufferLength++;
    client.goRightClient();
    highlight($('.operation', client.stateEl).eq(1));
  };

  Client.AwaitingWithBuffer.prototype.beforeApplyServer = Client.AwaitingConfirm.prototype.beforeApplyServer;

  Client.AwaitingWithBuffer.prototype.beforeServerAck = function (client) {
    client.goRightServer({ length: client.awaitingLength });
    client.awaitingLength = client.bufferLength;
    delete client.bufferLength;
  };

  Client.AwaitingWithBuffer.prototype.drawAwaitingAndBufferEdges = function (client) {
    var awaitingEdge = client.serverStatePoint.goRight({ length: client.awaitingLength });
    var bufferEdge = awaitingEdge.endPoint.goRight({ length: client.bufferLength });
    client.setAwaitingAndBufferEdge(awaitingEdge, bufferEdge);
  };


  var stateTransitions = [
    [
      Client.Synchronized,
      Client.AwaitingConfirm,
      function () {
        $('> span', this.stateEl)
          .text("Awaiting ")
          .append(createOperationElement(this.state.outstanding).addClass('outstanding'))
          .append(document.createTextNode(" "));
      }
    ],
    [
      Client.AwaitingConfirm,
      Client.AwaitingWithBuffer,
      function () {
        $('<span>with buffer </span>')
          .append(createOperationElement(this.state.buffer).addClass('buffer'))
          .fadeIn()
          .appendTo(this.stateEl);
      }
    ],
    [
      Client.AwaitingWithBuffer,
      Client.AwaitingConfirm,
      function () {
        var spans = $('> span', this.stateEl);
        hideSpan(spans.eq(0));
        spans.get(1).firstChild.data = "Awaiting ";
        spans.eq(1).append(document.createTextNode(" "));
        createOperationElement(this.state.outstanding)
          .addClass('outstanding')
          .replaceAll($('.operation', this.stateEl).eq(1));
      }
    ],
    [
      Client.AwaitingConfirm,
      Client.Synchronized,
      function () {
        $('> span', this.stateEl).text("Synchronized");
      }
    ]
  ];


  // Helper functions

  function inherit (Const, Super) {
    function F () {}
    F.prototype = Super.prototype;
    Const.prototype = new F();
    Const.prototype.constructor = Const;
  }

  function extend (target, source) {
    for (var name in source) {
      if (source.hasOwnProperty(name)) {
        target[name] = source[name];
      }
    }
    return target;
  }

  function async (fn) {
    setTimeout(fn, 0);
  }

  var generateOperationId = (function () {
    var counter = 1;
    return function () {
      return 'operation' + (counter++);
    };
  })();

  function tr (th, td, klass) {
    klass = klass ? ' class="'+klass+'"' : '';
    return '<tr' + klass + '><th>' + th + '</th><td>' + td + '</td></tr>';
  }

  function unescape (str) {
    return str.replace(/\n/g, '\\n').replace(/\t/g, '\\t');
  }

  function quote (str) {
    return '"' + str + '"';
  }

  function hideSpan (span) {
    span.animate({ width: 0 }, 500, function () {
      span.remove();
    });
  }

  function highlight (el) {
    // hacky!
    el.removeClass('animate').addClass('highlighted');
    async(function () {
      el.addClass('animate');
      async(function () {
        el.removeClass('highlighted');
      });
    });
  }


  // Initialize Visualization

  new Visualization("Lorem ipsum").insertBefore($('#wrapper footer'));
});