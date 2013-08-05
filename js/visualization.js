(function() {
  var Client, CodeMirrorAdapter, DiamondEdge, DiamondPoint, LEFT, MyClient, MyServer, NetworkChannel, RIGHT, Server, TextOperation, View, Visualization, WrappedOperation, createOperationElement, hideSpan, highlight, operationPopoverContent, operationToHtml, quote, stateTransitions, tr, unescape,
    __hasProp = {}.hasOwnProperty,
    __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

  Client = ot.Client;

  Server = ot.Server;

  TextOperation = ot.TextOperation;

  WrappedOperation = ot.WrappedOperation;

  CodeMirrorAdapter = ot.CodeMirrorAdapter;

  View = {
    appendTo: function(el) {
      this.el.appendTo(el);
      return this;
    }
  };

  operationToHtml = function(operation) {
    var html, i, op, _i, _len, _ref;
    html = '';
    _ref = operation.ops;
    for (i = _i = 0, _len = _ref.length; _i < _len; i = ++_i) {
      op = _ref[i];
      if (i !== 0) {
        html += ", ";
      }
      html += TextOperation.isRetain(op) ? '<span class="op-retain">retain(' + op + ')</span>' : TextOperation.isInsert(op) ? '<span class="op-insert">insert("' + op + '")</span>' : '<span class="op-delete">delete(' + (-op) + '</span>';
    }
    return html;
  };

  operationPopoverContent = function(operation) {
    return function() {
      return ['<table class="table table-condensed table-noheader">', tr("Author", operation.meta.creator), typeof operation.revision === 'number' ? tr("Revision", operation.revision) : '', tr("Changeset", operationToHtml(operation.wrapped)), '</table>'].join('\n');
    };
  };

  createOperationElement = function(operation) {
    return $('<span class="operation" title="Operation" />').addClass('operation' + operation.meta.id).attr({
      'data-operation-id': operation.meta.id
    }).addClass(operation.meta.creator.toLowerCase());
  };

  Visualization = (function() {

    function Visualization(str) {
      var clientReceive, serverReceive,
        _this = this;
      this.str = str;
      this.el = $('<div id="visualization" />').delegate('.operation', {
        mouseenter: function() {
          var operationId;
          operationId = $(this).attr('data-operation-id');
          return $('.operation' + operationId).addClass('same-operation');
        },
        mouseleave: function() {
          return $('.same-operation').removeClass('same-operation');
        }
      });
      serverReceive = function(o) {
        var oPrime;
        oPrime = _this.server.receiveOperation(o.revision, o);
        delete o.revision;
        _this.server.appendToHistory(oPrime);
        _this.aliceReceiveChannel.write(oPrime);
        return _this.bobReceiveChannel.write(oPrime);
      };
      clientReceive = function(client) {
        return function(o) {
          if (o.meta.creator === client.name) {
            return client.serverAck();
          } else {
            return client.applyServer(o);
          }
        };
      };
      this.server = new MyServer(str).appendTo(this.el);
      this.aliceSendChannel = new NetworkChannel(true, serverReceive).appendTo(this.el);
      this.aliceSendChannel.el.attr({
        id: 'alice-send-channel'
      });
      this.alice = new MyClient("Alice", str, 0, this.aliceSendChannel).appendTo(this.el);
      this.alice.el.attr({
        id: 'alice'
      });
      this.alice.svg.attr('id', 'alice-diamond-diagram');
      this.aliceReceiveChannel = new NetworkChannel(false, clientReceive(this.alice)).appendTo(this.el);
      this.aliceReceiveChannel.el.attr({
        id: 'alice-receive-channel'
      });
      this.bobSendChannel = new NetworkChannel(true, serverReceive).appendTo(this.el);
      this.bobSendChannel.el.attr({
        id: 'bob-send-channel'
      });
      this.bob = new MyClient("Bob", str, 0, this.bobSendChannel).appendTo(this.el);
      this.bob.el.attr({
        id: 'bob'
      });
      this.bob.svg.attr('id', 'bob-diamond-diagram');
      this.bobReceiveChannel = new NetworkChannel(false, clientReceive(this.bob)).appendTo(this.el);
      this.bobReceiveChannel.el.attr({
        id: 'bob-receive-channel'
      });
    }

    Visualization.prototype.insertBefore = function(el) {
      this.el.insertBefore(el);
      this.alice.cm.refresh();
      this.bob.cm.refresh();
      return this;
    };

    return Visualization;

  })();

  window.Visualization = Visualization;

  _.extend(Visualization.prototype, View);

  LEFT = 'left';

  RIGHT = 'right';

  DiamondPoint = (function() {

    function DiamondPoint(leftEdges, rightEdges) {
      this.leftEdges = leftEdges;
      this.rightEdges = rightEdges;
    }

    DiamondPoint.prototype.equals = function(o) {
      return this.leftEdges === o.leftEdges && this.rightEdges === o.rightEdges;
    };

    DiamondPoint.prototype.xPos = function() {
      return this.rightEdges - this.leftEdges;
    };

    DiamondPoint.prototype.yPos = function() {
      return this.rightEdges + this.leftEdges;
    };

    DiamondPoint.prototype.goLeft = function(data) {
      return new DiamondEdge(this, LEFT, data);
    };

    DiamondPoint.prototype.goRight = function(data) {
      return new DiamondEdge(this, RIGHT, data);
    };

    return DiamondPoint;

  })();

  DiamondEdge = (function() {

    function DiamondEdge(startPoint, direction, data) {
      this.startPoint = startPoint;
      this.direction = direction;
      this.length = 1;
      if (typeof data === 'object') {
        _.extend(this, data);
      }
      this.endPoint = this.direction === LEFT ? new DiamondPoint(this.startPoint.leftEdges + this.length, this.startPoint.rightEdges) : new DiamondPoint(this.startPoint.leftEdges, this.startPoint.rightEdges + this.length);
    }

    return DiamondEdge;

  })();

  NetworkChannel = (function() {

    function NetworkChannel(up, onReceive) {
      var arrow,
        _this = this;
      this.up = up;
      this.onReceive = onReceive;
      this.buffer = [];
      this.els = [];
      this.el = $('<div class="network-channel"><div class="connection" /></div>').addClass(this.up ? 'up-channel' : 'down-channel');
      arrow = this.up ? '&uarr;' : '&darr;';
      this.button = $('<a href="#" class="disabled">' + arrow + '</a>').appendTo(this.el).click(function(e) {
        if (!_this.button.hasClass('disabled')) {
          _this.receive();
        }
        return false;
      });
    }

    NetworkChannel.prototype.write = function(val) {
      if (this.buffer.length === 0) {
        this.button.removeClass('disabled');
      }
      this.buffer.push(val);
      return this.els.push(this.createElement(val));
    };

    NetworkChannel.prototype.read = function() {
      if (this.buffer.length === 1) {
        this.button.addClass('disabled');
      }
      this.removeElement(this.els.shift());
      return this.buffer.shift();
    };

    NetworkChannel.prototype.createElement = function(operation) {
      var finished,
        _this = this;
      _.defer(function() {
        return _this.distributeElements();
      });
      this.el.css('overflow', 'hidden');
      finished = function() {
        return _this.el.css('overflow', 'visible');
      };
      setTimeout(finished, 500);
      return createOperationElement(operation).popover({
        trigger: 'hover',
        html: true,
        content: operationPopoverContent(operation)
      }).css(this.up ? {
        top: '150px'
      } : {
        top: '-24px'
      }).appendTo(this.el);
    };

    NetworkChannel.prototype.removeElement = function(el) {
      var remove,
        _this = this;
      el.css(this.up ? {
        top: '-24px'
      } : {
        top: '150px'
      });
      this.el.css('overflow', 'hidden');
      remove = function() {
        el.remove();
        return _this.el.css('overflow', 'visible');
      };
      setTimeout(remove, 500);
      return this.distributeElements();
    };

    NetworkChannel.prototype.distributeElements = function() {
      var el, i, index, partLength, totalHeight, _i, _len, _ref, _results;
      totalHeight = 150;
      partLength = totalHeight / (this.els.length + 1);
      _ref = this.els;
      _results = [];
      for (i = _i = 0, _len = _ref.length; _i < _len; i = ++_i) {
        el = _ref[i];
        index = this.up ? i + 1 : this.els.length - i;
        _results.push(el.css({
          top: (Math.floor(index * partLength) - 12) + 'px'
        }));
      }
      return _results;
    };

    NetworkChannel.prototype.receive = function() {
      return this.onReceive.call(null, this.read());
    };

    return NetworkChannel;

  })();

  _.extend(NetworkChannel.prototype, View);

  MyServer = (function(_super) {

    __extends(MyServer, _super);

    function MyServer(doc) {
      MyServer.__super__.constructor.call(this, doc);
      this.el = $('<div id="server" class="well" />');
      $('<h2 />').text("Server").appendTo(this.el);
      this.stateTable = $('<table class="table table-condensed table-noheader" />').html(tr("Content", quote(unescape(this.document)), 'server-content') + tr("History", "", 'server-history')).appendTo(this.el);
    }

    MyServer.prototype.receiveOperation = function(revision, operation) {
      var operationPrime;
      highlight(this.el.find('.server-history .operation').slice(operation.revision));
      operationPrime = MyServer.__super__.receiveOperation.call(this, revision, operation);
      this.el.find('.server-content td').text(quote(unescape(this.document)));
      return operationPrime;
    };

    MyServer.prototype.appendToHistory = function(operation) {
      return this.el.find('.server-history td').append(document.createTextNode(" ")).append(createOperationElement(operation).popover({
        trigger: 'hover',
        html: true,
        content: operationPopoverContent(operation)
      }));
    };

    return MyServer;

  })(Server);

  _.extend(MyServer.prototype, View);

  MyClient = (function(_super) {

    __extends(MyClient, _super);

    function MyClient(name, str, revision, channel) {
      var cmWrapper, self,
        _this = this;
      this.name = name;
      this.channel = channel;
      MyClient.__super__.constructor.call(this, revision);
      this.fromServer = false;
      this.serverStatePoint = this.clientStatePoint = new DiamondPoint(0, 0);
      this.edges = [];
      self = this;
      this.el = $('<div class="well client" />').popover({
        selector: '.operation',
        trigger: 'hover',
        html: true,
        content: function() {
          return operationPopoverContent($(this).hasClass('buffer') ? self.state.buffer : self.state.outstanding);
        }
      });
      $('<h2 />').text(this.name).appendTo(this.el);
      this.stateEl = $('<p class="state" />').html("<strong>State:</strong> <span>Synchronized</span>").appendTo(this.el);
      cmWrapper = $('<div />').appendTo(document.body);
      this.cm = CodeMirror(cmWrapper.get(0), {
        lineNumbers: true,
        lineWrapping: true,
        value: str
      });
      this.cm.on('change', function(cm, change) {
        var operation;
        if (!_this.fromServer) {
          operation = new WrappedOperation(CodeMirrorAdapter.operationFromCodeMirrorChange(change, _this.cm)[0], {
            creator: _this.name,
            id: _.uniqueId('operation')
          });
          console.log(change, operation);
          return _this.applyClient(operation);
        }
      });
      cmWrapper.detach().appendTo(this.el);
      this.initD3();
    }

    MyClient.prototype.appendTo = function(el) {
      View.appendTo.call(this, el);
      $(this.svg[0]).appendTo(el);
      return this;
    };

    MyClient.prototype.initD3 = function() {
      var H, W, diagram, drag, edgesLayer, px, py, rules, rulesLayer, svg, x, y;
      W = 460;
      H = 320;
      px = W / 2;
      py = 0;
      svg = this.svg = d3.select('body').append('svg').attr('class', 'diamond-diagram').attr('width', W).attr('height', H);
      diagram = svg.append('g').attr('transform', 'translate(0, 20)');
      x = this.x = d3.scale.linear().domain([0, 20]).range([0, W]);
      y = this.y = d3.scale.linear().domain([0, 16]).range([0, H]);
      drag = d3.behavior.drag().on('drag', function() {
        py = Math.min(0, py + d3.event.dy);
        px += d3.event.dx;
        rulesLayer.attr('transform', 'translate(0,' + py + ')');
        return edgesLayer.attr('transform', 'translate(' + px + ',' + py + ')');
      });
      svg.call(drag);
      rulesLayer = diagram.append('g');
      rules = rulesLayer.selectAll('.rule').data(d3.range(200)).enter().append('g').attr('class', 'rule').attr('transform', function(i) {
        return "translate(0, " + (y(i)) + ")";
      });
      rules.append('line').attr('stroke', '#ddd').attr('y1', '0.5').attr('y2', '0.5').attr('x1', '30').attr('x2', W);
      rules.append('text').text(String).attr('x', 15).attr('dy', '0.35em').attr('text-anchor', 'middle');
      return edgesLayer = this.edgesLayer = diagram.append('g').attr('transform', 'translate(' + px + ',' + py + ')');
    };

    MyClient.prototype.sendOperation = function(revision, operation) {
      operation.revision = revision;
      return this.channel.write(operation);
    };

    MyClient.prototype.applyOperation = function(operation) {
      this.fromServer = true;
      CodeMirrorAdapter.applyOperationToCodeMirror(operation.wrapped, this.cm);
      return this.fromServer = false;
    };

    MyClient.prototype.drawEdges = function() {
      var _this = this;
      return this.edgesLayer.selectAll('.diamond-edge').data(this.edges).enter().append('line').attr('class', 'diamond-edge').attr('stroke', function(e) {
        if (e.direction === LEFT) {
          return '#1e488d';
        } else {
          return '#d11';
        }
      }).attr('stroke-width', '4px').attr('stroke-linecap', 'round').attr('stroke-dasharray', function(e) {
        if (e.dashed) {
          return '8, 10';
        } else {
          return '1, 0';
        }
      }).attr('x1', function(e) {
        return _this.x(e.startPoint.xPos());
      }).attr('y1', function(e) {
        return _this.y(e.startPoint.yPos());
      }).attr('x2', function(e) {
        return _this.x(e.endPoint.xPos());
      }).attr('y2', function(e) {
        return _this.y(e.endPoint.yPos());
      });
    };

    MyClient.prototype.addEdge = function(edge) {
      this.edges.push(edge);
      return this.drawEdges();
    };

    MyClient.prototype.setAwaitingAndBufferEdge = function(awaitingEdge, bufferEdge) {
      var drawEdge,
        _this = this;
      if (this.awaitingEdge) {
        this.awaitingEdge.remove();
        delete this.awaitingEdge;
      }
      if (this.bufferEdge) {
        this.bufferEdge.remove();
        delete this.bufferEdge;
      }
      drawEdge = function(edge, color) {
        return _this.edgesLayer.append('line').attr('stroke', color).attr('stroke-width', '4px').attr('stroke-linecap', 'round').attr('x1', _this.x(edge.startPoint.xPos())).attr('y1', _this.y(edge.startPoint.yPos())).attr('x2', _this.x(edge.endPoint.xPos())).attr('y2', _this.y(edge.endPoint.yPos()));
      };
      if (awaitingEdge) {
        this.awaitingEdge = drawEdge(awaitingEdge, '#ccc');
      }
      if (bufferEdge) {
        return this.bufferEdge = drawEdge(bufferEdge, '#999');
      }
    };

    MyClient.prototype.goLeft = function(data) {
      var clientEdge, edge, serverEdge;
      data || (data = {});
      if (this.clientStatePoint.equals(this.serverStatePoint)) {
        edge = this.clientStatePoint.goLeft(data);
        this.clientStatePoint = this.serverStatePoint = edge.endPoint;
        return this.addEdge(edge);
      } else {
        serverEdge = this.serverStatePoint.goLeft(data);
        this.serverStatePoint = serverEdge.endPoint;
        this.addEdge(serverEdge);
        clientEdge = this.clientStatePoint.goLeft(_.extend(data, {
          dashed: true
        }));
        this.clientStatePoint = clientEdge.endPoint;
        return this.addEdge(clientEdge);
      }
    };

    MyClient.prototype.goRightClient = function(data) {
      var edge;
      edge = this.clientStatePoint.goRight(data || {});
      this.clientStatePoint = edge.endPoint;
      return this.addEdge(edge);
    };

    MyClient.prototype.goRightServer = function(data) {
      var edge;
      edge = this.serverStatePoint.goRight(_.extend(data || {}, {
        dashed: true
      }));
      this.serverStatePoint = edge.endPoint;
      return this.addEdge(edge);
    };

    MyClient.prototype.setState = function(state) {
      var oldState, transition, _i, _len, _results;
      oldState = this.state;
      this.state = state;
      _results = [];
      for (_i = 0, _len = stateTransitions.length; _i < _len; _i++) {
        transition = stateTransitions[_i];
        if (oldState instanceof transition[0] && state instanceof transition[1]) {
          transition[2].call(this);
          break;
        } else {
          _results.push(void 0);
        }
      }
      return _results;
    };

    MyClient.prototype.applyClient = function(operation) {
      this.state.beforeApplyClient(this);
      MyClient.__super__.applyClient.call(this, operation);
      return this.drawAwaitingAndBufferEdges();
    };

    MyClient.prototype.applyServer = function(operation) {
      this.state.beforeApplyServer(this);
      MyClient.__super__.applyServer.call(this, operation);
      return this.drawAwaitingAndBufferEdges();
    };

    MyClient.prototype.serverAck = function() {
      this.state.beforeServerAck(this);
      MyClient.__super__.serverAck.call(this);
      return this.drawAwaitingAndBufferEdges();
    };

    MyClient.prototype.drawAwaitingAndBufferEdges = function() {
      return this.state.drawAwaitingAndBufferEdges(this);
    };

    return MyClient;

  })(Client);

  Client.Synchronized.prototype.beforeApplyClient = function(client) {
    client.goRightClient();
    return client.awaitingLength = 1;
  };

  Client.Synchronized.prototype.beforeApplyServer = function(client) {
    return client.goLeft();
  };

  Client.Synchronized.prototype.beforeServerAck = function() {};

  Client.Synchronized.prototype.drawAwaitingAndBufferEdges = function(client) {
    return client.setAwaitingAndBufferEdge(null, null);
  };

  Client.AwaitingConfirm.prototype.beforeApplyClient = function(client) {
    client.goRightClient();
    return client.bufferLength = 1;
  };

  Client.AwaitingConfirm.prototype.beforeApplyServer = function(client) {
    highlight($('.operation', client.stateEl));
    return client.goLeft();
  };

  Client.AwaitingConfirm.prototype.beforeServerAck = function(client) {
    client.goRightServer({
      length: client.awaitingLength
    });
    return delete client.awaitingLength;
  };

  Client.AwaitingConfirm.prototype.drawAwaitingAndBufferEdges = function(client) {
    return client.setAwaitingAndBufferEdge(client.serverStatePoint.goRight({
      length: client.awaitingLength
    }), null);
  };

  Client.AwaitingWithBuffer.prototype.beforeApplyClient = function(client) {
    client.bufferLength++;
    client.goRightClient();
    return highlight($('.operation', client.stateEl).eq(1));
  };

  Client.AwaitingWithBuffer.prototype.beforeApplyServer = Client.AwaitingConfirm.prototype.beforeApplyServer;

  Client.AwaitingWithBuffer.prototype.beforeServerAck = function(client) {
    client.goRightServer({
      length: client.awaitingLength
    });
    client.awaitingLength = client.bufferLength;
    return delete client.bufferLength;
  };

  Client.AwaitingWithBuffer.prototype.drawAwaitingAndBufferEdges = function(client) {
    var awaitingEdge, bufferEdge;
    awaitingEdge = client.serverStatePoint.goRight({
      length: client.awaitingLength
    });
    bufferEdge = awaitingEdge.endPoint.goRight({
      length: client.bufferLength
    });
    return client.setAwaitingAndBufferEdge(awaitingEdge, bufferEdge);
  };

  stateTransitions = [
    [
      Client.Synchronized, Client.AwaitingConfirm, function() {
        return $('> span', this.stateEl).text("Awaiting ").append(createOperationElement(this.state.outstanding).addClass('outstanding')).append(document.createTextNode(" "));
      }
    ], [
      Client.AwaitingConfirm, Client.AwaitingWithBuffer, function() {
        return $('<span>with buffer </span>').append(createOperationElement(this.state.buffer).addClass('buffer')).fadeIn().appendTo(this.stateEl);
      }
    ], [
      Client.AwaitingWithBuffer, Client.AwaitingConfirm, function() {
        var spans;
        spans = $('> span', this.stateEl);
        hideSpan(spans.eq(0));
        spans.get(1).firstChild.data = "Awaiting ";
        spans.eq(1).append(document.createTextNode(" "));
        return createOperationElement(this.state.outstanding).addClass('outstanding').replaceAll($('.operation', this.stateEl).eq(1));
      }
    ], [
      Client.AwaitingConfirm, Client.Synchronized, function() {
        return $('> span', this.stateEl).text("Synchronized");
      }
    ]
  ];

  tr = function(th, td, klass) {
    klass = klass ? ' class="' + klass + '"' : '';
    return "<tr" + klass + "><th>" + th + "</th><td>" + td + "</td></tr>";
  };

  unescape = function(str) {
    return str.replace(/\n/g, '\\n').replace(/\t/g, '\\t');
  };

  quote = function(str) {
    return '"' + str + '"';
  };

  hideSpan = function(span) {
    return span.animate({
      width: 0
    }, 500, function() {
      return span.remove();
    });
  };

  highlight = function(el) {
    el.removeClass('animate').addClass('highlighted');
    return _.defer(function() {
      el.addClass('animate');
      return _.defer(function() {
        return el.removeClass('highlighted');
      });
    });
  };

}).call(this);
