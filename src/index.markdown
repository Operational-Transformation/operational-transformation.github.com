---
title: Operational Transformation
---

Introduction
------------

Software that lets you edit a shared document together with others over the internet can be really useful if you're working in a team, especially a distributed one. Nowadays, there is a lot of web-based software for simultaneous collaborative editing, no matter if you're working on rich-text documents, spreadsheets, presentations or source code. Let's take a moment to define more precisely what we mean with the term *realtime collaborative editing*.

What we want is that multiple people working at different computers can make changes to a document hosted on a server at any time. These changes are synchronized immediately with the other peers, in contrast to version control systems like Git, where one usually works on a feature all by oneself and eventually merges the changes back to the project. No client should have to communicate with the server or any other client *before* making a change. In particular, there is no need to acquire a lock from the server to make an edit and concurrent edits can occur. After all changes have been synchronized, every client should see the exact same document.

It turns out that implementing this kind of real-time collaboration is far from trivial. The most common solution responds to the name *Operational Transformation* (usually abbreviated *OT*). It originated from a [research paper](http://dl.acm.org/citation.cfm?doid=67544.66963) published in 1989 but got more recently popularized by [Google Wave](http://en.wikipedia.org/wiki/Google_Wave). Today, it powers many collaborative editors such as

<!--
  Google's series of posts explaining OT:
  * http://googledocs.blogspot.de/2010/09/whats-different-about-new-google-docs.html
  * http://googledocs.blogspot.de/2010/09/whats-different-about-new-google-docs_22.html
  * http://googledocs.blogspot.de/2010/09/whats-different-about-new-google-docs_23.html
-->

* [SubEthaEdit (code editor)](http://www.codingmonkeys.de/subethaedit/)
* [EtherPad](http://etherpad.org/)
* [Google Docs](https://docs.google.com/)
* [Mockingbird (tool for creating wireframes)](https://gomockingbird.com/)

These are only a few examples from a growing number of [applications with realtime collaboration](http://en.wikipedia.org/wiki/Collaborative_real-time_editor). I expect this number to rise even faster now that Google has published the Drive Realtime API, which is based on OT <!-- citation --> and let's third-party apps use the same collaboration as Google Docs.

Operational Transformation has a reputation of being hard to understand. In my opinion, this reputation is hardly justified. I will try to convince you that despite the fancy name *Operation Transformation*, the ideas behind it are simple and mathematically elegant. To do this, we're going to study the problem that OT solves in an idealized setting, thereby arriving at a working algorithm.

Operations
----------

But first of all, what are *operations*? Simply put, operations are representations of changes to a document. For example, the action "set the stroke color of the star in the foreground to 'purple'" is an operation on vector-based drawings. In rich-text editors, you may have the operation "mark the text 'Hello' in the first line as bold". Many applications already support operations (under a different name of course) to implement undo/redo. In essence, an operation records the difference between one version of a document and the next.

For simplicity's sake, we won't consider rich-text documents or vector-based drawings. Instead we're going to focus on plain-text documents for the rest of this introductory article. There are only two fundamental actions on text documents, which will be our definition of operations in this setting:

Insertions
:    An insertion consists of the inserted text and its position in the document. First, we'll have to choose a coordinate system for our document. We could use a combination of line number and position within that line, but treating a document like a sequence of characters and giving the index (zero-based) index seems somewhat simpler. For brevity, we adapt the notation <span class="op-insert">Insert("World", 6)</span> for an insertion at position 6.
Deletions
:   When we have insertions, we will of course support deletions, too. For example <span class="op-delete">Delete(5, 6)</span> is a deletion of 5 characters, beginning at position 6 (this would undo the insertion above).

All other changes can be represented in terms of these two actions. For example, the replacement of a character with another has the same effect as a deletion of a character followed by an insertion at the same position. Below, you can edit and see the resulting operations on the right:

<div id="operations-demo" class="demo row">
  <div class="span6 offset3 well">
    <div class="row">
      <div class="span4">
        <textarea></textarea>
      </div>
      <div class="span2 operations">
        <ul></ul>
      </div>
    </div>
  </div>
</div>

<script src="/static/ot/lib/simple-text-operation.js"></script>
<script>
$(document).ready(function () {
  var STO = ot.SimpleTextOperation;

  // Breaks a `SimpleTextOperation` up into an array of `SimpleTextOperation`s
  // that insert/delete only one char.
  function makeCharTextOperations (operation) {
    if (operation instanceof STO.Noop) { return []; }
    var charOperations = [];
    if (operation instanceof STO.Insert) {
      for (var i = 0; i < operation.str.length; i++) {
        charOperations.push(new STO.Insert(
          operation.str.charAt(i),
          operation.position + i
        ));
      }
    } else if (operation instanceof STO.Delete) {
      for (var c = operation.count; c > 0; c--) {
        charOperations.push(new STO.Delete(1, operation.position))
      }
    }
    return charOperations;
  }

  var operationsEl = $('#operations-demo');

  var cm = CodeMirror.fromTextArea(operationsEl.find('textarea').get(0), {
    lineNumbers: true,
    lineWrapping: true
  });
  cm.setValue(
    "Hello World!\n\n" +
    "Dolor sit amet consectetuer elit.\n\n" +
    "Type here to see operations on the right"
  );
  cm.on('change', function (cm, change) {
    var operation = ot.CodeMirrorAdapter.operationFromCodeMirrorChange(change, cm)[0];
    var simpleOperations = STO.fromTextOperation(operation);
    _.each(_.flatten(_.map(simpleOperations, makeCharTextOperations)), addToList);
  });

  var operationsList = operationsEl.find('ul');
  function addToList (operation) {
    if (operation instanceof STO.Noop) { return; }
    var listItem = $('<li />');
    if (operation instanceof STO.Insert) {
      listItem.html('<span class="op-insert">Insert(' + JSON.stringify(operation.str) + ', ' + operation.position + ')</span>')
    } else {
      listItem.html('<span class="op-delete">Delete(' + operation.position + ')</span>')
    }
    operationsList.append(listItem);
    operationsEl.find('.operations').animate({
      scrollTop: operationsList.height()
    }, 'fast');
  }
});
</script>

A first approach
----------------

Let's try to do the simplest thing that could possibly work!
