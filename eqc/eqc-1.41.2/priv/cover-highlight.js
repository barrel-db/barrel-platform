/*! Copyright Quviq AB 2014 */

var preview;
var code;
var viewport;
var last_line;
var previewed_line = 0;
var initialised = false;

var bar_focus = false;
var dragging  = false;
var drag_delta   = 0;
var current_line = 0;

var preview_above = 5;
var preview_below = 5;

function init() {
  preview  = $('#preview-window');
  code     = $('.code');
  viewport = $('#viewport-indicator');
  preview.toggle(false);
  last_line = parseInt($(".anchor").last()[0].id.slice(4));
  code.scroll(on_code_scroll);
  on_code_scroll();
  $(window).mouseup(mouse_up);
  $(window).mousedown(mouse_down);
  num_summary_lines = $(".summary-line").length;
  adapt_summary();
  initialised = true;
}

function debug(s) { $('#console').empty().append(s) }

function resize() {
  if (!initialised) { init(); };
  on_code_scroll();
  adapt_summary();
}

function mk_line (n) { return Math.max(1, Math.min(last_line, n)) }

function on_code_scroll () {
  if (!dragging) {
    var line = top_line();
    set_viewport($('.sline' + line).offset().left, line);
  }
}

function highlight_line (n, yes) {
  if (yes) {
    $("#line" + n).next().addClass("highlighted");
  } else {
    $("#line" + n).next().removeClass("highlighted");
  }
}

function viewport_lines () {
  return bottom_line() - top_line() + 1;
}

function binary_search (a, b, f) {
  if (f(a) >= 0) { return a }
  if (f(b) <= 0) { return b }
  while (true) {
    if (a == b) { return a }
    var x  = Math.floor((a + b) / 2);
    var fx = f(x);
    if (fx == 0 || x == a) { return x }
    else if (fx < 0) { a = x }
    else if (fx > 0) { b = x }
  }
}

function is_visible (n) {
  var codeTop = code.offset().top;
  var codeBot = codeTop + code.height();
  var line    = $('#line' + n).next();
  var lineMid = line.offset().top + line.height() / 2;
  if (lineMid <  codeTop) { return -1 }
  if (lineMid >= codeBot) { return 1 }
  return 0;
}

function top_line () {
  return binary_search(1, last_line,
    function (n) {
      if (n == 1) { return is_visible(n) }
      var v = is_visible(n);
      if (v == 0) {
        if (is_visible(n - 1) == 0) { return 1 }
        return 0
      }
      return v;
    })
}

function bottom_line () {
  return binary_search(1, last_line,
    function (n) {
      if (n == last_line) { return is_visible(n) }
      var v = is_visible(n);
      if (v == 0) {
        if (is_visible(n + 1) == 0) { return -1 }
        return 0
      }
      return v;
    })
}

function set_viewport (x, line1) {
  x = Math.max(x, $('#summary-table').offset().left);
  if (0 == is_visible(last_line)) {
    line1 = top_line();
    x     = line_x(line1);
  }
  var line2 = bottom_line();
  var cell1 = $('.sline' + line1);
  var cell2 = $('.sline' + line2);
  var w     = cell2.offset().left + cell2.width() - x;
  viewport.offset({left: x});
  viewport.width(w);
}

function preview_line (line) {
  previewed_line = line;
  if (!dragging) {
    highlight_line(line, true);
  }
  preview.empty();
  var line1 = mk_line(line - preview_above);
  var line2 = mk_line(line + preview_below) + 1;
  var non_anchor = function () { return this.className != "anchor" };
  var items = $('#line' + line1).nextUntil('#line' + line2).clone().filter(non_anchor);
  items.find('.tooltip').each(function(_, t) { $(t).removeAttr('id') });
  preview.append(items);
}

function leave_line (line) {
  highlight_line(line, false)
}

function goto_line (line, pad) {
  if (pad == undefined) { pad = preview_above }
  var line1 = mk_line(line - pad);
  $('#line' + line1)[0].scrollIntoView();
}

function init_line (line) {
  this.line = line;
  console.dir("init line " + line)
}

function show_preview (show) {
  preview.toggle(show)
}

function line_x (l) {
  return $('.sline' + l).offset().left
}

function find_line (x) {
  var lines = $(".summary-line");
  var i = binary_search(0, lines.length - 1, function(n) {
    var line = $(lines[n]);
    var x0   = line.offset().left;
    var x1   = x0 + line.width();
    if (x < x0) { return 1 }
    if (x >= x1) { return -1 }
    return 0
  });
  return $(lines[i]).data("line")
}

function mouse_in_overview_bar (yes) {
  bar_focus = yes;
  if (!dragging) {
    preview.toggle(yes);
  }
}

function inside_x (widget, x) {
  var x0 = widget.offset().left,
      x1 = x0 + widget.width();
  return x >= x0 && x < x1;
}

function mouse_move (e) { on_drag(e.pageX) }

function on_drag (x0) {
  var x    = x0 + drag_delta,
      line = find_line(x);
  if (line != current_line) {
    current_line = line;
    goto_line(line, 0)
  }
  set_viewport(x, line);
}

function mouse_down (e) {
  if (bar_focus) {
    preview.toggle(false);
    highlight_line(previewed_line, false);
    dragging = true;
    if (inside_x(viewport, e.pageX)) {
      drag_delta = viewport.offset().left - e.pageX
    } else {
      var line = find_line(e.pageX);
      drag_delta = line_x(mk_line(line - Math.floor(viewport_lines()/2))) - e.pageX;
    }
    on_drag(e.pageX);
    $(window).mousemove(mouse_move);
  }
}

function mouse_up (e) {
  if (dragging) {
    $(window).off("mousemove");
    dragging = false;
  }
  if (bar_focus) {
    preview_line(previewed_line);
    preview.toggle(true);
  }
}

var hide_tooltip = {};

function show_tooltip(id, show, e) {
  var tooltip = $('#' + id);
  if (show) {
    if (!hide_tooltip[id]) {
      var p = tooltip.parent();
      var x = e.clientX + 10;
      var y = p.position().top + p.height() + 2;
      var h = tooltip.height();
      var bot = $(window).height();
      if (y + h >= bot) { y = bot - h - 10 }
      tooltip.css('left', x);
      tooltip.css('top',  y);
      tooltip.css('visibility', 'visible');
    } else {
      delete hide_tooltip[id];
    }
  } else {
    hide_tooltip[id] = true;
    // We get out/over events when moving between children.
    // To avoid moving the tooltip in this case we wait 10ms
    // before removing the tooltip.
    setTimeout(function() {
      if (hide_tooltip[id]) {
      tooltip.css('visibility', 'hidden');
        delete hide_tooltip[id];
      } }, 10);
  }
}

//-- Collapsing lines in overview bar ---------------------------------------

function collapse_whites () {
  $(".summary-line").each(function(_, line) {
    if (! $(line).hasClass("summary-green") &&
        ! $(line).hasClass("summary-red")) {
      $(line).addClass("collapsed")
    }
  });
  on_code_scroll();
}

function uncollapse_all () {
  $(".summary-line").each(function(_, line) {
    $(line).removeClass("collapsed")
  });
  on_code_scroll();
}

// Only keep a single entry for each 'n' lines of the code.
// red > light green > dark green
function merge_lines (n) {
  var cur_line  = 1;
  var best      = undefined;
  var visible   = 0;

  if (n == undefined) { n = 1 }

  var green = function (a) {
    // yuck
    for (var i = 1; i <= 16; i++) {
      if (a.hasClass("level" + i))
        return i;
    }
    return 20;
  }

  var priority = function (a) {
      if (a == undefined)               { return 0 }
      if (a.hasClass("summary-red"))    { return 100 }
      return 30 - green(a);
    }

  var better = function(a, b) { return priority(a) > priority(b) }

  $(".summary-line").each(function(_, l) {
    var line   = $(l);
    var m      = line.data("line");
    if (line.hasClass("collapsed")) {
    } else if (m < cur_line + n) {
      if (better(line, best)) {
        if (best != undefined) {
          best.addClass("collapsed")
          visible--;
        }
        best = line;
        visible++;
      } else { line.addClass("collapsed") }
    } else {
      best = line;
      visible++;
    }
    if (m >= cur_line + n) {
      cur_line = m;
    }
  });
  on_code_scroll();
  return visible;
}

var collapse_factor = 0;
var num_summary_lines;

function summary_width (collapse) {
  if (collapse == 0) { return num_summary_lines }
  return Math.ceil(last_line / collapse);
}

function adapt_summary () {
  var width = window.innerWidth;
  var c = 0;

  for (; summary_width(c) > width; c++) { }

  if (c != collapse_factor) {
    uncollapse_all();
    if (c > 0) { merge_lines(c) }
    collapse_factor = c;
  }
  return c;
}


