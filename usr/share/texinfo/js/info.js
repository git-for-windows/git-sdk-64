/* info.js - Javascript UI for Texinfo manuals
   Copyright (C) 2017-2019 Free Software Foundation, Inc.

   This file is part of GNU Texinfo.

   GNU Texinfo is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GNU Texinfo is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Texinfo.  If not, see <http://www.gnu.org/licenses/>.  */

(function (features, user_config) {
  "use strict";

  /*-------------------.
  | Define constants.  |
  `-------------------*/

  /** To override those default parameters, define 'INFO_CONFIG' in the global
      environment before loading this script.  */
  var config = {
    EXT: ".html",
    INDEX_NAME: "index.html",
    INDEX_ID: "index",
    CONTENTS_ID: "SEC_Contents",
    MAIN_ANCHORS: ["Top"],
    WARNING_TIMEOUT: 3000,
    SCREEN_MIN_WIDTH: 700,
    LOCAL_HTML_PAGE_PATTERN: "^([^:/]*[.](html|htm|xhtml))?([#].*)?$",
    SHOW_SIDEBAR_HTML: '<span class="hide-icon">&#x21db;</span>',
    HIDE_SIDEBAR_HTML: '<span class="hide-icon">&#x21da;</span><span class="hide-text">Hide sidebar</span>',
    SHOW_SIDEBAR_TOOLTIP: 'Show navigation sidebar',
    HIDE_SIDEBAR_TOOLTIP: 'Hide navigation sidebar',

    // hooks:
    /** Define a function called after 'DOMContentLoaded' event in
        the INDEX_NAME context.
        @type {function (): void}*/
    on_main_load: null,
    /** Define a function called after 'DOMContentLoaded' event in
        the iframe context.
        @type {(function (): void)} */
    on_iframe_load: null
  };

  /*-------------------.
  | State Management.  |
  `-------------------*/

  /** A 'store' is an object managing the state of the application and having
      a dispatch method which accepts actions as parameter.  This method is
      the only way to update the state.
      @typedef {function (Action): void} Action_consumer
      @type {{dispatch: Action_consumer, state?: any, listeners?: any[]}}.  */
  var store;

  var section_names = [
    'top', 'chapter', 'unnumbered', 'chapheading', 'appendix',
    'section', 'unnumberedsec', 'heading', 'appendixsec',
    'subsection', 'unnumberedsubsec', 'subheading', 'appendixsubsec',
    'subsubsection', 'unnumberedsubsubsec', 'subsubheading',
    'appendixsubsubsec' ];

  /** Create a Store that calls its listeners at each state change.
      @arg {function (Object, Action): Object} reducer
      @arg {Object} state  */
  function
  Store (reducer, state)
  {
    this.listeners = [];
    this.reducer = reducer;
    this.state = state;
  }

  /** @arg {Action} action */
  Store.prototype.dispatch = function dispatch (action) {
    var new_state = this.reducer (this.state, action);
    if (new_state !== this.state)
      {
        this.state = new_state;
        if (window["INFO_DEBUG"])
          console.log ("state: ", new_state);
        this.listeners.forEach (function (listener) {
          listener (new_state);
        });
      }
  };

  /** Build a store delegate that will forward its actions to a "real"
      store that can be in a different browsing context.  */
  function
  Remote_store ()
  {
    /* The browsing context containing the real store.  */
    this.delegate = window.parent;
  }

  /** Dispatch ACTION to the delegate browing context.  This method must be
      used in conjunction of an event listener on "message" events in the
      delegate browsing context which must forwards ACTION to an actual store.
      @arg {Action} action */
  Remote_store.prototype.dispatch = function dispatch (action) {
    this.delegate.postMessage ({ message_kind: "action", action: action }, "*");
  };

  /** @typedef {{type: string, [x: string]: any}} Action - Payloads
      of information meant to be treated by the store which can receive them
      using the 'store.dispatch' method.  */

  /* Place holder for action creators.  */
  var actions = {
    /** Return an action that makes LINKID the current page.
        @arg {string} linkid - link identifier
        @arg {string|false} [history] - method name that will be applied on
        the 'window.history' object.  */
    set_current_url: function (linkid, history, clicked = false) {
      if (undef_or_null (history))
        history = "pushState";
      return { type: "current-url", url: linkid,
               history: history, clicked: clicked };
    },

    /** Set current URL to the node corresponding to POINTER which is an
        id refering to a linkid (such as "*TOP*" or "*END*"). */
    set_current_url_pointer: function (pointer) {
      return { type: "current-url", pointer: pointer, history: "pushState" };
    },

    /** @arg {string} dir */
    navigate: function (dir) {
      return { type: "navigate", direction: dir, history: "pushState" };
    },

    /** @arg {{[id: string]: string}} links */
    cache_links: function (links) {
      return { type: "cache-links", links: links };
    },

    /** @arg {NodeListOf<Element>} links */
    cache_index_links: function (links) {
      var dict = {};
      for (var i = 0; i < links.length; i += 1)
        {
          var link = links[i];
          dict[link.textContent] = href_hash (link_href (link));
        }
      return { type: "cache-index-links", links: dict };
    },

    /** Show or hide the help screen.  */
    show_help: function () {
      return { type: "help", visible: true };
    },

    /** Make the text input INPUT visible.  If INPUT is a falsy value then
        hide current text input.
        @arg {string} input */
    show_text_input: function (input) {
      return { type: "input", input: input };
    },

    /** Hide the current current text input.  */
    hide_text_input: function () {
      return { type: "input", input: null };
    },

    /** @arg {string} msg */
    warn: function (msg) {
      return { type: "warning", msg: msg };
    },

    /** Search EXP in the whole manual.
        @arg {RegExp|string} exp*/
    search: function (exp) {
      var rgxp;
      if (typeof exp === "object")
        rgxp = exp;
      else if (exp === "")
        rgxp = null;
      else
        rgxp = new RegExp (exp, "i");

      return { type: "search-init", regexp: rgxp, input: exp };
    }
  };

  /** Update STATE based on the type of ACTION.  This update is purely
      fonctional since STATE is not modified in place and a new state object
      is returned instead.
      @arg {Object} state
      @arg {Action} action
      @return {Object} a new state */
  /* eslint-disable complexity */
  /* A "big" switch has been preferred over splitting each case in a separate
     function combined with a dictionary lookup.  */
  function
  updater (state, action)
  {
    var res = Object.assign ({}, state, { action: action });
    var linkid;
    switch (action.type)
      {
      case "cache-links":
        {
          var nodes = Object.assign ({}, state.loaded_nodes);
          Object
            .keys (action.links)
            .forEach (function (key) {
              if (typeof action.links[key] === "object")
                nodes[key] = Object.assign ({}, nodes[key], action.links[key]);
              else
                nodes[key] = action.links[key];
            });

          return Object.assign (res, { loaded_nodes: nodes });
        }
      case "cache-index-links":
        {
          res.index = Object.assign ({}, res.index, action.links);
          return res;
        }
      case "section":
        {
          res.section_hash = action.section_hash;
          return res;
        }
      case "current-url":
        {
          if (document.body.getAttribute("show-sidebar") == "yes"
              && is_narrow_window ()
              && action.clicked === "in-page")
            return state;
          linkid = (action.pointer) ?
              state.loaded_nodes[action.pointer] : action.url;

          res.current = linkid;
          res.section_hash = null;
          res.history = action.history;
          res.text_input = null;
          res.warning = null;
          res.help = false;
          res.focus = false;
          res.highlight = null;
          res.loaded_nodes = Object.assign ({}, res.loaded_nodes);
          res.loaded_nodes[linkid] = res.loaded_nodes[linkid] || {};
          return res;
        }
      case "unfocus":
        {
          if (!res.focus)
            return state;
          else
            {
              res.help = false;
              res.text_input = null;
              res.focus = false;
              return res;
            }
        }
      case "help":
        {
          res.help = action.visible;
          res.focus = true;
          return res;
        }
      case "show-sidebar": // "yes" (show), "no" (hide) or "hide-if-narrow"
        {
          res.show_sidebar = action.show === "hide-if-narrow" && res.show_sidebar === "no" ? "no" : action.show;
          return res;
        }
      case "navigate":
        {
          var current = state.current;
          var link = linkid_split (state.current);

          /* Handle inner 'config.INDEX_NAME' anchors specially.  */
          if (link.pageid === config.INDEX_ID)
            current = config.INDEX_ID;

          var ids = state.loaded_nodes[current];
          linkid = ids[action.direction];
          if (!linkid)
            {
              // Look for sibling link in ToC.
              // Needed for (say) @subsection without corresponding @node.
              let toc_current =
                document.querySelector ('#slider a[toc-current="yes"]');
              if (toc_current)
                {
                  let item_current = toc_current.parentNode; // 'li' element
                  let nlink = (action.direction === "next"
                               ? item_current.nextElementSibling
                               : action.direction === "prev"
                               ? item_current.previousElementSibling
                               : action.direction === "up"
                               ? item_current.parentNode.parentNode
                               : null);
                  if (nlink)
                    nlink = nlink.firstElementChild;
                  if (nlink && nlink.matches("a"))
                    {
                      let href = link_href (nlink)
                      if (href)
                        linkid = href_hash (href) ;
                    }
                }
            }
          if (!linkid)
            {
              /* When CURRENT is in index but doesn't have the requested
                 direction, ask its corresponding 'pageid'.  */
              var is_index_ref =
                Object.keys (state.index)
                      .reduce (function (acc, val) {
                        return acc || state.index[val] === current;
                      }, false);
              if (is_index_ref)
                {
                  ids = state.loaded_nodes[link.pageid];
                  linkid = ids[action.direction];
                }
            }

          if (!linkid)
            return state;
          else
            {
              res.current = linkid;
              res.section_hash = null;
              res.history = action.history;
              res.text_input = null;
              res.warning = null;
              res.help = false;
              res.highlight = null;
              res.focus = false;
              res.loaded_nodes = Object.assign ({}, res.loaded_nodes);
              res.loaded_nodes[action.url] = res.loaded_nodes[action.url] || {};
              return res;
            }
        }
      case "search-init":
        {
          res.search = {
            regexp: action.regexp || state.search.regexp,
            input: action.input || state.search.input,
            status: "ready",
            current_pageid: linkid_split (state.current).pageid,
            found: false
          };
          res.focus = false;
          res.help = false;
          res.text_input = null;
          res.warning = null;
          return res;
        }
      case "search-query":
        {
          res.search = Object.assign ({}, state.search);
          res.search.status = "searching";
          return res;
        }
      case "search-result":
        {
          res.search = Object.assign ({}, state.search);
          if (action.found)
            {
              res.search.status = "done";
              res.search.found = true;
              res.current = res.search.current_pageid;
              res.section_hash = null;
              res.history = "pushState";
              res.highlight = res.search.regexp;
            }
          else
            {
              var fwd = forward_pageid (state, state.search.current_pageid);
              if (fwd === null)
                {
                  res.search.status = "done";
                  res.warning = "Search failed: \"" + res.search.input + "\"";
                  res.highlight = null;
                }
              else
                {
                  res.search.current_pageid = fwd;
                  res.search.status = "ready";
                }
            }
          return res;
        }
      case "input":
        {
          var needs_update = (state.text_input && !action.input)
              || (!state.text_input && action.input)
              || (state.text_input && action.input
                  && state.text_input !== action.input);

          if (!needs_update)
            return state;
          else
            {
              res.focus = Boolean (action.input);
              res.help = false;
              res.text_input = action.input;
              res.warning = null;
              return res;
            }
        }
      case "iframe-ready":
        {
          res.ready = Object.assign ({}, res.ready);
          res.ready[action.id] = true;
          return res;
        }
      case "echo":
        {
          res.echo = action.msg;
          return res;
        }
      case "warning":
        {
          res.warning = action.msg;
          if (action.msg !== null)
            res.text_input = null;
          return res;
        }
      default:
        console.error ("no reducer for action type:", action.type);
        return state;
      }
  }
  /* eslint-enable complexity */

  /*-----------------------.
  | Context initializers.  |
  `-----------------------*/

  /** Initialize the index page of the manual which manages the state of the
      application.  */
  function
  init_index_page ()
  {
    /*--------------------------.
    | Component for help page.  |
    `--------------------------*/

    function
    Help_page ()
    {
      this.id = "help-screen";
      /* Create help div element.*/
      var div = document.createElement ("div");
      div.setAttribute ("hidden", "true");
      div.classList.add ("modal");
      div.innerHTML = "\
<div class=\"modal-content\">\
<span class=\"close\">&times;</span>\
<h2>Keyboard Shortcuts</h2>\
<table id=\"keyboard-shortcuts\">\
  <tbody>\
    <tr><th colspan=\"2\">Moving around</th></tr>\
    <tr><td><code><b>n / p</b></code></td>\
        <td>goto the next / previous section</td></tr>\
    <tr><td><code><b>[ / ]</b></code></td>\
        <td>goto the next / previous sibling</td></tr>\
    <tr><td><code><b>&lt; / &gt;</b></code></td>\
        <td>goto the first / last section</td></tr>\
    <tr><td><code><b>t</b></code></td><td>goto the first section</td></tr>\
    <tr><td><code><b>u</b></code></td>\
        <td>go one level up (parent section)</td></tr>\
    <tr><td><code><b>l / r</b></code></td>\
        <td>go back / forward in history.</td></tr>\
  </tbody>\
  <tbody>\
    <tr><th colspan=\"2\">Searching</th></tr>\
    <tr><td><code><b>i</b></code></td><td>search in the index</td></tr>\
    <tr><td><code><b>s</b></code></td>\
        <td>global text search in the manual</td></tr>\
    <tr><td><code><b>m</b></code></td>\
        <td>search in current node menu</td></tr>\
  </tbody>\
  <tbody>\
    <tr><th colspan=\"2\">Misc</th></tr>\
    <tr><td><code><b>?</b></code></td><td>show this help screen</td></tr>\
    <tr><td><code><b>Esc</b></code></td><td>hide this help screen</td></tr>\
  </tbody>\
</table>\
</div>\
";
      var span = div.querySelector (".close");
      div.addEventListener ("click", function (event) {
        if (event.target === span || event.target === div)
          store.dispatch ({ type: "unfocus" });
      }, false);

      this.element = div;
    }

    Help_page.prototype.render = function render (state) {
      if (!state.help)
        this.element.style.display = "none";
      else
        {
          this.element.style.display = "block";
          this.element.focus ();
        }
    };

    /*---------------------------------.
    | Components for menu navigation.  |
    `---------------------------------*/

    /** @arg {string} id */
    function
    Search_input (id)
    {
      this.id = id;
      this.render = null;
      this.prompt = document.createTextNode ("Text search" + ": ");

      /* Create input div element.*/
      var div = document.createElement ("div");
      div.setAttribute ("hidden", "true");
      div.appendChild (this.prompt);
      this.element = div;

      /* Create input element.*/
      var input = document.createElement ("input");
      input.setAttribute ("type", "search");
      this.input = input;
      this.element.appendChild (input);

      /* Define a special key handler when 'this.input' is focused and
         visible.*/
      this.input.addEventListener ("keydown", (function (event) {
        if (is_escape_key (event.key))
          store.dispatch ({ type: "unfocus" });
        else if (event.key === "Enter")
          store.dispatch (actions.search (this.input.value));
        event.stopPropagation ();
      }).bind (this));
    }

    Search_input.prototype.show = function show () {
      /* Display previous search. */
      var search = store.state.search;
      var input = search && (search.status === "done") && search.input;
      if (input)
        this.prompt.textContent = "Text search (default " + input + "): ";
      this.element.removeAttribute ("hidden");
      this.input.focus ();
    };

    Search_input.prototype.hide = function hide () {
      this.element.setAttribute ("hidden", "true");
      this.input.value = "";
    };

    /** @arg {string} id */
    function
    Text_input (id)
    {
      this.id = id;
      this.data = null;
      this.datalist = null;
      this.render = null;

      /* Create input div element.*/
      var div = document.createElement ("div");
      div.setAttribute ("hidden", "true");
      div.appendChild (document.createTextNode (id + ": "));
      this.element = div;

      /* Create input element.*/
      var input = document.createElement ("input");
      input.setAttribute ("type", "search");
      input.setAttribute ("list", id + "-data");
      this.input = input;
      this.element.appendChild (input);

      /* Define a special key handler when 'this.input' is focused and
         visible.*/
      this.input.addEventListener ("keydown", (function (event) {
        if (is_escape_key (event.key))
          store.dispatch ({ type: "unfocus" });
        else if (event.key === "Enter")
          {
            var linkid = this.data[this.input.value];
            if (linkid)
              {
                hide_sidebar_if_narrow ();
                store.dispatch (actions.set_current_url (linkid));
              }
          }
        event.stopPropagation ();
      }).bind (this));
    }

    /* Display a text input for searching through DATA.*/
    Text_input.prototype.show = function show (data) {
      if (!features || features.datalistelem)
        {
          var datalist = create_datalist (data);
          datalist.setAttribute ("id", this.id + "-data");
          this.data = data;
          this.datalist = datalist;
          this.element.appendChild (datalist);
        }
      this.element.removeAttribute ("hidden");
      this.input.focus ();
    };

    Text_input.prototype.hide = function hide () {
      this.element.setAttribute ("hidden", "true");
      this.input.value = "";
      if (this.datalist)
        {
          this.datalist.remove ();
          this.datalist = null;
        }
    };

    function
    Minibuffer ()
    {
      /* Create global container.*/
      var elem = document.createElement ("div");
      elem.classList.add ("text-input");

      var menu = new Text_input ("menu");
      menu.render = function (state) {
        if (state.text_input === "menu")
        {
          var current_menu = state.loaded_nodes[state.current].menu;
          if (current_menu)
            this.show (current_menu);
          else
            store.dispatch (actions.warn ("No menu in this node"));
        }
      };

      var index = new Text_input ("index");
      index.render = function (state) {
        if (state.text_input === "index")
          this.show (state.index);
      };

      var search = new Search_input ("regexp-search");
      search.render = function (state) {
        if (state.text_input === "regexp-search")
          this.show ();
      };

      elem.appendChild (menu.element);
      elem.appendChild (index.element);
      elem.appendChild (search.element);

      /* Create a container for warning when no menu in current page.*/
      var warn$ = document.createElement ("div");
      warn$.setAttribute ("hidden", "true");
      elem.appendChild (warn$);

      this.element = elem;
      this.menu = menu;
      this.index = index;
      this.search = search;
      this.warn = warn$;
      this.toid = null;
    }

    Minibuffer.prototype.render = function render (state) {
      if (!state.warning)
        {
          this.warn.setAttribute ("hidden", "true");
          this.toid = null;
        }
      else if (!this.toid)
        {
          console.warn (state.warning);
          var toid = window.setTimeout (function () {
            store.dispatch ({ type: "warning", msg: null });
          }, config.WARNING_TIMEOUT);
          this.warn.innerHTML = state.warning;
          this.warn.removeAttribute ("hidden");
          this.toid = toid;
        }

      if (!state.text_input || state.warning)
        {
          this.menu.hide ();
          this.index.hide ();
          this.search.hide ();
        }
      else
        {
          this.index.render (state);
          this.menu.render (state);
          this.search.render (state);
        }
    };

    function
    Echo_area ()
    {
      var elem = document.createElement ("div");
      elem.classList.add ("echo-area");
      elem.setAttribute ("hidden", "true");

      this.element = elem;
      this.toid = null;
    }

    Echo_area.prototype.render = function render (state) {
      if (!state.echo)
        {
          this.element.setAttribute ("hidden", "true");
          this.toid = null;
        }
      else if (!this.toid)
        {
          console.info (state.echo);
          var toid = window.setTimeout (function () {
            store.dispatch ({ type: "echo", msg: null });
          }, config.WARNING_TIMEOUT);
          this.element.innerHTML = state.echo;
          this.element.removeAttribute ("hidden");
          this.toid = toid;
        }
    };

    /*----------------------------.
    | Component for the sidebar.  |
    `----------------------------*/

    function
    Sidebar (contents_node)
    {
      this.element = document.createElement ("div"); // FIXME unneeded?
      this.element.setAttribute ("id", "slider");
      var div = document.createElement ("div");
      div.classList.add ("toc-sidebar");
      var toc = document.querySelector ("#SEC_Contents");
      toc.remove ();

      // Like n.cloneNode, but also copy _href
      function cloneNode(n)
      {
        let r = n.cloneNode(false);
        for (let c = n.firstChild; c; c = c.nextSibling)
          {
            let d = cloneNode(c);
            let h = c._href;
            if (h)
              d._href = h;
            r.appendChild(d);
          }
        return r;
      }
      contents_node.appendChild(cloneNode(toc));

      /* Remove table of contents header.  */
      toc = toc.querySelector(".contents"); // skip ToC header

      /* Move contents of <body> into a a fresh <div> to let the components
         treat the index page like other iframe page.  */
      var nav = document.createElement ("nav");
      nav.classList.add ("contents");
      for (var ch = toc.firstChild; ch; ch = toc.firstChild)
        nav.appendChild (ch);

      var div$ = document.createElement ("div");
      div$.classList.add ("toc");
      div$.appendChild (nav);
      div.appendChild (div$);
      this.element.appendChild (div);

      let header = document.createElement ("header");
      div$.parentElement.insertBefore (header, div$);
      let hider = document.createElement ("button");
      hider.classList.add ("sidebar-hider");
      hider.innerHTML = config.HIDE_SIDEBAR_HTML;
      this.show_sidebar_button = hider;
      header.appendChild(hider);
    }

    /* Render 'sidebar' according to STATE which is a new state. */
    Sidebar.prototype.render = function render (state) {
      /* Update sidebar to highlight the title corresponding to
         'state.current'.*/
      let currently_showing = document.body.getAttribute("show-sidebar");
      let show = state.show_sidebar;
      if (show == "hide-if-narrow")
        show = is_narrow_window() || currently_showing == "no" ? "no" : "yes";
      if (show === undefined)
        show = "yes";
      if (show !== currently_showing)
        {
          document.body.setAttribute("show-sidebar", show);
          this.show_sidebar_button.innerHTML = show == "yes" ? config.HIDE_SIDEBAR_HTML : config.SHOW_SIDEBAR_HTML;
          let tooltip = show == "yes" ? config.HIDE_SIDEBAR_TOOLTIP : config.SHOW_SIDEBAR_TOOLTIP;
          if (tooltip)
            this.show_sidebar_button.setAttribute("title", tooltip);
          else
            this.show_sidebar_button.removeAttribute("title");
        }
      var msg = { message_kind: "update-sidebar", selected: state.current, section_hash: state.section_hash };
      window.postMessage (msg, "*");
    };

    /*--------------------------.
    | Component for the pages.  |
    `--------------------------*/

    /** @arg {HTMLDivElement} index_div */
    function
    Pages (index_div)
    {
      index_div.setAttribute ("id", config.INDEX_ID);
      index_div.setAttribute ("node", config.INDEX_ID);
      index_div.setAttribute ("hidden", "true");
      this.element = document.createElement ("div");
      this.element.setAttribute ("id", "sub-pages");
      this.element.appendChild (index_div);
      /** @type {string[]} Currently created divs.  */
      this.ids = [config.INDEX_ID];
      /** @type {string} */
      this.prev_id = null;
      /** @type {HTMLElement} */
      this.prev_div = null;
      this.prev_search = null;
    }

    Pages.prototype.add_div = function add_div (pageid) {
      var div = document.createElement ("div");
      div.setAttribute ("id", pageid);
      div.setAttribute ("node", pageid);
      div.setAttribute ("hidden", "true");
      this.ids.push (pageid);
      this.element.appendChild (div);
      if (linkid_contains_index (pageid))
        load_page (pageid);
      return div;
    };

    Pages.prototype.render = function render (state) {
      var that = this;

      /* Create div elements for pages corresponding to newly added
         linkids from 'state.loaded_nodes'.*/
      Object.keys (state.loaded_nodes)
            .map (function (id) { return id.replace (/\..*$/, ""); })
            .filter (function (id) { return !that.ids.includes (id); })
            .reduce (function (acc, id) {
              return ((acc.includes (id)) ? acc : acc.concat ([id]));
            }, [])
            .forEach (function (id) { return that.add_div (id); });

      /* Blur pages if help screen is on.  */
      this.element.classList[(state.help) ? "add" : "remove"] ("blurred");

      if (state.current !== this.prev_id)
        {
          if (this.prev_id)
            {
              this.prev_div.setAttribute ("hidden", "true");
              /* Remove previous highlights.  */
              var old = linkid_split (this.prev_id);
              var msg = { message_kind: "highlight", regexp: null };
              post_message (old.pageid, msg);
            }
          var div = resolve_page (state.current, true);
          update_history (state.current, state.history);
          this.prev_id = state.current;
          this.prev_div = div;
        }

      if (state.search
          && (this.prev_search !== state.search)
          && state.search.status === "ready")
        {
          this.prev_search = state.search;
          if (state.search.current_pageid === config.INDEX_ID)
            {
              window.setTimeout (function () {
                store.dispatch ({ type: "search-query" });
                var res = search (document.getElementById (config.INDEX_ID),
                                  state.search.regexp);
                store.dispatch ({ type: "search-result", found: res });
              }, 0);
            }
          else
            {
              window.setTimeout (function () {
                store.dispatch ({ type: "search-query" });
                var msg = {
                  message_kind: "search",
                  regexp: state.search.regexp,
                  id: state.search.current_pageid
                };
                post_message (state.search.current_pageid, msg);
              }, 0);
            }
        }

      /* Update highlight of current page.  */
      if (!state.highlight)
        {
          if (state.current === config.INDEX_ID)
            remove_highlight (document.getElementById (config.INDEX_ID));
          else
            {
              var link = linkid_split (state.current);
              var msg$ = { message_kind: "highlight", regexp: null };
              post_message (link.pageid, msg$);
            }
        }

      /* Scroll to highlighted search result. */
      if (state.search
          && (this.prev_search !== state.search)
          && state.search.status === "done"
          && state.search.found === true)
        {
          var link$ = linkid_split (state.current);
          var msg$$ = { message_kind: "scroll-to", hash: "#search-result" };
          post_message (link$.pageid, msg$$);
          this.prev_search = state.search;
        }
    };

    /*--------------.
    | Utilitaries.  |
    `--------------*/

    /** Load PAGEID.
        @arg {string} pageid */
    function
    load_page (pageid)
    {
      var div = resolve_page (pageid, false);
      /* Making the iframe visible triggers the load of the iframe DOM.  */
      if (div.hasAttribute ("hidden"))
        {
          div.removeAttribute ("hidden");
          div.setAttribute ("hidden", "true");
        }
    }

    /** Return the div element that correspond to LINKID.  If VISIBLE is true
        then display the div and position the corresponding iframe to the
        anchor specified by LINKID.
        @arg {string} linkid - link identifier
        @arg {boolean} [visible]
        @return {HTMLElement} the div element.  */
    function
    resolve_page (linkid, visible)
    {
      var msg;
      var link = linkid_split (linkid);
      var pageid = link.pageid;
      var div = document.getElementById (link.pageid);
      if (!div)
        {
          msg = "no iframe container correspond to identifier: " + pageid;
          throw new ReferenceError (msg);
        }

      /* Create iframe if necessary unless the div is refering to the Index
         or Contents page.  */
      if (pageid === config.INDEX_ID || pageid === config.CONTENTS_ID)
        {
          div.removeAttribute ("hidden");
          /* Unlike iframes, Elements are unlikely to be scrollable (CSSOM
             Scroll-behavior), so choose an arbitrary element inside "index"
             div and at the top of it.  */
          document.getElementById ("icon-bar").scrollIntoView ();
        }
      else
        {
          var iframe = div.querySelector ("iframe");
          if (!iframe)
            {
              iframe = document.createElement ("iframe");
              iframe.classList.add ("node");
              iframe.setAttribute ("src", linkid_to_url (pageid));
              div.appendChild (iframe);
              iframe.addEventListener ("load", function () {
                store.dispatch ({ type: "iframe-ready", id: pageid });
              }, false);
            }
          if (visible)
            {
              div.removeAttribute ("hidden");
              msg = { message_kind: "scroll-to", hash: link.hash };
              post_message (pageid, msg);
            }
        }

      return div;
    }

    /** Create a datalist element containing option elements corresponding
        to the keys in MENU.  */
    function
    create_datalist (menu)
    {
      var datalist = document.createElement ("datalist");
      Object.keys (menu)
            .sort ()
            .forEach (function (title) {
              var opt = document.createElement ("option");
              opt.setAttribute ("value", title);
              datalist.appendChild (opt);
            });
      return datalist;
    }

    /** Mutate the history of page navigation.  Store LINKID in history
        state, The actual way to store LINKID depends on HISTORY_MODE.
        @arg {string} linkid
        @arg {string} history_mode */
    function
    update_history (linkid, history_mode)
    {
      var method = window.history[history_mode];
      if (method)
        {
          /* Pretend that the current page is the one corresponding to the
             LINKID iframe.  Handle errors since changing the visible file
             name can fail on some browsers with the "file:" protocol.  */
          var visible_url =
            dirname (window.location.pathname) + linkid_to_url (linkid);
          try
            {
              method.call (window.history, linkid, null, visible_url);
            }
          catch (err)
            {
              /* Fallback to changing only the hash part which is safer.  */
              visible_url = window.location.pathname;
              if (linkid !== config.INDEX_ID)
                visible_url += ("#" + linkid);
              method.call (window.history, linkid, null, visible_url);
            }
        }
    }

    /** Send MSG to the browsing context corresponding to PAGEID.  This is a
        wrapper around 'Window.postMessage' which ensures that MSG is sent
        after PAGEID is loaded.
        @arg {string} pageid
        @arg {any} msg */
    function
    post_message (pageid, msg)
    {
      if (pageid === config.INDEX_ID || pageid === config.CONTENTS_ID)
        window.postMessage (msg, "*");
      else
        {
          load_page (pageid);
          var iframe = document.getElementById (pageid)
                               .querySelector ("iframe");
          /* Semantically this would be better to use "promises" however
             they are not available in IE.  */
          if (store.state.ready[pageid])
            iframe.contentWindow.postMessage (msg, "*");
          else
            {
              iframe.addEventListener ("load", function handler () {
                this.contentWindow.postMessage (msg, "*");
                this.removeEventListener ("load", handler, false);
              }, false);
            }
        }
    }

    /*--------------------------------------------
    | Event handlers for the top-level context.  |
    `-------------------------------------------*/

    /* Initialize the top level 'config.INDEX_NAME' DOM.  */
    function
    on_load ()
    {
      fix_links (document.links);
      add_icons ();
      document.body.classList.add ("mainbar");

      /* Move contents of <body> into a a fresh <div> to let the components
         treat the index page like other iframe page.  */
      var index_div = document.createElement ("div");
      for (var ch = document.body.firstChild; ch; ch = document.body.firstChild)
        index_div.appendChild (ch);

      /* Aggregation of all the sub-components.  */
      var components = {
        element: document.body,
        components: [],

        add: function add (component) {
          this.components.push (component);
          this.element.appendChild (component.element);
        },

        render: function render (state) {
          this.components
              .forEach (function (cmpt) { cmpt.render (state); });

          /* Ensure that focus is on the current node unless some component is
             focused.  */
          if (!state.focus)
            {
              var link = linkid_split (state.current);
              var elem = document.getElementById (link.pageid);
              if (link.pageid !== config.INDEX_ID && link.pageid !== config.CONTENTS_ID)
                elem.querySelector ("iframe").focus ();
              else
                {
                  /* Move the focus to top DOM.  */
                  document.documentElement.focus ();
                  /* Allow the spacebar scroll in the main page to work.  */
                  elem.focus ();
                }
            }
        }
      };
      var pages = new Pages (index_div);
      components.add (pages);
      var contents_node = pages.add_div(config.CONTENTS_ID);
      components.add (new Sidebar (contents_node));
      components.add (new Help_page ());
      components.add (new Minibuffer ());
      components.add (new Echo_area ());
      store.listeners.push (components.render.bind (components));

      if (window.location.hash)
        {
          var linkid = normalize_hash (window.location.hash);
          store.dispatch (actions.set_current_url (linkid, "replaceState"));
        }

      /* Retrieve NEXT link and local menu.  */
      var links = {};
      links[config.INDEX_ID] = navigation_links (document);
      store.dispatch (actions.cache_links (links));
      store.dispatch ({ type: "iframe-ready", id: config.INDEX_ID });
      store.dispatch ({
        type: "echo",
        msg: "Welcome to Texinfo documentation viewer 6.1, type '?' for help."
      });

      /* Call user hook.  */
      if (config.on_main_load)
        config.on_main_load ();
    }

    /* Handle messages received via the Message API.
       @arg {MessageEvent} event */
    function
    on_message (event)
    {
      var data = event.data;
      if (data.message_kind === "action")
        {
          /* Follow up actions to the store.  */
          store.dispatch (data.action);
        }
    }

    /* Event handler for 'popstate' events.  */
    function
    on_popstate (event)
    {
      /* When EVENT.STATE is 'null' it means that the user has manually
         changed the hash part of the URL bar.  */
      var linkid = (event.state === null) ?
        normalize_hash (window.location.hash) : event.state;
      store.dispatch (actions.set_current_url (linkid, false));
    }

    return {
      on_load: on_load,
      on_message: on_message,
      on_popstate: on_popstate
    };
  }

  /** Initialize the iframe which contains the lateral table of content.  */
  function
  init_sidebar ()
  {
    /* Keep children but remove grandchildren (Exception: don't remove
       anything on the current page; however, that's not a problem in the Kawa
       manual).  */
    function
    hide_grand_child_nodes (ul, excluded)
    {
      var lis = ul.children;
      for (var i = 0; i < lis.length; i += 1)
        {
          if (lis[i] === excluded)
            continue;
          var first = lis[i].firstElementChild;
          if (first && first.matches ("ul"))
            hide_grand_child_nodes (first);
          else if (first && first.matches ("a"))
            {
              var ul$ = first && first.nextElementSibling;
              if (ul$)
                ul$.setAttribute ("toc-detail", "yes");
            }
        }
    }

    /** Make the parent of ELEMS visible.
        @arg {HTMLElement} elem */
    function
    mark_parent_elements (elem)
    {
      if (elem && elem.parentElement && elem.parentElement.parentElement)
        {
          var pparent = elem.parentElement.parentElement;
          for (var sib = pparent.firstElementChild; sib;
               sib = sib.nextElementSibling)
            {
              if (sib !== elem.parentElement
                  && sib.firstElementChild
                  && sib.firstElementChild.nextElementSibling)
                {
                  sib.firstElementChild
                     .nextElementSibling
                     .setAttribute ("toc-detail", "yes");
                }
            }
        }
    }

    /** Scan ToC entries to see which should be hidden.
        @arg {HTMLElement} elem
        @arg {string} linkid */
    function
    scan_toc (elem, linkid, section_hash = null)
    {
      /** @type {Element} */
      var res;
      if (section_hash)
        {
          let dot = linkid.lastIndexOf('.');
          if (dot >= 0)
            linkid = linkid.substring(0, dot+1) + section_hash;
        }
      var url = with_sidebar_query (linkid_to_url (linkid));

      /** Set CURRENT to the node corresponding to URL linkid.
          @arg {Element} elem */
      function
      find_current (elem)
      {
        if (elem.localName === "a" && link_href(elem) == url)
          {
            elem.setAttribute ("toc-current", "yes");
            var sib = elem.nextElementSibling;
            if (sib && sib.matches ("ul"))
              hide_grand_child_nodes (sib);
            res = elem;
          }
      }

      var ul = elem.querySelector ("ul");
      if (linkid === config.INDEX_ID || linkid === config.CONTENTS_ID)
        {
          hide_grand_child_nodes (ul);
          res = document.getElementById(linkid);
        }
      else
        {
          depth_first_walk (ul, find_current, Node.ELEMENT_NODE);
          /* Mark every parent node.  */
          var current = res;
          while (current && current !== ul)
            {
              mark_parent_elements (current);
              /* XXX: Special case for manuals with '@part' commands.  */
              if (current.parentElement === ul)
                hide_grand_child_nodes (ul, current);
              current = current.parentElement;
            }
        }
      return res;
    }

    /* Build the global dictionary containing navigation links from NAV.  NAV
       must be an 'ul' DOM element containing the table of content of the
       manual.  */
    function
    create_link_dict (nav)
    {
      var prev_id = config.INDEX_ID;
      var links = {};

      function
      add_link (elem)
      {
        let href;
          if (elem.matches ("a") && (href = link_href(elem)))
          {
            var id = href_hash (href);
            links[prev_id] =
              Object.assign ({}, links[prev_id], { forward: id });
            links[id] = Object.assign ({}, links[id], { backward: prev_id });
            prev_id = id;
          }
      }

      depth_first_walk (nav, add_link, Node.ELEMENT_NODE);
      /* Add a reference to the first and last node of the manual.  */
      links["*TOP*"] = config.INDEX_ID;
      links["*END*"] = prev_id;
      return links;
    }

    /* Add a link from the sidebar to the main index file.  ELEM is the first
       sibling of the newly created header.  */
    function
    add_header (elem)
    {
      var h1 = document.querySelector ("h1.settitle");
      if (h1)
        {
          var a = document.createElement ("a");
          a.setAttribute ("href", config.INDEX_NAME);
          a.setAttribute ("id", config.INDEX_ID);

          let header = elem.previousSibling;
          header.appendChild (a);
          if (window.sidebarLinkAppendContents)
            window.sidebarLinkAppendContents(a, h1.textContent);
          else
            {
              var div = document.createElement ("div");
              a.appendChild (div);
              var span = document.createElement ("span");
              span.textContent = h1.textContent;
              div.appendChild (span);
            }
        }
    }

    /*------------------------------------------
    | Event handlers for the sidebar context.  |
    `-----------------------------------------*/

    /* Initialize TOC_FILENAME which must be loaded in the context of an
       iframe.  */
    function
    on_load ()
    {
      var toc_div = document.getElementById ("slider");
      add_header (toc_div.querySelector (".toc"));

      /* Specify the base URL to use for all relative URLs.  */
      /* FIXME: Add base also for sub-pages.  */
      var base = document.createElement ("base");
      base.setAttribute ("href",
                         window.location.href.replace (/[/][^/]*$/, "/"));
      document.head.appendChild (base);

      scan_toc (toc_div, config.INDEX_NAME);

      /* Get 'backward' and 'forward' link attributes.  */
      var dict = create_link_dict (toc_div.querySelector ("nav.contents ul"));
      store.dispatch (actions.cache_links (dict));
    }

    /* Handle messages received via the Message API.  */
    function
    on_message (event)
    {
      var data = event.data;
      if (data.message_kind === "update-sidebar")
        {
          var toc_div = document.getElementById ("slider");

          /* Reset previous calls to 'scan_toc'.  */
          depth_first_walk (toc_div, function clear_toc_styles (elem) {
            elem.removeAttribute ("toc-detail");
            elem.removeAttribute ("toc-current");
          }, Node.ELEMENT_NODE);

          /* Remove the hash part for the main page.  */
          var pageid = linkid_split (data.selected).pageid;
          var selected = (pageid === config.INDEX_ID) ? pageid : data.selected;
          /* Highlight the current LINKID in the table of content.  */
          var elem = scan_toc (toc_div, selected, data.section_hash);
          if (elem)
            elem.scrollIntoView (true);
        }
    }

    return {
      on_load: on_load,
      on_message: on_message
    };
  }

  /** Initialize iframes which contain pages of the manual.  */
  function
  init_iframe ()
  {
    /* Initialize the DOM for generic pages loaded in the context of an
       iframe.  */
    function
    on_load ()
    {
      document.body.classList.add ("in-iframe");
      fix_links (document.links);
      var links = {};
      var linkid = basename (window.location.pathname, /[.]x?html$/);
      links[linkid] = navigation_links (document);
      store.dispatch (actions.cache_links (links));

      if (linkid_contains_index (linkid))
        {
          /* Scan links that should be added to the index.  */
          var index_links = document.querySelectorAll ("td[valign=top] a");
          store.dispatch (actions.cache_index_links (index_links));
        }

      add_icons ();

      /* Call user hook.  */
      if (config.on_iframe_load)
        config.on_iframe_load ();
    }

    /* Handle messages received via the Message API.  */
    function
    on_message (event)
    {
      var data = event.data;
      if (data.message_kind === "highlight")
        remove_highlight (document.body);
      else if (data.message_kind === "search")
        {
          var found = search (document.body, data.regexp);
          store.dispatch ({ type: "search-result", found: found });
        }
      else if (data.message_kind === "scroll-to")
        {
          /* Scroll to the anchor corresponding to HASH.  */
          if (data.hash)
            {
              let elem = document.getElementById(data.hash.substring(1));
              // Check if hash reference is to a sectioing element.  
              // If not we need to find the outer sectioning element,
              // so we can update the sidebar's ToC correctly.
              if (elem)
                {
                  let p = elem;
                  let section = null;
                  let id = null;
                  for (let p = elem;
                       section === null && p instanceof Element;
                        p = p.parentNode)
                    {
                      let sid = p.getAttribute("id");
                      if (sid == null)
                        continue;
                      let cl = p.classList;
                      for (let i = cl.length; --i >= 0; )
                        {
                          if (section_names.indexOf(cl.item(i)) >= 0)
                            {
                              section = p;
                              id = sid;
                              break;
                            }
                        }
                    }
                    if (section && section !== elem)
                      {
                        // Send section id to sidebar so it can properly update.
                        store.dispatch({ type: "section", hash: data.hash, section_hash: id } );
                      }
                }
                window.location.replace (data.hash);
            }
          else
            window.scroll (0, 0);
        }
    }

    return {
      on_load: on_load,
      on_message: on_message
    };
  }

  /*-------------------------
  | Common event handlers.  |
  `------------------------*/

  /** Handle click events.  */
  function
  on_click (event)
  {
    let in_sidebar = event.target.matches ("#slider *");
    for (var target = event.target; target !== null; target = target.parentNode)
      {
        if (! (target instanceof Element))
          continue;
        if (target.matches ("a"))
          {
            var href = link_href(target);
            if (href && maybe_pageref_url_p (href)
                  && !external_manual_url_p (href))
              {
                var linkid = href_hash (href) || config.INDEX_ID;
                if (linkid === "index.SEC_Contents")
                    linkid = config.CONTENTS_ID;
                store.dispatch (actions.set_current_url (linkid, null,
                                                         in_sidebar ? "in-sidebar" : "in-page"));
                event.preventDefault ();
                event.stopPropagation ();
                break;
              }
          }
        if (target.matches (".sidebar-hider"))
          {
              let body = document.body;
              let show = body.getAttribute("show-sidebar");
              show_sidebar (show==="no");
              return;
          }
      }
    if (! in_sidebar)
      hide_sidebar_if_narrow ();
  }

  // Only valid when showing sidebar.
  function is_narrow_window ()
  {
    return document.body.firstChild.offsetLeft == 0;
  }

  function show_sidebar (show)
  {
    store.dispatch({ type: "show-sidebar", show: show ? "yes" : "no" });
  }

  function hide_sidebar_if_narrow ()
  {
    store.dispatch({ type: "show-sidebar", show: "hide-if-narrow" });
  }

  /** Handle unload events.  */
  function
  on_unload ()
  {
    /* Cross origin requests are not supported in file protocol.  */
    if (window.location.protocol !== "file:")
    {
      var request = new XMLHttpRequest ();
      request.open ("GET", "(WINDOW-CLOSED)");
      request.send (null);
    }
  }

  /** Handle Keyboard 'keydown' events.
      @arg {KeyboardEvent} event */
  function
  on_keydown (event)
  {
    if (is_escape_key (event.key))
      store.dispatch ({ type: "unfocus" });
    else
      {
        var val = on_keydown.dict[event.key];
        if (val)
          {
            if (typeof val === "function")
              val ();
            else
              store.dispatch (val);
          }
      }
  }

  /* Associate an Event 'key' property to an action or a thunk.  */
  on_keydown.dict = {
    i: actions.show_text_input ("index"),
    l: window.history.back.bind (window.history),
    m: actions.show_text_input ("menu"),
    n: actions.navigate ("next"),
    p: actions.navigate ("prev"),
    r: window.history.forward.bind (window.history),
    s: actions.show_text_input ("regexp-search"),
    t: actions.set_current_url_pointer ("*TOP*"),
    u: actions.navigate ("up"),
    "]": actions.navigate ("forward"),
    "[": actions.navigate ("backward"),
    "<": actions.set_current_url_pointer ("*TOP*"),
    ">": actions.set_current_url_pointer ("*END*"),
    "?": actions.show_help ()
  };

  /** Some standard methods used in this script might not be implemented by
      the current browser.  If that is the case then augment the prototypes
      appropriately.  */
  function
  register_polyfills ()
  {
    function
    includes (search, start)
    {
      start = start || 0;
      return ((start + search.length) <= this.length)
        && (this.indexOf (search, start) !== -1);
    }

    /* eslint-disable no-extend-native */
    if (!Array.prototype.includes)
      Array.prototype.includes = includes;

    if (!String.prototype.includes)
      String.prototype.includes = includes;

    if (!String.prototype.endsWith)
      {
        String.prototype.endsWith = function endsWith (search, position) {
          var subject_string = this.toString ();
          if (typeof position !== "number"
              || !isFinite (position)
              || Math.floor (position) !== position
              || position > subject_string.length)
            position = subject_string.length;

          position -= search.length;
          var last_index = subject_string.lastIndexOf (search, position);
          return last_index !== -1 && last_index === position;
        };
      }

    if (!Element.prototype.matches)
      {
        Element.prototype.matches = Element.prototype["matchesSelector"]
          || Element.prototype["mozMatchesSelector"]
          || Element.prototype["mozMatchesSelector"]
          || Element.prototype["msMatchesSelector"]
          || Element.prototype["webkitMatchesSelector"]
          || function element_matches (str) {
            var document = (this.document || this.ownerDocument);
            var matches = document.querySelectorAll (str);
            var i = matches.length;
            while ((i -= 1) >= 0 && matches.item (i) !== this);
            return i > -1;
          };
      }

    if (typeof Object.assign != "function")
      {
        Object.assign = function assign (target) {
          if (undef_or_null (target))
            throw new TypeError ("Cannot convert undefined or null to object");

          var to = Object (target);
          for (var index = 1; index < arguments.length; index += 1)
            {
              var next_source = arguments[index];
              if (undef_or_null (next_source))
                continue;
              for (var key in next_source)
                {
                  /* Avoid bugs when hasOwnProperty is shadowed.  */
                  if (Object.prototype.hasOwnProperty.call (next_source, key))
                    to[key] = next_source[key];
                }
            }
          return to;
        };
      }

    (function (protos) {
      protos.forEach (function (proto) {
        if (!proto.hasOwnProperty ("remove"))
          {
            Object.defineProperty (proto, "remove", {
              configurable: true,
              enumerable: true,
              writable: true,
              value: function value () {
                this.parentNode.removeChild (this);
              }
            });
          }
      });
    } ([Element.prototype, CharacterData.prototype, DocumentType.prototype]));
    /* eslint-enable no-extend-native */
  }

  /*---------------------.
  | Common utilitaries.  |
  `---------------------*/

  /** Check portably if KEY correspond to "Escape" key value.
      @arg {string} key */
  function
  is_escape_key (key)
  {
    /* In Internet Explorer 9 and Firefox 36 and earlier, the Esc key
       returns "Esc" instead of "Escape".  */
    return key === "Escape" || key === "Esc";
  }

  /** Check if OBJ is equal to 'undefined' or 'null'.  */
  function
  undef_or_null (obj)
  {
    return (obj === null || typeof obj === "undefined");
  }

  /** Return a relative URL corresponding to HREF, which refers to an anchor
      of 'config.INDEX_NAME'.  HREF must be a USVString representing an
      absolute or relative URL.  For example "foo/bar.html" will return
      "config.INDEX_NAME#bar".

      @arg {string} href.*/
  function
  with_sidebar_query (href)
  {
    if (basename (href) === config.INDEX_NAME)
      return config.INDEX_NAME;
    else
      {
        /* XXX: Do not use the URL API for IE portability.  */
        var url = with_sidebar_query.url;
        url.setAttribute ("href", href);
        var new_hash = "#" + basename (url.pathname, /[.]x?html/);
        /* XXX: 'new_hash !== url.hash' is a workaround to work with links
           produced by makeinfo which link to an anchor element in a page
           instead of directly to the page. */
        if (url.hash && new_hash !== url.hash)
          new_hash += ("." + url.hash.slice (1));
        return config.INDEX_NAME + new_hash;
      }
  }

  /* Use the same DOM element for every function call.  */
  with_sidebar_query.url = document.createElement ("a");

  /** Modify LINKS to handle the iframe based navigation properly.  Relative
      links will be opened inside the corresponding iframe and absolute links
      will be opened in a new tab.  If ID is true then define an "id"
      attribute with a linkid value for relative links.

      @typedef {HTMLAnchorElement|HTMLAreaElement} Links
      @arg {Links[]|HTMLCollectionOf<Links>} links
      @arg {boolean} [id]
      @return void  */
  function
  fix_links (links, id)
  {
    for (var i = 0; i < links.length; i += 1)
      {
        var link = links[i];
        var href = link_href(link);
        if (!href)
          continue;
        else if (external_manual_url_p (href))
          link.setAttribute ("target", "_top");
        else if (! maybe_pageref_url_p (href))
          link.setAttribute ("target", "_blank");
        else
          {
            var href$ = with_sidebar_query (href);
            if (href !== href$)
              {
                let protocol = location.protocol;
                if (protocol == "https:" || protocol == "http:")
                  link._href = href$;
                else
                  link.setAttribute ("href", href$);
              }
            if (id)
              {
                var linkid = (href$ === config.INDEX_NAME) ?
                    config.INDEX_ID : href_hash (href$);
                link.setAttribute ("id", linkid);
              }
          }
      }
  }

  /** Convert HASH which is something that can be found 'Location.hash' to a
      "linkid" which can be handled in our model.
      @arg {string} hash
      @return {string} linkid */
  function
  normalize_hash (hash)
  {
    var text = hash.slice (1);
    /* Some anchor elements are present in 'config.INDEX_NAME' and we need to
       handle link to them specially (i.e. not try to find their corresponding
       iframe).*/
    if (config.MAIN_ANCHORS.includes (text))
      return config.INDEX_ID + "." + text;
    else
      return text;
  }

  /** Return an object composed of the filename and the anchor of LINKID.
      LINKID can have the form "foobar.anchor" or just "foobar".
      @arg {string} linkid
      @return {{pageid: string, hash: string}} */
  function
  linkid_split (linkid)
  {
    if (!linkid.includes ("."))
      return { pageid: linkid, hash: "" };
    else
      {
        var ref = linkid.match (/^(.+)\.(.*)$/).slice (1);
        return { pageid: ref[0], hash: "#" + ref[1] };
      }
  }

  /** Convert LINKID which has the form "foobar.anchor" or just "foobar", to
      an URL of the form `foobar${config.EXT}#anchor`.
      @arg {string} linkid */
  function
  linkid_to_url (linkid)
  {
    if (linkid === config.INDEX_ID)
      return config.INDEX_NAME;
    else
      {
        var link = linkid_split (linkid);
        return link.pageid + config.EXT + link.hash;
      }
  }

  /** Check if 'URL' may be a cross-reference to another page in this manual. */
  function
  maybe_pageref_url_p (url)
  {
    return url.match(config.LOCAL_HTML_PAGE_PATTERN);
  }

  /** Check if 'URL' is a link to another manual.  For locally installed 
      manuals only. */
  function
  external_manual_url_p (url)
  {
    if (typeof url !== "string")
      throw new TypeError ("'" + url + "' is not a string");

    return url.match(/^..\//);
  }

  /** Return PATHNAME with any leading directory components removed.  If
      specified, also remove a trailing SUFFIX.  */
  function
  basename (pathname, suffix)
  {
    var res = pathname.replace (/.*[/]/, "");
    if (!suffix)
      return res;
    else if (suffix instanceof RegExp)
      return res.replace (suffix, "");
    else /* typeof SUFFIX === "string" */
      return res.replace (new RegExp ("[.]" + suffix), "");
  }

  /** Strip last component from PATHNAME and keep the trailing slash. For
      example if PATHNAME is "/foo/bar/baz.html" then return "/foo/bar/".
      @arg {string} pathname */
  function
  dirname (pathname)
  {
    var res = pathname.match (/\/?.*\//);
    if (res)
      return res[0];
    else
      throw new Error ("'location.pathname' is not recognized");
  }

  /** Apply FUNC to each nodes in the NODE subtree.  The walk follows a depth
      first algorithm.  Only consider the nodes of type NODE_TYPE.  */
  function
  depth_first_walk (node, func, node_type)
  {
    if (!node_type || (node.nodeType === node_type))
      func (node);

    for (var child = node.firstChild; child; child = child.nextSibling)
      depth_first_walk (child, func, node_type);
  }

  /** Return the "effective" href attribute of a link (a) element. */
  function
  link_href (alink)
  {
    return alink._href || alink.getAttribute("href");
  }

  /** Return the hash part of HREF without the '#' prefix.  HREF must be a
      string.  If there is no hash part in HREF then return the empty
      string.  */
  function
  href_hash (href)
  {
    if (typeof href !== "string")
      throw new TypeError (href + " is not a string");

    if (href.includes ("#"))
      return href.replace (/.*#/, "");
    else
      return "";
  }

  /** Check if LINKID corresponds to a page containing index links.  */
  function
  linkid_contains_index (linkid)
  {
    return linkid.match (/^.*-index$/i) || linkid.match (/^Index$/);
  }

  /** Retrieve PREV, NEXT, and UP links, and local menu from CONTENT and
      return an object containing references to those links.  CONTENT must be
      an object implementing the ParentNode interface (Element,
      Document...).  */
  function
  navigation_links (content)
  {
    var links = content.querySelectorAll ("a[accesskey][href]");
    var res = {};
    /* links have the form MAIN_FILE.html#FRAME-ID.  For convenience
       only store FRAME-ID.  */
    for (var i = 0; i < links.length; i += 1)
      {
        var link = links[i];
        var nav_id = navigation_links.dict[link.getAttribute ("accesskey")];
        if (nav_id)
          {
            var href = basename (link_href (link));
            if (href === config.INDEX_NAME)
              res[nav_id] = config.INDEX_ID;
            else
              res[nav_id] = href_hash (href);
          }
        else /* this link is part of local table of content. */
          {
            res.menu = res.menu || {};
            res.menu[link.text] = href_hash (link_href (link));
          }
      }

      return res;
  }

  navigation_links.dict = { n: "next", p: "prev", u: "up" };

  function
  add_icons ()
  {
    var div = document.createElement ("div");
    div.setAttribute ("id", "icon-bar");
    var span = document.createElement ("span");
    span.innerHTML = "?";
    // Set tool-tip (on hover)
    span.setAttribute ("title", "Help for keyboard shortcuts");
    span.classList.add ("icon");
    span.addEventListener ("click", function () {
      store.dispatch (actions.show_help ());
    }, false);
    div.appendChild (span);
    document.body.insertBefore (div, document.body.firstChild);
  }

  /** Check if ELEM matches SEARCH
      @arg {Element} elem
      @arg {RegExp} rgxp
      @return {boolean} */
  function
  search (elem, rgxp)
  {
    /** @type {Text} */
    var text = null;

    /** @arg {Text} node */
    function
    find (node)
    {
      if (rgxp.test (node.textContent))
        {
          /* Ignore previous match.  */
          var prev = node.parentElement.matches ("span.highlight");
          text = (prev) ? null : (text || node);
        }
    }

    depth_first_walk (elem, find, Node.TEXT_NODE);
    remove_highlight (elem);
    if (!text)
      return false;
    else
      {
        highlight_text (rgxp, text);
        return true;
      }
  }

  /** Find the pageid corresponding to forward direction.
      @arg {any} state
      @arg {string} linkid
      @return {string} the forward pageid */
  function
  forward_pageid (state, linkid)
  {
    var data = state.loaded_nodes[linkid];
    if (!data)
      throw new Error ("page not loaded: " + linkid);
    else if (!data.forward)
      return null;
    else
      {
        var cur = linkid_split (linkid);
        var fwd = linkid_split (data.forward);
        if (!fwd.hash && fwd.pageid !== cur.pageid)
          return fwd.pageid;
        else
          return forward_pageid (state, data.forward);
      }
  }

  /** Highlight text in NODE which match RGXP.
      @arg {RegExp} rgxp
      @arg {Text} node */
  function
  highlight_text (rgxp, node)
  {
    /* Skip elements corresponding to highlighted words to avoid infinite
       recursion.  */
    if (node.parentElement.matches ("span.highlight"))
      return;

    var matches = rgxp.exec (node.textContent);
    if (matches)
      {
        /* Create an highlighted element containing first match.  */
        var span = document.createElement ("span");
        span.appendChild (document.createTextNode (matches[0]));
        span.setAttribute ("id", "search-result");
        span.classList.add ("highlight");

        var right_node = node.splitText (matches.index);
        /* Remove first match from right node.  */
        right_node.textContent = right_node.textContent
                                           .slice (matches[0].length);
        node.parentElement.insertBefore (span, right_node);
      }
  }

  /** Remove every highlighted elements and inline their text content.
      @arg {Element} elem */
  function
  remove_highlight (elem)
  {
    var spans = elem.getElementsByClassName ("highlight");
    /* Replace spans with their inner text node.  */
    while (spans.length > 0)
      {
        var span = spans[0];
        var parent = span.parentElement;
        parent.replaceChild (span.firstChild, span);
      }
  }

  /*--------------.
  | Entry point.  |
  `--------------*/

  /** Depending on the role of the document launching this script, different
      event handlers are registered.  This script can be used in the context of:

      - the index page of the manual which manages the state of the application
      - the iframe which contains the lateral table of content
      - other iframes which contain other pages of the manual

      This is done to allow referencing the same script inside every HTML page.
      This has the benefits of reducing the number of HTTP requests required to
      fetch the Javascript code and simplifying the work of the Texinfo HTML
      converter.  */

  /* Display MSG bail out message portably.  */
  function
  error (msg)
  {
    /* XXX: This code needs to be highly portable.
       Check <https://quirksmode.org/dom/core/> for details.  */
    return function () {
        var div = document.createElement ("div");
        div.setAttribute ("class", "error");
        div.innerHTML = msg;
        var elem = document.body.firstChild;
        document.body.insertBefore (div, elem);
        window.setTimeout (function () {
          document.body.removeChild (div);
        }, config.WARNING_TIMEOUT);
      };
  }

  /* Check if current browser supports the minimum requirements required for
     properly using this script, otherwise bails out.  */
  if (features && !(features.es5
                    && features.classlist
                    && features.eventlistener
                    && features.hidden
                    && features.history
                    && features.postmessage
                    && features.queryselector))
    {
      window.onload = error ("'info.js' is not compatible with this browser");
      return;
    }

  /* Until we have a responsive design implemented, fallback to basic
     HTML navigation for small screen.  */
    /*
  if (window.screen.availWidth < config.SCREEN_MIN_WIDTH)
    {
      window.onload =
        error ("screen width is too small to display the table of content");
      return;
    }
*/

  register_polyfills ();
  /* Let the config provided by the user mask the default one.  */
  config = Object.assign (config, user_config);

  var inside_iframe = top !== window;
  var inside_index_page = window.location.pathname === config.INDEX_NAME
      || window.location.pathname.endsWith ("/" + config.INDEX_NAME)
      || window.location.pathname.endsWith ("/");

  if (inside_index_page)
    {
      var initial_state = {
        /* Dictionary associating page ids to next, prev, up, forward,
           backward link ids.  */
        loaded_nodes: {},
        /* Dictionary associating keyword to linkids.  */
        index: {},
        /* page id of the current page.  */
        current: config.INDEX_ID,
        /* dictionary associating a page id to a boolean.  */
        ready: {},
        /* Current mode for handling history.  */
        history: "replaceState",
        /* Define the name of current text input.  */
        text_input: null
      };

      store = new Store (updater, initial_state);
      var index = init_index_page ();
      var sidebar = init_sidebar ();
      window.addEventListener ("DOMContentLoaded", function () {
        index.on_load ();
        sidebar.on_load ();
      }, false);
      window.addEventListener ("message", index.on_message, false);
      window.addEventListener ("message", sidebar.on_message, false);
      window.onpopstate = index.on_popstate;
    }
  else if (inside_iframe)
    {
      store = new Remote_store ();
      var iframe = init_iframe ();
      window.addEventListener ("DOMContentLoaded", iframe.on_load, false);
      window.addEventListener ("message", iframe.on_message, false);
    }
  else
    {
      /* Jump to 'config.INDEX_NAME' and adapt the selected iframe.  */
      window.location.replace (with_sidebar_query (window.location.href));
    }

  /* Register common event handlers.  */
  window.addEventListener ("beforeunload", on_unload, false);
  window.addEventListener ("click", on_click, false);
  /* XXX: handle 'keydown' event instead of 'keypress' since Chromium
     doesn't handle the 'Escape' key properly.  See
     https://bugs.chromium.org/p/chromium/issues/detail?id=9061.  */
  window.addEventListener ("keydown", on_keydown, false);
} (window["Modernizr"], window["INFO_CONFIG"]));
