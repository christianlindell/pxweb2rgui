# Variabelrutor i samma stil som SCB:s webbplats: rubrik med "(Obligatoriskt)",
# Markera alla / Avmarkera alla, "Valda x Totalt y", sökfält för långa
# listor och en lista med samma klick som SCB:s <select multiple>: klick väljer
# ett värde, Ctrl+klick lägger till eller tar bort, Shift+klick väljer ett intervall.
# Listan är en egen komponent (div-element med en Shiny-inputbindning) i
# stället för en <select multiple>: webbläsarens listruta hoppar till toppen
# vid klick i vissa fönster, t.ex. RStudios Viewer. Beteendet finns i
# px_var_js nedan.

# Vilka värden som är förvalda, som på SCB: första tabellinnehållet och
# senaste perioden. Övriga variabler börjar utan val.
px_default_selection <- function(role, codes) {
  if (length(codes) == 0) return(character())
  if (identical(role, "time")) return(utils::tail(codes, 1))
  if (identical(role, "contents")) return(codes[1])
  character()
}

# Etiketter i listan. show = "code_value" (från metadata) visar koden före
# namnet, t.ex. "0114 Upplands Väsby", som SCB gör för regioner.
px_option_labels <- function(values, show = NA) {
  codes <- values$code
  labels <- ifelse(is.na(values$label) | values$label == "", codes, values$label)
  if (identical(show, "code_value")) {
    has_code <- startsWith(labels, codes)
    labels[!has_code] <- paste(codes[!has_code], labels[!has_code])
  }
  labels
}

# Kolumnen common, som bara finns med flera tabeller. En tibble varnar för saknade
# kolumner vid values$common.
px_common_of <- function(values) {
  if ("common" %in% names(values)) values[["common"]]
}

# common: FALSE för värden som bara vissa tabeller har (data-bara), som "Visa
# gemensamma värden" döljer. NULL när bara en tabell är vald.
px_options_html <- function(codes, labels, selected = character(), reverse = FALSE,
                            common = NULL) {
  ord <- if (reverse) rev(seq_along(codes)) else seq_along(codes)
  is_sel <- codes[ord] %in% selected
  bara <- if (is.null(common)) rep("", length(ord)) else ifelse(common[ord], "", ' data-bara="1"')
  paste0(
    '<div class="px-opt', ifelse(is_sel, " sel", ""), '" role="option" tabindex="-1"', bara,
    ' aria-selected="', ifelse(is_sel, "true", "false"), '"',
    ' data-value="', htmltools::htmlEscape(codes[ord], attribute = TRUE), '"',
    ' title="', htmltools::htmlEscape(codes[ord], attribute = TRUE), '">',
    htmltools::htmlEscape(labels[ord]), "</div>",
    collapse = ""
  )
}

# codelists: data.frame med id, label och type (från pxweb2_list_codelists())
# för just den här variabeln. Finns det några visas "Välj indelning", som på
# SCB, t.ex. Riket/Län/Kommuner för region.
px_var_box <- function(id, label, values, role, elimination, show = NA,
                       codelists = NULL, search_min = 10, selected = NULL, note = NULL,
                       common_toggle = FALSE) {
  codes <- values$code
  labels <- px_option_labels(values, show)
  selected <- selected %||% px_default_selection(role, codes)

  # tidsvariabeln visas med senaste perioden överst, som hos SCB
  options <- px_options_html(codes, labels, selected, reverse = identical(role, "time"),
                             common = px_common_of(values))

  shiny::div(
    class = paste("px-var", if (!isTRUE(elimination)) "px-var-required",
                  if (!isTRUE(elimination) && length(selected) == 0) "px-var-missing"),
    shiny::div(
      class = "px-var-head",
      shiny::tags$strong(label),
      if (!isTRUE(elimination)) {
        shiny::tags$small(class = "px-required",
                          title = "Minst ett värde måste väljas för den här variabeln",
                          " (Obligatoriskt)")
      },
      # vid flera tabeller: vilka tabeller som har variabeln och som kräver den
      lapply(note, function(n) shiny::div(class = "px-var-note small text-muted", n))
    ),
    if (!is.null(codelists) && nrow(codelists) > 0) {
      shiny::tags$select(
        class = "form-select form-select-sm px-indelning mt-1", `data-target` = id,
        shiny::tags$option(value = "", "Välj indelning (alla värden i tabellen)"),
        lapply(seq_len(nrow(codelists)), function(k) {
          shiny::tags$option(value = codelists$id[k], `data-type` = codelists$type[k],
                             codelists$label[k])
        })
      )
    },
    shiny::div(
      class = "px-var-links",
      shiny::tags$a(href = "#", class = "px-mark-all", `data-target` = id, "Markera alla"),
      shiny::tags$a(href = "#", class = "px-unmark-all", `data-target` = id, "Avmarkera alla")
    ),
    if (isTRUE(common_toggle)) {
      shiny::tags$label(
        class = "px-word-start",
        shiny::tags$input(type = "checkbox", class = "px-common-only", `data-target` = id),
        " Visa gemensamma värden"
      )
    },
    if (length(codes) >= search_min) {
      shiny::div(
        class = "px-var-search",
        shiny::tags$label(
          class = "px-word-start",
          shiny::tags$input(type = "checkbox", class = "px-search-start", `data-target` = id),
          " Början av ord"
        ),
        shiny::tags$input(type = "search", class = "form-control form-control-sm px-search",
                          placeholder = "Sök här", `data-target` = id)
      )
    },
    shiny::div(
      class = "px-var-count",
      "Valda ", shiny::tags$span(id = paste0(id, "_valda"), length(selected)),
      " Totalt ", shiny::tags$span(id = paste0(id, "_totalt"), length(codes))
    ),
    shiny::div(
      id = id, class = "px-list", role = "listbox", `aria-multiselectable` = "true",
      `aria-label` = label, tabindex = "0",
      shiny::HTML(options)
    ),
    if (identical(role, "time")) {
      shiny::div(
        class = "form-check mt-1",
        shiny::tags$input(type = "checkbox", class = "form-check-input",
                          id = paste0(id, "_senaste")),
        shiny::tags$label(class = "form-check-label small", `for` = paste0(id, "_senaste"),
                          "Hämta alltid senaste perioden (\"9999\" i koden)")
      )
    }
  )
}

px_var_css <- "
.px-vars { display: flex; flex-wrap: wrap; gap: 1rem; align-items: flex-start; }
.px-var { background: var(--bs-tertiary-bg); padding: 0.9rem; width: 360px; max-width: 100%; }
.px-var-head small { color: var(--bs-body-color); }
.px-var-missing { outline: 2px solid var(--bs-danger); }
.px-var-missing .px-required { color: var(--bs-danger); font-weight: 600; }
.px-var-links { margin: 0.4rem 0; font-size: 0.85rem; }
.px-var-links a { margin-right: 0.6rem; }
.px-var-search { margin-bottom: 0.3rem; }
.px-word-start { font-size: 0.8rem; display: block; margin-bottom: 0.2rem; }
.px-var-count { font-size: 0.8rem; margin-bottom: 0.2rem; }
.px-list { width: 100%; height: 12.4rem; overflow-y: auto; border: 1px solid var(--bs-border-color);
           background: var(--bs-body-bg); font-size: 0.85rem; user-select: none; }
.px-opt { padding: 0.35rem 0.5rem; cursor: pointer; white-space: nowrap; overflow: hidden;
          text-overflow: ellipsis; }
.px-opt:hover { background: var(--bs-secondary-bg); }
.px-opt.sel { background: #222; color: #fff; }
.px-list.disabled { opacity: 0.5; pointer-events: none; }
.px-list.px-gemensamma .px-opt[data-bara] { display: none; }
"

px_var_js <- "
// Shiny-inputbindning för listan: värdet är koderna för markerade rader
var pxListBinding = new Shiny.InputBinding();
$.extend(pxListBinding, {
  find: function(scope) { return $(scope).find('.px-list'); },
  getValue: function(el) {
    return $(el).find('.px-opt.sel').map(function() {
      return this.getAttribute('data-value');
    }).get();
  },
  subscribe: function(el, callback) {
    $(el).on('change.pxList', function() { callback(); });
  },
  unsubscribe: function(el) { $(el).off('.pxList'); }
});
Shiny.inputBindings.register(pxListBinding, 'pxweb2rgui.list');

function pxVarCount(list) {
  var n = $(list).find('.px-opt.sel').length;
  $('#' + list.id + '_valda').text(n);
  // obligatorisk variabel utan val markeras med röd ram, som en påminnelse
  var box = $(list).closest('.px-var');
  var latest = $('#' + list.id + '_senaste').prop('checked');
  box.toggleClass('px-var-missing', box.hasClass('px-var-required') && n === 0 && !latest);
}
function pxToggle(opt, on) {
  $(opt).toggleClass('sel', on).attr('aria-selected', $(opt).hasClass('sel'));
}
// dolt av 'Visa gemensamma värden'
function pxCommonHidden(opt) {
  return opt.hasAttribute('data-bara') && $(opt).closest('.px-gemensamma').length > 0;
}
// 'Visa gemensamma värden': dolda värden avmarkeras, så att urvalet är det som syns
function pxCommonTotal(list) {
  var n = $(list).find('.px-opt').filter(function() { return !pxCommonHidden(this); }).length;
  $('#' + list.attr('id') + '_totalt').text(n);
}
$(document).on('change', '.px-common-only', function() {
  var list = $('#' + $(this).data('target'));
  list.toggleClass('px-gemensamma', this.checked);
  list.find('.px-opt').each(function() { if (pxCommonHidden(this)) pxToggle(this, false); });
  pxCommonTotal(list);
  list.trigger('change');
});
// Samma klick som i SCB:s lista (<select multiple>):
//   klick             väljer värdet och avmarkerar övriga
//   Ctrl+klick        lägger till eller tar bort värdet (Cmd på Mac)
//   Shift+klick       väljer intervallet från ankaret, övriga avmarkeras
//   Ctrl+Shift+klick  lägger intervallet till urvalet
// Ankaret är den rad som senast klickades utan Shift. Håller man ner musknappen
// och drar utökas intervallet, med samma läge som det första klicket gav. Drar man
// förbi listans kant scrollar den. Listan flyttar sig aldrig av sig själv.
var pxDrag = null;
function pxDragApply(opt) {
  var i = pxDrag.opts.index(opt);
  if (i < 0 || i === pxDrag.last) return;
  pxDrag.last = i;
  var lo = Math.min(pxDrag.start, i), hi = Math.max(pxDrag.start, i);
  pxDrag.opts.each(function(k) {
    // inom intervallet: dragläget; utanför: som före draget
    pxToggle(this, (k >= lo && k <= hi) ? pxDrag.mark : pxDrag.before[k]);
  });
  pxVarCount(pxDrag.list[0]);
}
function pxDragAt(y) {
  // raden vid y-positionen (begränsad till listans synliga del); raderna
  // är lika höga, så indexet räknas fram från den första synliga raden
  var opts = pxDrag.opts;
  if (opts.length === 0) return;
  var r = pxDrag.list[0].getBoundingClientRect();
  var yy = Math.max(r.top + 1, Math.min(r.bottom - 1, y));
  var first = opts[0].getBoundingClientRect();
  var i = Math.floor((yy - first.top) / first.height);
  pxDragApply(opts[Math.max(0, Math.min(opts.length - 1, i))]);
}
$(document).on('mousedown', '.px-list .px-opt', function(e) {
  if (e.button !== 0) return;
  e.preventDefault();
  var list = $(this).closest('.px-list');
  var opts = list.find('.px-opt').filter(function() {
    return this.style.display !== 'none' && !pxCommonHidden(this);
  });
  var ctrl = e.ctrlKey || e.metaKey;
  // ett ankare som har ersatts eller som ett sökord döljer räknas inte
  var ankare = list.data('pxAnkare');
  var start = (e.shiftKey && ankare) ? opts.index(ankare) : -1;
  if (start < 0) { start = opts.index(this); list.data('pxAnkare', this); }
  var mark = (ctrl && !e.shiftKey) ? !$(this).hasClass('sel') : true;
  // även värden som ett sökord döljer avmarkeras, som 'Valda' visar
  if (!ctrl) list.find('.px-opt').each(function() { pxToggle(this, false); });
  pxDrag = {
    list: list, opts: opts, start: start, last: -1,
    mark: mark, y: e.clientY, timer: null,
    before: opts.map(function() { return $(this).hasClass('sel'); }).get()
  };
  pxDragApply(this);
  pxDrag.timer = setInterval(function() {
    if (!pxDrag) return;
    var r = pxDrag.list[0].getBoundingClientRect();
    var step = pxDrag.y < r.top ? -Math.min(40, r.top - pxDrag.y) :
               pxDrag.y > r.bottom ? Math.min(40, pxDrag.y - r.bottom) : 0;
    if (step !== 0) { pxDrag.list[0].scrollTop += step; pxDragAt(pxDrag.y); }
  }, 50);
});
$(document).on('mousemove', function(e) {
  if (!pxDrag) return;
  pxDrag.y = e.clientY;
  pxDragAt(e.clientY);
});
$(document).on('mouseup', function() {
  if (!pxDrag) return;
  clearInterval(pxDrag.timer);
  var list = pxDrag.list;
  pxDrag = null;
  list.trigger('change');
});
$(document).on('keydown', '.px-list .px-opt', function(e) {
  if (e.key === ' ' || e.key === 'Enter') {
    e.preventDefault();
    pxToggle(this);
    $(this).closest('.px-list').data('pxAnkare', this).trigger('change');
  }
});
// Shiny skickar NULL både före första valet och när allt avmarkerats;
// '_andrad' skiljer de två fallen åt
$(document).on('change', '.px-list', function() {
  pxVarCount(this);
  Shiny.setInputValue(this.id + '_andrad', true);
});
$(document).on('click', '.px-mark-all, .px-unmark-all', function(e) {
  e.preventDefault();
  var list = $('#' + $(this).data('target'));
  var mark = $(this).hasClass('px-mark-all');
  list.find('.px-opt').each(function() {
    if (!mark || (this.style.display !== 'none' && !pxCommonHidden(this))) pxToggle(this, mark);
  });
  list.trigger('change');
});
// 'Välj alla värden': alla rader markeras, även de som ett sökord döljer,
// och 'senaste perioden' tas bort eftersom den annars går före markeringen
Shiny.addCustomMessageHandler('pxMarkAll', function(ids) {
  ids.forEach(function(id) {
    var latest = $('#' + id + '_senaste');
    if (latest.prop('checked')) latest.prop('checked', false).trigger('change');
    var list = $('#' + id);
    list.find('.px-opt').each(function() { if (!pxCommonHidden(this)) pxToggle(this, true); });
    list.trigger('change');
  });
});
function pxFilter(id) {
  var box = $('.px-search[data-target=\"' + id + '\"]');
  var q = (box.val() || '').toLowerCase().trim();
  var start = $('.px-search-start[data-target=\"' + id + '\"]').prop('checked');
  $('#' + id).find('.px-opt').each(function() {
    var t = this.textContent.toLowerCase();
    var hit = q === '' || (start ?
      t.split(/[\\s\\-\\/(),]+/).some(function(w) { return w.indexOf(q) === 0; }) :
      t.indexOf(q) >= 0);
    this.style.display = hit ? '' : 'none';
  });
}
$(document).on('input', '.px-search', function() { pxFilter($(this).data('target')); });
$(document).on('change', '.px-search-start', function() { pxFilter($(this).data('target')); });
// byte av indelning: servern hämtar kodlistan och skickar nya värden
$(document).on('change', 'select.px-indelning', function() {
  Shiny.setInputValue('px_indelning', {target: $(this).data('target'),
    codelist: this.value, nonce: Math.random()});
});
Shiny.addCustomMessageHandler('pxSetOptions', function(m) {
  var sel = $('#' + m.id);
  sel.html(m.html).scrollTop(0);
  $('.px-search[data-target=\"' + m.id + '\"]').val('');
  $('#' + m.id + '_totalt').text(m.total);
  pxCommonTotal(sel);
  sel.trigger('change');
});
// 'Hämta alltid senaste perioden' låser listan
$(document).on('change', 'input[id$=\"_senaste\"]', function() {
  var id = this.id.replace(/_senaste$/, '');
  $('#' + id).toggleClass('disabled', this.checked);
  pxVarCount(document.getElementById(id));
  Shiny.setInputValue(this.id, this.checked);
});
"
