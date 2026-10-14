/* Datos sucios a patrones reales · EH 2024 · v3.1
   Todas las cifras se leen de window.EH (data/datos.js, generado por GENERAR_DASHBOARD.R) */
(function () {
'use strict';
const E = window.EH;
if (!E) { document.body.innerHTML = '<p style="padding:40px;color:#fff">No se encontró data/datos.js. Ejecute GENERAR_DASHBOARD.R.</p>'; return; }
const T = n => (E.tablas && E.tablas[n]) || [];
const $ = s => document.querySelector(s);
const $$ = s => [...document.querySelectorAll(s)];
const el = (tag, cls, html) => { const e = document.createElement(tag); if (cls) e.className = cls; if (html != null) e.innerHTML = html; return e; };
const nf = (x, d = 0) => x == null || isNaN(x) ? '—' : Number(x).toLocaleString('en-US', { minimumFractionDigits: d, maximumFractionDigits: d });
const pc = (x, d = 1) => x == null || isNaN(x) ? '—' : (x * 100).toFixed(d) + '%';
const pp = (x, d = 1) => x == null || isNaN(x) ? '—' : (x >= 0 ? '+' : '') + (x * 100).toFixed(d) + ' pp';
const sum = a => a.reduce((s, v) => s + (+v || 0), 0);
const by = (t, k, v) => t.find(r => r[k] === v) || {};
const C = { neon: '#39ff9f', cyan: '#22e4ff', lila: '#b46cff', pink: '#ff4fa3', amber: '#ffc24d', blue: '#4d7cff', orange: '#ff8a4d', txt: '#e8eeff', mut: '#93a3d6', dim: '#5f6f9f', grid: 'rgba(36,57,122,.45)' };
const PAL = [C.neon, C.cyan, C.lila, C.pink, C.amber, C.blue, '#7dffcf', '#ff8fd0', '#9fb4ff', C.orange];
const ENF = {
  persona: { k: 'persona', l: 'A · Persona', s: 'A', c: C.pink, col: 'tasa_persona', vul: 'vul_A', mira: 'Mirando a la persona', q: '¿Su propio ingreso laboral es menor al Salario Mínimo Nacional (Bs 2,500)?',
    def: 'Se fija solo en lo que gana la persona por su trabajo. Si gana menos del salario mínimo 2024, su empleo es vulnerable, aunque su familia tenga otros ingresos.', op: 'ylab < 2500', corto: 'gana menos del salario mínimo' },
  hogar: { k: 'hogar', l: 'B · Hogar', s: 'B', c: C.cyan, col: 'tasa_hogar', vul: 'vul_B', mira: 'Mirando al hogar', q: '¿Vive en un hogar pobre según la línea oficial del INE?',
    def: 'Suma todo lo que entra al hogar y lo divide entre sus miembros. Si ese ingreso por persona no alcanza la línea de pobreza oficial, quien trabaja ahí es vulnerable.', op: 'yhogpc < z  (p0 = 1)', corto: 'vive en un hogar pobre' },
  oit: { k: 'oit', l: 'C · OIT', s: 'C', c: C.amber, col: 'tasa_oit', vul: null, mira: 'Mirando el tipo de trabajo', q: '¿Trabaja por cuenta propia o como familiar sin pago?',
    def: 'Definición internacional de la OIT: no mira el ingreso sino la forma de trabajar. Quien trabaja por cuenta propia o ayuda en el negocio familiar sin sueldo no tiene contrato ni protección.', op: 's04b_12 ∈ {3, 6}', corto: 'trabaja por cuenta propia o sin pago' } };
const DEP_ACC = { Potosi: 'Potosí' };
const dn = d => DEP_ACC[d] || d;
const ACW = { tenia: 'tenía', anio: 'año', anios: 'años', Anios: 'Años', educacion: 'educación', Educacion: 'Educación', remuneracion: 'remuneración', Administracion: 'Administración', publica: 'pública', Tecnicos: 'Técnicos',
  ganaderia: 'ganadería', Mineria: 'Minería', Construccion: 'Construcción', construccion: 'construcción', Informacion: 'Información', informacion: 'información', instruccion: 'instrucción', Potosi: 'Potosí',
  Regresion: 'Regresión', regresion: 'regresión', Logistica: 'Logística', logistica: 'logística', decision: 'decisión', Mas: 'Más', mas: 'más', Arbol: 'Árbol', arbol: 'árbol', Tamanio: 'Tamaño', estocastica: 'estocástica',
  parametrica: 'paramétrica', distribucion: 'distribución', razon: 'razón', geometricas: 'geométricas', Confirmacion: 'Confirmación', relacion: 'relación', monotona: 'monótona', Correlacion: 'Correlación',
  area: 'área', Area: 'Área', condicion: 'condición', participacion: 'participación', poblacion: 'población', numericas: 'numéricas', leido: 'leído', clasica: 'clásica', region: 'región', genero: 'género',
  Eliminacion: 'Eliminación', Multiple: 'Múltiple', tecnicas: 'técnicas', comunicacion: 'comunicación', Imputacion: 'Imputación', imputacion: 'imputación',
  Comparacion: 'Comparación', comparacion: 'comparación', segun: 'según', unico: 'único', metodo: 'método', Metodo: 'Método', categoria: 'categoría', categorias: 'categorías',
  Clasificacion: 'Clasificación', clasificacion: 'clasificación', Validacion: 'Validación', validacion: 'validación', Reconstruccion: 'Reconstrucción', reconstruccion: 'reconstrucción', Exportacion: 'Exportación',
  Prediccion: 'Predicción', prediccion: 'predicción', Interpretacion: 'Interpretación', elimino: 'eliminó', mayoria: 'mayoría', minimo: 'mínimo', maximo: 'máximo', Analisis: 'Análisis', analisis: 'análisis',
  Desagregacion: 'Desagregación', Sintesis: 'Síntesis', automatica: 'automática', Ocupacion: 'Ocupación', ocupacion: 'ocupación', patron: 'patrón', Patron: 'Patrón', Identificacion: 'Identificación', identificacion: 'identificación',
  numerico: 'numérico', Numerico: 'Numérico', estandar: 'estándar', Busqueda: 'Búsqueda', notacion: 'notación', cientifica: 'científica', Simulacion: 'Simulación', simulacion: 'simulación', exactamente: 'exactamente',
  validos: 'válidos', despues: 'después', Despues: 'Después', tambien: 'también', Ingles: 'Inglés', ultimo: 'último', ultimos: 'últimos', basica: 'básica', Basica: 'Básica', Ninguno: 'Ninguno', Tecnico: 'Técnico',
  hipotesis: 'hipótesis', Hipotesis: 'Hipótesis', estadistica: 'estadística', Estadistica: 'Estadística', grafico: 'gráfico', graficos: 'gráficos', dias: 'días', semanas: 'semanas', mediana: 'mediana', numero: 'número', Numero: 'Número',
  periodo: 'período', telefono: 'teléfono', jubilacion: 'jubilación', Jubilacion: 'Jubilación', publico: 'público', Publico: 'Público', organizacion: 'organización', economica: 'económica', economico: 'económico', tecnica: 'técnica',
  limite: 'límite', limites: 'límites', calculo: 'cálculo', criterio: 'criterio', tipico: 'típico', medico: 'médico', Medico: 'Médico', pais: 'país', Bolivia: 'Bolivia' };
const ACC = s => String(s == null ? '' : s).replace(/[A-Za-z]+/g, w => ACW[w] || w);

/* ---------------- glosario (tooltips) ---------------- */
const GL = {
  media: ['Media (promedio)', 'Se suman todos los valores y se divide entre cuántos son. Si unos pocos valores son enormes, la media sube mucho.'],
  mediana: ['Mediana', 'El valor del medio: la mitad de las personas está por debajo y la otra mitad por encima. No le afectan los extremos.'],
  moda: ['Moda', 'El valor que más se repite.'],
  minimo: ['Mínimo', 'El valor más pequeño registrado.'],
  maximo: ['Máximo', 'El valor más grande registrado.'],
  rango: ['Rango', 'Distancia entre el máximo y el mínimo.'],
  desv: ['Desviación estándar', 'Cuánto se alejan, en promedio, los valores de la media. Grande = datos muy dispersos.'],
  cv: ['Coeficiente de variación', 'La desviación dividida entre la media: permite comparar la dispersión de variables con unidades distintas.'],
  q1: ['Primer cuartil (Q1)', 'El 25% de los valores está por debajo de este número.'],
  q3: ['Tercer cuartil (Q3)', 'El 75% de los valores está por debajo de este número.'],
  asim: ['Asimetría', '0 = simétrica. Positiva = una cola larga hacia valores altos (pocos casos muy grandes), típico de los ingresos.'],
  curt: ['Curtosis', 'Qué tan “puntiaguda” es la distribución y qué tan pesadas sus colas. Alta = muchos valores extremos.'],
  decil: ['Decil', 'Si se ordena a todos de menor a mayor y se divide en 10 grupos iguales, cada corte es un decil.'],
  faltante: ['Dato faltante', 'Celda vacía. Puede ser estructural (la pregunta no correspondía) o no respuesta (debía responder y no lo hizo).'],
  atipico: ['Valor atípico', 'Un valor muy alejado del resto. Puede ser real o un error; se revisa antes de decidir.'],
  factor: ['Factor de expansión', 'Cuántas personas reales representa cada persona encuestada. Todas las tasas se calculan con él.'],
  ic: ['Intervalo de confianza 95%', 'Rango donde, con 95% de seguridad, está el valor real para todo el país.'],
  smn: ['Salario Mínimo Nacional', 'Bs 2,500 al mes en 2024 (Decreto Supremo 5154).'],
  linea: ['Línea de pobreza', 'Ingreso mínimo por persona para cubrir una canasta básica, calculado por el INE para cada zona.'],
  kappa: ['Kappa de Cohen', 'Mide si dos clasificaciones coinciden más de lo que lo harían por azar. 0 = azar, 1 = acuerdo perfecto.'],
  lift: ['Lift', 'Cuántas veces más frecuente es la vulnerabilidad en ese grupo que en el promedio. 2 = el doble.'],
  auc: ['AUC', 'Toma una persona vulnerable y otra que no: es la probabilidad de que el modelo le dé más riesgo a la vulnerable. 0.5 = azar, 1 = perfecto.'],
  cv5: ['Validación cruzada', 'Se divide el entrenamiento en 5 partes; se entrena con 4 y se prueba con la quinta, cinco veces. Así la nota no depende de la suerte.'],
  umbral: ['Umbral', 'Probabilidad a partir de la cual el modelo dice “vulnerable”. Más bajo = detecta más, pero con más falsas alarmas.'],
  imput: ['Imputación', 'Completar un dato faltante con una estimación basada en personas parecidas.'],
  ivl: ['IVL', 'Índice de Vulnerabilidad Laboral, 0 a 100: combina desempleo, subempleo, informalidad, baja educación y la tasa del enfoque.'],
  informal: ['Informalidad', 'Aquí: el trabajador no aporta a la Gestora (sistema de jubilación).'],
  kmeans: ['k-means', 'Algoritmo que agrupa a las personas más parecidas entre sí sin que nadie le diga qué buscar.'],
  logit: ['Regresión logística', 'Modelo que convierte una suma ponderada de características en una probabilidad entre 0 y 1. Sus coeficientes se pueden leer.'],
  rf: ['Random Forest', 'Cientos de árboles de decisión que votan. Suele ser preciso y robusto.'],
  gbm: ['Gradient Boosting', 'Árboles que se construyen uno tras otro, cada uno corrigiendo los errores del anterior.'],
  mincer: ['Ecuación de Mincer', 'Modelo clásico de economía: el logaritmo del ingreso depende de años de estudio y experiencia.'],
  winsor: ['Winsorizar', 'Recortar los valores extremos al límite aceptado, sin borrarlos.'],
  calib: ['Calibración', 'Si el modelo dice 30%, ¿realmente 3 de cada 10 de esas personas son vulnerables?']
};
const gl = (txt, k) => `<span class="gl" data-g="${k}">${txt}</span>`;
(function () { const tip = $('#gtip');
  document.addEventListener('mouseover', e => { const g = e.target.closest && e.target.closest('.gl'); if (!g) { tip.classList.remove('on'); return; } const d = GL[g.dataset.g]; if (!d) return; tip.innerHTML = `<b>${d[0]}</b>${d[1]}`; tip.classList.add('on'); });
  document.addEventListener('mousemove', e => { if (!tip.classList.contains('on')) return; const x = Math.min(e.clientX + 14, innerWidth - 340); tip.style.left = x + 'px'; tip.style.top = (e.clientY + 16) + 'px'; }, { passive: true });
})();

/* ---------------- fases ---------------- */
const FASES = [
  ['F01', 'Revisión de los datos', 'Comprensión de los datos', '#22e4ff', 'F01_auditoria'],
  ['F02', 'Primera mirada estadística', 'Comprensión de los datos', '#2fd3ff', 'F02_descriptivo_inferencial'],
  ['F03', 'Formatos y etiquetas', 'Preparación de los datos', '#39ff9f', 'F03_formatos'],
  ['F04', 'Duplicados', 'Preparación de los datos', '#6dff86', 'F04_duplicados'],
  ['F05', 'Datos faltantes', 'Preparación de los datos', '#a6ff5e', 'F05_faltantes_imputacion'],
  ['F06', 'Valores atípicos', 'Preparación de los datos', '#ffc24d', 'F06_outliers'],
  ['F07', 'Validación en ciclo', 'Preparación de los datos', '#ff9a52', 'F07_validacion_ciclo'],
  ['F08', 'Variables objetivo', 'Preparación de los datos', '#ff6fa8', 'F08_transformacion'],
  ['F09', 'Antes y después', 'Evaluación', '#e070ff', 'F09_descriptivo_limpio'],
  ['F10', 'Modelado', 'Modelado', '#b46cff', 'F10_modelado'],
  ['F11', 'Patrones', 'Evaluación', '#8a7dff', 'F11_patrones'],
  ['F12', 'Cierre y exportación', 'Despliegue', '#4d9cff', 'F12_cierre']
];
const FN = Object.fromEntries(FASES.map(f => [f[0], f]));
const bit = E.bitacora || [];
const bitF = k => bit.filter(r => (r.fase || '').slice(0, 3) === k);
const seg = k => { const t = ((E.log || {}).tiempos || []).find(r => r.fase === k); return t ? t.segundos : null; };

/* ---------------- plotly ---------------- */
const FONT = 'Inter, Segoe UI, sans-serif', FMONO = 'JetBrains Mono, Consolas, monospace', FHEAD = 'Space Grotesk, Segoe UI, sans-serif';
const AX = { gridcolor: C.grid, zerolinecolor: C.grid, linecolor: C.grid, tickfont: { color: C.mut }, title: { font: { color: C.mut } } };
function L(o = {}) {
  const b = { paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)', font: { family: FONT, color: C.mut, size: 12 },
    margin: { l: 56, r: 24, t: 16, b: 56 }, legend: { orientation: 'h', y: -0.2, font: { color: C.mut } }, colorway: PAL,
    hoverlabel: { bgcolor: '#0a1430', bordercolor: C.cyan, font: { color: '#fff', family: FONT } }, xaxis: { ...AX }, yaxis: { ...AX },
    transition: { duration: 700, easing: 'cubic-in-out' } };
  for (const k in o) b[k] = /^[xy]axis/.test(k) ? Object.assign({}, AX, o[k]) : o[k];
  return b;
}
const CFG = { displaylogo: false, responsive: true, modeBarButtonsToRemove: ['lasso2d', 'select2d', 'autoScale2d'], toImageButtonOptions: { format: 'png', scale: 2 } };
const P = (id, d, l) => { const n = document.getElementById(id); if (n) return Plotly.react(n, d, L(l), CFG); };
const SC3 = { bgcolor: 'rgba(0,0,0,0)', xaxis: { color: C.mut, gridcolor: C.grid, backgroundcolor: 'rgba(0,0,0,0)', showbackground: false }, yaxis: { color: C.mut, gridcolor: C.grid, showbackground: false }, zaxis: { color: C.mut, gridcolor: C.grid, showbackground: false } };
/* "crecer": dibuja en cero y anima al valor real */
function grow(id, d, l = {}, key = 'x') {
  const n = document.getElementById(id); if (!n) return;
  const ax = key === 'x' ? 'xaxis' : 'yaxis', vals = d.filter(t => t.type === 'bar').flatMap(t => (t[key] || []).filter(v => typeof v === 'number' && isFinite(v)));
  l = { ...l }; l[ax] = { ...(l[ax] || {}) };
  if (!l[ax].range && l[ax].type !== 'log' && vals.length) { const mx = Math.max(0, ...vals), mn = Math.min(0, ...vals); l[ax].range = [mn * 1.15, mx * 1.18]; }
  const z = d.map(t => t.type === 'bar' ? { ...t, [key]: (t[key] || []).map(v => typeof v === 'number' ? 0 : v), text: (t.text || []).map(() => '') } : t);
  Plotly.newPlot(n, z, L(l), CFG).then(() => setTimeout(() => Plotly.animate(n, { data: d, traces: d.map((_, i) => i) }, { transition: { duration: 900, easing: 'cubic-out' }, frame: { duration: 900, redraw: true } }), 150));
}
const io = new IntersectionObserver(es => es.forEach(e => { if (e.isIntersecting) { const f = e.target._draw; if (f) { e.target._draw = null; try { f(); } catch (err) { console.error(err); } } io.unobserve(e.target); } }), { rootMargin: '250px' });
const lazy = (id, fn) => { const n = document.getElementById(id); if (!n) return; n._draw = fn; io.observe(n); };
const REDRAW = [];
const onEnf = fn => { REDRAW.push(fn); };

/* ---------------- utilidades de interfaz ---------------- */
function table(id, cols, rows, hl) {
  const t = typeof id === 'string' ? document.getElementById(id) : id; if (!t) return;
  t.innerHTML = '<thead><tr>' + cols.map(c => `<th>${c[1]}</th>`).join('') + '</tr></thead><tbody>' +
    rows.map(r => `<tr class="${hl && hl(r) ? 'hl' : ''}">` + cols.map(c => { const v = c[2] ? c[2](r[c[0]], r) : r[c[0]]; return `<td class="${typeof r[c[0]] === 'number' ? 'n' : ''}">${v == null ? '—' : v}</td>`; }).join('') + '</tr>').join('') + '</tbody>';
  return t;
}
function segBtns(id, items, onSel, first) {
  const s = document.getElementById(id); if (!s) return; s.innerHTML = '';
  items.forEach((it, i) => { const b = el('button', i === (first || 0) ? 'on' : '', it[1]); b.dataset.k = it[0]; b.onclick = () => { s.querySelectorAll('button').forEach(x => x.classList.remove('on')); b.classList.add('on'); onSel(it[0]); }; s.appendChild(b); });
  onSel(items[first || 0][0]);
}
const segSet = (id, k) => { const s = document.getElementById(id); if (!s) return; const b = [...s.querySelectorAll('button')].find(x => x.dataset.k === k); if (b) b.click(); };
const chip = (v, l, c) => `<div class="chip tilt" style="--c:${c || C.neon}"><div class="v">${v}</div><div class="l">${l}</div></div>`;
const neon = (txt, c) => `<span class="neon" style="--c:${c || C.neon}">${txt}</span>`;
const setNeon = (id, arr) => { const n = document.getElementById(id); if (n) n.innerHTML = arr.filter(Boolean).map(a => neon(a[0], a[1])).join(''); };
/* tarjetas que se inclinan con el cursor */
document.addEventListener('pointermove', e => {
  const c = e.target.closest && e.target.closest('.card'); if (c) { const r = c.getBoundingClientRect(); c.style.setProperty('--mx', (e.clientX - r.left) + 'px'); c.style.setProperty('--my', (e.clientY - r.top) + 'px'); }
  const t = e.target.closest && e.target.closest('.tilt,.kpi,.mys,.cu,.oc,.bn,.st'); if (!t) return; const r = t.getBoundingClientRect();
  const x = (e.clientX - r.left) / r.width - .5, y = (e.clientY - r.top) / r.height - .5;
  t.style.transform = `perspective(700px) rotateY(${x * 9}deg) rotateX(${-y * 9}deg) translateY(-2px)`; t._tl = true;
}, { passive: true });
document.addEventListener('pointerout', e => { const t = e.target.closest && e.target.closest('.tilt,.kpi,.mys,.cu,.oc,.bn,.st'); if (t && !t.contains(e.relatedTarget)) t.style.transform = ''; }, { passive: true });

/* ---------------- figuras / lightbox ---------------- */
const G = E.graficos || [];
let LB = [], LBi = 0;
function thumb(g, list) {
  const f = el('figure', 'th'), fz = FN[g.fase] || [g.fase, ''];
  f.innerHTML = `<img loading="lazy" src="img/${g.archivo}" alt="${g.titulo}"><figcaption><b>Fase ${g.fase} · ${fz[1]}</b>${ACC(g.titulo)}</figcaption>`;
  f.onclick = () => { LB = list; LBi = list.indexOf(g); showLB(); $('#lb').classList.add('on'); };
  return f;
}
function showLB() { const g = LB[LBi], fz = FN[g.fase] || [g.fase, '']; $('#lbi').src = 'img/' + g.archivo; $('#lbc').innerHTML = `<b>FASE ${g.fase} · ${fz[1].toUpperCase()}</b>${ACC(g.titulo)}`; }
$('#lbx').onclick = () => $('#lb').classList.remove('on');
$('#lbp').onclick = () => { LBi = (LBi - 1 + LB.length) % LB.length; showLB(); };
$('#lbn').onclick = () => { LBi = (LBi + 1) % LB.length; showLB(); };
$('#lb').onclick = e => { if (e.target.id === 'lb') $('#lb').classList.remove('on'); };
document.addEventListener('keydown', e => { if (!$('#lb').classList.contains('on')) return; if (e.key === 'Escape') $('#lbx').click(); if (e.key === 'ArrowLeft') $('#lbp').click(); if (e.key === 'ArrowRight') $('#lbn').click(); });

/* ---------------- código: resaltado y extractos ---------------- */
const SCR = E.scripts || {};
const esc = s => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
function hl1(line) {
  let h = esc(line), c = '';
  const i = h.indexOf('#'); if (i >= 0 && !/["'][^"']*#/.test(h.slice(0, i))) { c = `<span class="rc">${h.slice(i)}</span>`; h = h.slice(0, i); }
  h = h.replace(/("[^"]*")/g, '<span class="rs">$1</span>').replace(/\b(function|if|else|for|while|repeat|return|in|next|break|TRUE|FALSE|NULL|NA)\b/g, '<span class="rk">$1</span>')
    .replace(/\b([A-Za-z_.][A-Za-z0-9_.]*)(?=\()/g, '<span class="rf">$1</span>').replace(/\b(\d+(\.\d+)?)\b/g, '<span class="rn">$1</span>');
  return h + c;
}
const hlR = src => src.split('\n').map(l => `<span class="ln">${hl1(l)}</span>`).join('');
/* extracto: anclas = [[texto, lineas_antes, lineas_despues], ...] */
function snip(key, anchors) {
  const src = SCR[key]; if (!src) return null; const Ls = src.split('\n'); let R = [];
  anchors.forEach(([t, b = 0, a = 0]) => { const i = Ls.findIndex(l => l.includes(t)); if (i >= 0) R.push([Math.max(0, i - b), Math.min(Ls.length - 1, i + a)]); });
  if (!R.length) return null;
  R.sort((x, y) => x[0] - y[0]); const M = [R[0].slice()]; R.slice(1).forEach(r => { const m = M[M.length - 1]; if (r[0] <= m[1] + 1) m[1] = Math.max(m[1], r[1]); else M.push(r.slice()); });
  const html = M.map(r => Ls.slice(r[0], r[1] + 1).map((l, j) => `<span class="ln"><i>${r[0] + j + 1}</i>${hl1(l)}</span>`).join('')).join('<span class="sep">        ⋯</span>');
  return { html, lab: M.map(r => r[0] === r[1] ? r[0] + 1 : `${r[0] + 1}–${r[1] + 1}`).join(', '), first: Ls[M[0][0]] };
}
function codeBox(blocks, cap) {
  return blocks.map(([key, anchors]) => { const s = snip(key, anchors); if (!s) return '';
    return `<div class="codebox" style="margin-top:10px"><div class="cf"><span>scripts/${key}.R · líneas ${s.lab}</span><button data-open="${key}" data-hot="${esc(s.first.trim().slice(0, 40)).replace(/"/g, '&quot;')}">Abrir completo ↗</button></div><pre>${s.html}</pre></div>`; }).join('') + (cap ? `<div class="src">${cap}</div>` : '');
}
document.addEventListener('click', e => { const b = e.target.closest && e.target.closest('[data-open]'); if (b) openDock(b.dataset.open, b.dataset.hot); });
const dsel = $('#dock-sel');
dsel.innerHTML = Object.keys(SCR).sort().map(k => `<option value="${k}">${k}.R</option>`).join('');
let dockKey = null;
function openDock(key, hot) {
  if (!SCR[key]) return; const d = $('#dock'); d.classList.add('on'); d.classList.remove('min');
  if (dockKey !== key) { $('#dock-code').innerHTML = hlR(SCR[key]); dockKey = key; dsel.value = key; $('#dock-f').textContent = `scripts/${key}.R · ${SCR[key].split('\n').length} líneas`; }
  if (hot) { const lines = SCR[key].split('\n'); let j = lines.findIndex(l => l.includes(hot)); if (j < 0) j = 0;
    const ln = $('#dock-code').children; [...ln].forEach(x => x.classList.remove('hot')); if (ln[j]) { ln[j].classList.add('hot'); $('#dock-code').scrollTop = ln[j].offsetTop - 60; } }
}
dsel.onchange = () => openDock(dsel.value);
$('#dock-x').onclick = () => $('#dock').classList.remove('on');
$('#dock-min').onclick = () => $('#dock').classList.toggle('min');
$('#opendock').onclick = () => openDock(dockKey || 'F01_auditoria');

/* ================= DATOS GLOBALES ================= */
const cfg = E.cfg || {}, SMN = cfg.smn || 2500;
const b0 = bit[0] || {};
const filas0 = b0.filas, cols0 = b0.columnas;
const dic = T('F12_diccionario_final'), dic0 = T('F01_diccionario_verificado');
const colsF = dic.length || null;
const main = bit.filter(r => r.filas === filas0 && r.columnas >= cols0);
const filasF = main.length ? main[main.length - 1].filas : filas0;
const vddi = T('F01_validacion_contra_ddi');
const ylabV = by(vddi, 'variable', 'ylab');
const pobOf = T('F01_pobreza_vs_oficial'), pob0 = pobOf[0] || {}, pobX = pobOf[1] || {};
const FMT = T('F01_revision_formato'), PRU = T('F01_prueba_formato_resumen'), PRM = T('F01_prueba_formato_medias'), PRE_EJ = T('F01_prueba_formato_ejemplos');
const nFmt = sum(FMT.map(r => (r.digitos_agrupados || 0) + (r.cientifica_con_coma || 0)));
const TE = T('F08_tasas_por_enfoque');
const te = k => by(TE, 'enfoque', k);
const cuad = T('F08_cuadrante_persona_hogar');
const conc = T('F08_concordancia_enfoques');
const kAB = conc.find(r => r.enfoque_1 === 'persona' && r.enfoque_2 === 'hogar') || {};
const dep = T('F11_tasa_vulnerable_dep');
const zon = T('F11_tasa_vulnerable_zona'), sx = T('F11_tasa_vulnerable_sexo');
const ivlE = T('F08_ivl_departamento_enfoques');
const freq = T('F02_frecuencias_cualitativas');
const pobTot = by(vddi, 'variable', 'factor').total_exp_calculado || sum(freq.filter(r => r.variable === 'sexo').map(r => r.poblacion));
const hog = by(dic0, 'variable', 'folio').n_unicos;
const CUR = T('F02_curiosidades'), cur = k => by(CUR, 'clave', k);
const M0 = T('F02_medidas_resumen_sin_tratar'), M1 = T('F09_medidas_resumen_limpio'), HAD = T('F09_histogramas_antes_despues');
const CVR = T('F10_cv_resumen'), TM = T('F10_test_metricas');
const best = o => CVR.filter(r => r.objetivo === o).sort((a, b) => b.auc_media - a.auc_media)[0] || {};
const naTot = sum(dic0.map(r => r.n_na));
const HQ = [[/hombres y mujeres/, '¿Hombres y mujeres ganan lo mismo?'], [/log del ingreso medio es igual en los 9/, '¿Se gana lo mismo en los 9 departamentos?'], [/distribucion del ingreso es igual en los 9/, '¿Se gana lo mismo en los 9 departamentos? (prueba sin supuestos)'],
  [/pobreza es independiente del area/, '¿La pobreza es igual en el área urbana y rural?'], [/actividad es independiente del sexo/, '¿Hombres y mujeres trabajan en la misma proporción?'], [/estudio y log del ingreso/, '¿Más años de estudio van con más ingreso?'], [/estudio e ingreso/, '¿Más años de estudio van con más ingreso? (prueba sin supuestos)']];
const hq = t => (HQ.find(h => h[0].test(t)) || [0, ACC(t)])[1];
/* vacio estructural (pregunta con filtro o que no aplica) vs no respuesta dentro de quien debia responder */
const naSplit = r => /^Estructural|^Mayormente/.test(r.tipo_faltante || '') ? [r.n_na || 0, 0] : [r.na_fuera_dominio || 0, r.na_en_dominio || 0];
let ENFQ = 'persona';
const enfs = () => ENFQ === 'comparar' ? [ENF.persona, ENF.hogar] : [ENF[ENFQ]];
const enfAB = () => ENFQ === 'hogar' ? ENF.hogar : ENF.persona;
const enf1 = () => ENFQ === 'comparar' ? ENF.persona : ENF[ENFQ];
const EQ = (E.equipo || []).map(r => r.nombre).filter(Boolean);

/* ================= HERO ================= */
$('#lead').innerHTML = `${nf(filas0)} personas de ${nf(hog)} hogares respondieron la Encuesta de Hogares 2024 del INE. Representan a <b>${nf(pobTot / 1e6, 1)} millones de bolivianos</b>. Entre sus ${nf(cols0)} preguntas se esconde una pregunta incómoda: <b>de quienes trabajan, ¿cuántos lo hacen en condiciones vulnerables?</b> La respuesta cambia según dónde se mire. Toca cada tarjeta.`;
const ROTQ = [
  ['horas_max', r => `Alguien declaró trabajar <b>${nf(r.valor)} horas</b> en una semana… que solo tiene 168.`],
  ['ylab_max', r => `El ingreso laboral más alto de la encuesta es <b>Bs ${nf(r.valor)}</b> al mes.`],
  ['sin_pago', r => `<b>${nf(r.valor)} personas</b> trabajan, pero no reciben ningún pago.`],
  ['horas_80', r => `<b>${nf(r.valor)} personas</b> trabajan más de 80 horas por semana.`],
  ['joven', r => `El trabajador más joven de la encuesta tiene <b>${nf(r.valor)} años</b>.`],
  ['mayor', r => `El trabajador de mayor edad tiene <b>${nf(r.valor)} años</b>.`],
  ['hogar', r => `El hogar más numeroso tiene <b>${nf(r.valor)} personas</b>.`],
  ['vacias', r => `<b>${pc(r.valor / (filas0 * cols0), 0)}</b> de las celdas del archivo están vacías… y casi todas tienen una explicación.`]].filter(q => CUR.some(r => r.clave === q[0]));
(function () { let i = 0; const n = $('#rotq'); if (!ROTQ.length) { $('.rotq').style.display = 'none'; return; }
  const show = () => { const q = ROTQ[i % ROTQ.length]; n.classList.add('out'); setTimeout(() => { n.innerHTML = q[1](cur(q[0])); n.classList.remove('out'); }, 450); i++; };
  show(); setInterval(show, 4200); })();
$('#reveal3').innerHTML = Object.values(ENF).map(e => { const t = te(e.k);
  return `<div class="mys" data-k="${e.k}" style="--c:${e.c}"><div class="in3">
    <div class="fa"><span class="t">${e.mira}</span><span class="q">?</span><span class="s">${e.q}</span></div>
    <div class="ba"><span class="t">Enfoque ${e.s} · ${e.mira.toLowerCase()}</span><span class="v">${pc(t.tasa)}</span><span class="s">de los ocupados de 19+ ${e.corto} · ${nf(t.poblacion_vulnerable / 1e6, 2)} millones</span></div></div></div>`; }).join('');
$$('.mys').forEach(m => m.onclick = () => { m.classList.toggle('open'); if (m.classList.contains('open') && ENFQ !== m.dataset.k) setEnf(m.dataset.k); });
const K = [['Personas encuestadas', filas0, 0, '', `${nf(hog)} hogares de los 9 departamentos`, C.neon], ['Preguntas por persona', cols0, 0, '', 'columnas del archivo persona.csv', C.cyan],
  ['Bolivianos representados', pobTot / 1e6, 1, ' M', `con el ${gl('factor de expansión', 'factor')} del INE`, C.lila], ['Filas eliminadas', 0, 0, '', 'regla de oro: no se borra nada, se marca y se documenta', C.pink]];
$('#kpis').innerHTML = K.map(k => `<div class="kpi" style="--c:${k[5]}"><div class="l">${k[0]}</div><div class="v" data-to="${k[1]}" data-d="${k[2]}" data-s="${k[3]}">0</div><div class="s">${k[4]}</div></div>`).join('');
function countUp(n) { const to = +n.dataset.to, d = +n.dataset.d, s = n.dataset.s || '', t0 = performance.now(), D = 1700;
  const step = t => { const p = Math.min(1, (t - t0) / D), e = 1 - Math.pow(1 - p, 3); n.textContent = nf(to * e, d) + s; if (p < 1) requestAnimationFrame(step); }; requestAnimationFrame(step); }
setTimeout(() => $$('#kpis .kpi .v').forEach(countUp), 300);
$('#team').innerHTML = EQ.length ? `<span class="lb">EQUIPO</span>` + EQ.map(n => `<span class="p"><i>◆</i>${n}</span>`).join('') : '';
(function net() {
  const cv = $('#net'), cx = cv.getContext('2d'); let W, H, pts = [], mx = -1e9, my = -1e9;
  const rs = () => { W = cv.width = cv.offsetWidth * devicePixelRatio; H = cv.height = cv.offsetHeight * devicePixelRatio; pts = Array.from({ length: Math.min(120, Math.floor(W * H / 24000)) }, () => ({ x: Math.random() * W, y: Math.random() * H, vx: (Math.random() - .5) * .35, vy: (Math.random() - .5) * .35, c: [C.neon, C.cyan, C.lila, C.pink][Math.floor(Math.random() * 4)] })); };
  rs(); addEventListener('resize', rs);
  cv.parentElement.addEventListener('mousemove', e => { const r = cv.getBoundingClientRect(); mx = (e.clientX - r.left) * devicePixelRatio; my = (e.clientY - r.top) * devicePixelRatio; });
  (function loop() { cx.clearRect(0, 0, W, H); const R = 140 * devicePixelRatio;
    for (const p of pts) { const dx = p.x - mx, dy = p.y - my, d = Math.hypot(dx, dy); if (d < 160 * devicePixelRatio) { p.x += dx / d * 1.2; p.y += dy / d * 1.2; }
      p.x += p.vx; p.y += p.vy; if (p.x < 0 || p.x > W) p.vx *= -1; if (p.y < 0 || p.y > H) p.vy *= -1; }
    for (let i = 0; i < pts.length; i++) for (let j = i + 1; j < pts.length; j++) { const a = pts[i], b = pts[j], d = Math.hypot(a.x - b.x, a.y - b.y); if (d < R) { cx.strokeStyle = `rgba(34,228,255,${(1 - d / R) * .22})`; cx.lineWidth = 1; cx.beginPath(); cx.moveTo(a.x, a.y); cx.lineTo(b.x, b.y); cx.stroke(); } }
    for (const p of pts) { cx.fillStyle = p.c; cx.shadowColor = p.c; cx.shadowBlur = 10; cx.beginPath(); cx.arc(p.x, p.y, 1.9 * devicePixelRatio, 0, 7); cx.fill(); cx.shadowBlur = 0; }
    requestAnimationFrame(loop); })();
})();

/* ================= ENFOQUE GLOBAL ================= */
(function () {
  const box = $('#enf');
  [...Object.values(ENF), { k: 'comparar', l: 'A vs B', c: C.lila }].forEach(e => { const b = el('button', e.k === ENFQ ? 'on' : '', e.l); b.style.setProperty('--c', e.c); b.dataset.k = e.k;
    b.onclick = () => setEnf(e.k); box.appendChild(b); });
})();
function setEnf(k) { ENFQ = k; $$('#enf button').forEach(b => b.classList.toggle('on', b.dataset.k === k));
  $$('#lens .card').forEach(c => c.classList.toggle('on', c.dataset.k === k || (k === 'comparar' && c.dataset.k !== 'oit')));
  $$('.mys').forEach(m => { if (m.dataset.k === k || (k === 'comparar' && m.dataset.k !== 'oit')) m.classList.add('open'); });
  REDRAW.forEach(f => { try { f(); } catch (err) { console.error(err); } }); }

/* ================= 01 LA PREGUNTA ================= */
$('#lens').innerHTML = Object.values(ENF).map(e => { const t = te(e.k);
  return `<div class="card" data-k="${e.k}" style="--c:${e.c}"><span class="tag" style="--c:${e.c}">Enfoque ${e.s}</span><h3 style="margin-top:12px">${e.mira}</h3>
    <p><b>${e.q}</b></p><p style="margin-top:6px">${e.def}</p><span class="op">${e.op}</span><div class="big" style="--c:${e.c}">${pc(t.tasa)}</div>
    <p class="src" style="margin-top:6px">${nf(t.poblacion_vulnerable / 1e6, 2)} millones de ocupados de 19+ · ${gl('IC 95%', 'ic')} ${pc(t.li)}–${pc(t.ls)}</p></div>`; }).join('');
$$('#lens .card').forEach(c => c.onclick = () => setEnf(c.dataset.k));
document.querySelector('#lens .card[data-k="persona"]').classList.add('on');
(function () { const z = by(vddi, 'variable', 'z').media_oficial || 1000, pc1 = (1800 + 5500) / 4;
  $('#story').innerHTML = `<div class="who"><div class="ic">👩🏽‍💼</div><b style="font-family:var(--fh);font-size:18px">Ana, 34 años</b><p style="margin-top:6px">Vende ropa por <b>cuenta propia</b> en un mercado de El Alto y gana <b>Bs 1,800</b> al mes. Su esposo es profesor y gana <b>Bs 5,500</b>. Viven 4 personas.</p></div>
    <div class="res" style="--c:${C.pink}"><span class="t">A · Persona</span><div class="a">Vulnerable</div><p>Bs 1,800 es menos que el ${gl('salario mínimo', 'smn')} (Bs ${nf(SMN)}).</p></div>
    <div class="res" style="--c:${C.cyan}"><span class="t">B · Hogar</span><div class="a">No vulnerable</div><p>Su hogar tiene Bs ${nf(pc1)} por persona: más que la ${gl('línea de pobreza', 'linea')} (en promedio Bs ${nf(z)}).</p></div>
    <div class="res" style="--c:${C.amber}"><span class="t">C · OIT</span><div class="a">Vulnerable</div><p>Trabaja por cuenta propia: sin contrato ni seguro.</p></div>`; })();
lazy('ch-embudo', () => {
  const cond = T('F01_condact_vs_pea_pei'), ocu = sum(cond.filter(r => r.condact === 1).map(r => r.n)), pet = sum(cond.filter(r => r.condact != null).map(r => r.n));
  const st = [['Personas encuestadas', filas0, pobTot], ['Con edad de trabajar (7+)', pet, null], ['Trabajan (ocupados)', ocu, null], ['Ocupados de 19 años o más', te('persona').n_muestra, te('persona').poblacion]];
  P('ch-embudo', [{ type: 'funnel', y: st.map(s => s[0]), x: st.map(s => s[1]), text: st.map(s => nf(s[1]) + (s[2] ? ` (${nf(s[2] / 1e6, 2)} M)` : '')), textinfo: 'text', textfont: { color: '#02040c', family: FHEAD, size: 14 },
    marker: { color: [C.cyan, C.lila, C.pink, C.neon], line: { color: '#030714', width: 2 } }, connector: { fillcolor: 'rgba(34,228,255,.08)' }, hovertemplate: '%{y}: %{x:,}<extra></extra>' }],
    { margin: { l: 170, r: 20, t: 10, b: 20 }, yaxis: { tickfont: { color: C.txt } } });
});
lazy('ch-lens', () => {
  const R = ['persona', 'hogar', 'oit'].map(k => ({ ...te(k), e: ENF[k] }));
  const draw = () => grow('ch-lens', [{ type: 'bar', orientation: 'h', y: R.map(r => r.e.l), x: R.map(r => r.tasa), marker: { color: R.map(r => r.e.c), opacity: R.map(r => enfs().includes(r.e) ? 1 : .35), line: { color: '#030714', width: 1 } },
    error_x: { type: 'data', symmetric: false, array: R.map(r => r.ls - r.tasa), arrayminus: R.map(r => r.tasa - r.li), color: C.txt, thickness: 1.5, width: 6 },
    text: R.map(r => pc(r.tasa)), textposition: 'inside', insidetextanchor: 'start', textfont: { color: '#02040c', family: FHEAD, size: 15 }, hovertemplate: '%{y}: %{x:.1%}<extra></extra>' }],
    { margin: { l: 100, r: 30, t: 10, b: 40 }, xaxis: { tickformat: '.0%', range: [0, .8] }, yaxis: { autorange: 'reversed', tickfont: { color: C.txt } },
      shapes: [{ type: 'line', x0: pob0.oficial_ine, x1: pob0.oficial_ine, yref: 'paper', y0: 0, y1: 1, line: { color: C.mut, dash: 'dot' } }],
      annotations: [{ x: pob0.oficial_ine, yref: 'paper', y: 1.04, text: `pobreza oficial ${pc(pob0.oficial_ine)} (toda la población)`, showarrow: false, font: { size: 10, color: C.mut } }] });
  draw(); onEnf(draw);
  setNeon('lens-neon', [[`Coinciden A y B: ${gl('kappa', 'kappa')} <b>${nf(kAB.kappa, 2)}</b>`, C.lila], [`A − B <b>${pp(te('persona').tasa - te('hogar').tasa)}</b>`, C.pink], [`C − A <b>${pp(te('oit').tasa - te('persona').tasa)}</b>`, C.amber]]);
});
(function () { const ocAll = T('F05_experimento_sin_pago'), tB = te('hogar');
  $('#faq').innerHTML = `<div class="qq" style="--c:${C.cyan}"><b>¿Por qué B da ${pc(tB.tasa)} si la pobreza oficial es ${pc(pob0.oficial_ine)}?</b>Porque miran a gente distinta. La cifra oficial incluye a toda la población: niños y adultos mayores, que viven más en hogares pobres. B solo mira a los ocupados de 19+. Con los mismos datos, la pobreza de toda la población da <b>${pc(pob0.calculada)}</b>, igual a la oficial.</div>
    <div class="qq" style="--c:${C.amber}"><b>¿Por qué C da ${pc(te('oit').tasa)}?</b>La OIT mide la forma de trabajar, no el ingreso: casi la mitad de los ocupados trabaja por cuenta propia. Por eso en reportes internacionales Bolivia aparece con un “empleo vulnerable” entre 40% y 60%, según el año y la población.</div>
    <div class="qq" style="--c:${C.lila}"><b>¿Cuál es la correcta?</b>Las tres. Cada una responde una pregunta distinta. Lo interesante es dónde <b>no coinciden</b>: ahí están las personas que una sola mirada dejaría fuera (sección 07).</div>`; })();

/* ================= 02 LOS DATOS ================= */
(function () {
  const vac80 = dic0.filter(r => r.pct_na > 80).length, tipos = new Set(dic0.map(r => r.tipo_estadistico)).size;
  $('#kpis-datos').innerHTML = [['Celdas en total', nf(filas0 * cols0), `${nf(filas0)} filas × ${nf(cols0)} columnas`, C.cyan], ['Celdas vacías', pc(naTot / (filas0 * cols0)), `${nf(naTot)} celdas`, C.pink],
    ['Columnas casi vacías', nf(vac80), 'con más de 80% de vacíos: se analizan, no se borran', C.amber], ['Tipos de variable', nf(tipos), 'nominal, ordinal, discreta, continua, fecha…', C.lila]]
    .map(k => `<div class="kpi" style="--c:${k[3]}"><div class="l">${k[0]}</div><div class="v">${k[1]}</div><div class="s">${k[2]}</div></div>`).join('');
})();
lazy('ch-tipos', () => { const g = {}; dic0.forEach(r => { const t = ACC(r.tipo_estadistico), s = ACC(r.seccion).replace(/^\d+ /, ''); g[t] = g[t] || {}; g[t][s] = (g[t][s] || 0) + 1; });
  const ids = [], lab = [], par = [], val = [], hov = [];
  Object.entries(g).forEach(([t, ss]) => { ids.push(t); lab.push(t); par.push(''); val.push(sum(Object.values(ss))); hov.push('');
    Object.entries(ss).forEach(([s, n]) => { ids.push(t + '/' + s); lab.push(s); par.push(t); val.push(n); hov.push(dic0.filter(r => ACC(r.tipo_estadistico) === t && ACC(r.seccion).replace(/^\d+ /, '') === s).slice(0, 4).map(r => r.variable).join(', ')); }); });
  P('ch-tipos', [{ type: 'sunburst', ids, labels: lab, parents: par, values: val, branchvalues: 'total', customdata: hov, hovertemplate: '<b>%{label}</b><br>%{value} columnas<br>%{customdata}<extra></extra>',
    texttemplate: ids.map((x, i) => val[i] / cols0 > .035 ? '%{label}' : ''), marker: { colors: ids.map((x, i) => PAL[Object.keys(g).indexOf(x.split('/')[0]) % PAL.length]), line: { color: '#030714', width: 2 } }, insidetextfont: { color: '#02040c', family: FHEAD }, insidetextorientation: 'horizontal', leaf: { opacity: .75 } }],
    { margin: { l: 0, r: 0, t: 0, b: 0 } }); });
lazy('ch-vacios', () => { const F5 = T('F05_clasificacion_faltantes'), g = {};
  F5.forEach(r => { const s = ACC(r.seccion).replace(/^\d+ /, ''), k = naSplit(r); g[s] = g[s] || [0, 0, 0]; g[s][0] += k[0]; g[s][1] += k[1]; g[s][2] += filas0; });
  const S = Object.keys(g).sort((a, b) => (g[a][0] + g[a][1]) / g[a][2] - (g[b][0] + g[b][1]) / g[b][2]);
  grow('ch-vacios', [{ type: 'bar', orientation: 'h', name: 'No le correspondía responder', y: S, x: S.map(s => g[s][0] / g[s][2]), marker: { color: C.lila }, hovertemplate: '%{y}: %{x:.1%} estructural<extra></extra>' },
    { type: 'bar', orientation: 'h', name: 'Debía responder y no lo hizo', y: S, x: S.map(s => g[s][1] / g[s][2]), marker: { color: C.pink }, hovertemplate: '%{y}: %{x:.1%} sin respuesta<extra></extra>' }],
    { barmode: 'stack', margin: { l: 190, r: 20, t: 10, b: 50 }, xaxis: { tickformat: '.0%', range: [0, 1] }, yaxis: { tickfont: { color: C.txt } }, legend: { orientation: 'h', y: -0.18 } }); });
(function () { const CC = [C.amber, C.pink, C.neon, C.cyan, C.lila, C.blue, C.orange, '#7dffcf', '#ff8fd0', '#9fb4ff'];
  const U = { horas: 'horas a la semana', personas: 'personas', 'Bs al mes': 'Bs al mes', anios: 'años', celdas: 'celdas' };
  const FZ = { F04: 'F04 · duplicados', F05: 'F05 · faltantes', F06: 'F06 · atípicos', F07: 'F07 · validación', F08: 'F08 · variables nuevas' };
  const QUE = { F05: 'Se clasifica: estructural o no respuesta.', F06: 'Se conserva y se marca como atípico; los modelos usan una versión recortada.', F07: 'Es imposible: vuelve como vacío y se completa con personas parecidas.', F04: 'Se verifica que no sea un hogar duplicado.', F08: 'Se conserva: es real y se analiza aparte.' };
  $('#cur').innerHTML = CUR.map((r, i) => { const v = r.unidad === 'Bs al mes' ? 'Bs ' + nf(r.valor) : nf(r.valor, r.valor < 10 && r.valor % 1 ? 2 : 0);
    return `<div class="cu" style="--c:${CC[i % CC.length]}"><div class="in3"><div class="fa"><span class="t">${ACC(r.titulo)}</span><span class="v">${v}</span><span class="u">${U[r.unidad] || ACC(r.unidad)}</span></div>
      <div class="ba"><b>${ACC(r.titulo)}</b><span>${ACC(r.detalle)}</span><span>${QUE[r.se_trata_en] || ''}</span><span class="tag" style="--c:${CC[i % CC.length]}">${FZ[r.se_trata_en] || r.se_trata_en}</span></div></div></div>`; }).join('');
  $$('.cu').forEach(c => c.onclick = () => c.classList.toggle('open')); })();
(function () { const tA = sum(PRU.map(r => r.alterados)), tR = sum(PRU.map(r => r.recuperados_exactos)), my = by(PRM, 'variable', 'ylab');
  $('#fmt-txt').innerHTML = `En el trabajo diario, un CSV descargado puede abrirse o guardarse con la configuración regional de la computadora (coma para decimales, punto para miles). Sin avisar, <b>4524.85</b> se convierte en <b>“452.485.009.765.625”</b> y una lectura directa entiende <b>452.485</b>. Por eso la fase 01 revisa el formato de cada número antes de analizar (en este archivo: <b>${nf(nFmt)}</b> valores con problema en ${FMT.length} variables) y deja listo un tratamiento previo, probado con una simulación.`;
  setNeon('fmt-neon', [[`Simulación en <b>${nf(by(PRU, 'variable', 'factor').n_valores)}</b> personas (4 de cada 10 hogares)`, C.amber], [`<b>${nf(tA)}</b> valores alterados`, C.pink], [`<b>${pc(tR / tA, 2)}</b> recuperados exactos`, C.neon],
    [`Ingreso medio: real <b>Bs ${nf(my.media_real)}</b> · lectura directa <b>Bs ${nf(my.media_lectura_directa)}</b> · tratado <b>Bs ${nf(my.media_con_tratamiento)}</b>`, C.cyan]]);
  const VNm = { ylab: 'Ingreso laboral', yhogpc: 'Ingreso por persona', factor: 'Factor', z: 'Línea de pobreza' };
  const draw = anim => { const R = PRE_EJ.filter((r, i) => i % 4 === 0).concat(PRE_EJ.filter((r, i) => i % 4 === 1)).slice(0, 6);
    $('#fmt-demo').innerHTML = '<div class="h">Valor real</div><div class="h">Cómo se vería</div><div class="h">Lectura directa</div><div class="h">Con tratamiento</div>' +
      R.map((r, i) => `<div class="o" title="${VNm[r.variable] || r.variable}">${nf(r.valor_real, 2)}</div><div class="x">${anim ? '' : '"' + r.como_se_veria + '"'}</div><div class="b">${anim ? '' : nf(r.lectura_directa, 3)}</div><div class="r" id="fr${i}">${anim ? '░░░░' : nf(r.con_tratamiento, 2)}</div>`).join('');
    if (anim) R.forEach((r, i) => { const row = $$('#fmt-demo .x')[i], bad = $$('#fmt-demo .b')[i], ok = document.getElementById('fr' + i); let k = 0;
      setTimeout(() => { row.textContent = '"' + r.como_se_veria + '"'; }, 250 + i * 260); setTimeout(() => { bad.textContent = nf(r.lectura_directa, 3); }, 600 + i * 260);
      setTimeout(() => { ok.classList.add('scr'); const t = setInterval(() => { k++; ok.textContent = (Math.random() * r.valor_real * 2).toFixed(2); if (k > 12) { clearInterval(t); ok.classList.remove('scr'); ok.textContent = nf(r.con_tratamiento, 2); } }, 55); }, 900 + i * 260); }); };
  draw(false); $('#fmt-run').onclick = () => draw(true); })();

/* ================= 03 PRIMERA MIRADA ================= */
const VN = { niv_ed_g: 'Nivel educativo', edad: 'Edad', aestudio: 'Años de estudio', tothrs: 'Horas de trabajo (semana)', phrs: 'Horas en el trabajo principal', shrs: 'Horas en el trabajo secundario', ylab: 'Ingreso laboral (Bs/mes)', yhogpc: 'Ingreso por persona del hogar', yper: 'Ingreso personal total', ynolab: 'Ingreso no laboral', yhog: 'Ingreso del hogar', factor: 'Factor de expansión' };
const UN = { edad: 'años', aestudio: 'años', tothrs: 'h', phrs: 'h', shrs: 'h', ylab: 'Bs', yhogpc: 'Bs', yper: 'Bs', ynolab: 'Bs', yhog: 'Bs', factor: 'personas' };
const VORD = ['ylab', 'yhogpc', 'edad', 'aestudio', 'tothrs', 'yper', 'ynolab', 'yhog', 'phrs', 'factor'].filter(v => M0.some(r => r.variable === v));
const fv = (v, x, d) => x == null || isNaN(x) ? '—' : (UN[v] === 'Bs' ? 'Bs ' + nf(x, d == null ? 0 : d) : nf(x, d == null ? (Math.abs(x) < 100 && x % 1 ? 1 : 0) : d));
function histOf(v, col) { const H = HAD.filter(r => r.variable === v), lg = (H[0] || {}).escala === 'log10'; return { lg, x: H.map(r => (r.desde + r.hasta) / 2), w: H.map(r => r.hasta - r.desde), y: H.map(r => r[col]), cd: H.map(r => lg ? [Math.pow(10, r.desde), Math.pow(10, r.hasta)] : [r.desde, r.hasta]) }; }
const LGX = { tickvals: [0, 1, 2, 3, 4, 5], ticktext: ['1', '10', '100', '1,000', '10,000', '100,000'] };
const xpos = (h, v) => h.lg ? Math.log10(Math.max(v, 1e-9)) : v;
let V0 = 'ylab';
function showV0(v) { V0 = v; $$('#var0 button').forEach(b => b.classList.toggle('on', b.dataset.v === v));
  const a = by(M0, 'variable', v), u = UN[v];
  const S = [['n', 'Datos válidos', nf(a.n), `personas con dato`, C.cyan], ['falt', gl('Faltantes', 'faltante'), nf(a.n_na), `${pc(a.n_na / (a.n + a.n_na))} del total`, C.pink],
    ['min', gl('Mínimo', 'minimo'), fv(v, a.minimo), 'el valor más bajo', C.blue], ['max', gl('Máximo', 'maximo'), fv(v, a.maximo), a.maximo > a.q3 + 3 * a.iqr ? '⚠ muy lejos del resto' : 'el valor más alto', C.amber],
    ['media', gl('Media', 'media'), fv(v, a.media), 'el promedio', C.neon], ['mediana', gl('Mediana', 'mediana'), fv(v, a.mediana), 'la mitad está por debajo', C.neon],
    ['moda', gl('Moda', 'moda'), fv(v, a.moda), 'el valor más repetido', C.lila], ['desv', gl('Desviación estándar', 'desv'), fv(v, a.desv_est), 'qué tan dispersos', C.lila],
    ['q1', gl('Q1', 'q1'), fv(v, a.q1), '25% está por debajo', C.cyan], ['q3', gl('Q3', 'q3'), fv(v, a.q3), '75% está por debajo', C.cyan],
    ['rango', gl('Rango', 'rango'), fv(v, a.rango), 'máximo − mínimo', C.blue], ['asim', gl('Asimetría', 'asim'), nf(a.asimetria, 2), Math.abs(a.asimetria) < .5 ? 'casi simétrica' : a.asimetria > 0 ? 'cola larga a la derecha' : 'cola larga a la izquierda', C.amber]];
  $('#stats0').innerHTML = S.map(s => `<div class="st" style="--c:${s[4]}"><div class="l">${s[1]}</div><div class="v">${s[2]}</div><div class="d">${s[3]}</div></div>`).join('');
  const h = histOf(v, 'antes');
  P('ch-hist0', [{ type: 'bar', x: h.x, width: h.w, y: h.y, customdata: h.cd, marker: { color: h.y, colorscale: [[0, '#1f3f8a'], [.5, C.lila], [1, C.cyan]], line: { color: '#030714', width: .5 } }, hovertemplate: 'Entre %{customdata[0]:,.1f} y %{customdata[1]:,.1f}: %{y:,} personas<extra></extra>' }],
    { bargap: 0, xaxis: { title: { text: VN[v] + (h.lg ? ' (escala logarítmica)' : '') }, ...(h.lg ? LGX : {}) }, yaxis: { title: { text: 'Personas' } }, margin: { l: 60, r: 20, t: 30, b: 60 },
      shapes: [['media', C.neon, 'dash'], ['mediana', C.pink, 'solid']].map(m => ({ type: 'line', x0: xpos(h, a[m[0]]), x1: xpos(h, a[m[0]]), yref: 'paper', y0: 0, y1: 1, line: { color: m[1], dash: m[2], width: 2 } })),
      annotations: [['media', C.neon, 'media'], ['mediana', C.pink, 'mediana']].map((m, i) => ({ x: xpos(h, a[m[0]]), yref: 'paper', y: 1.06, xanchor: a.media >= a.mediana ? (i ? 'right' : 'left') : (i ? 'left' : 'right'), text: m[2], showarrow: false, font: { color: m[1], size: 11 } })) });
  const lo = Math.max(a.minimo, a.q1 - 1.5 * a.iqr), hi = Math.min(a.maximo, a.q3 + 1.5 * a.iqr);
  P('ch-box0', [{ type: 'box', name: VN[v], q1: [a.q1], median: [a.mediana], q3: [a.q3], lowerfence: [lo], upperfence: [hi], mean: [a.media], x: [VN[v]], orientation: 'v', boxmean: true, marker: { color: C.cyan }, line: { color: C.cyan }, fillcolor: 'rgba(34,228,255,.15)', hoverinfo: 'y' },
    { type: 'scatter', mode: 'markers+text', x: [VN[v], VN[v]], y: [a.minimo, a.maximo], text: ['mín ' + fv(v, a.minimo), 'máx ' + fv(v, a.maximo)], textposition: 'middle right', textfont: { color: C.amber }, marker: { size: 12, color: C.amber, symbol: 'diamond' }, hovertemplate: '%{text}<extra></extra>' }],
    { showlegend: false, yaxis: { type: h.lg ? 'log' : 'linear', title: { text: VN[v] } }, xaxis: { showticklabels: false }, margin: { l: 70, r: 20, t: 10, b: 30 } });
  const r = a.media / a.mediana, out = a.maximo > a.q3 + 3 * a.iqr;
  $('#read0').innerHTML = `<b>Cómo leerlo:</b> ` + (r > 1.15 ? `la media (${fv(v, a.media)}) es mayor que la mediana (${fv(v, a.mediana)}): unos pocos valores muy altos tiran del promedio hacia arriba. Para describir a la persona típica conviene la mediana. `
    : r < 0.87 ? `la media (${fv(v, a.media)}) es menor que la mediana (${fv(v, a.mediana)}): hay valores muy bajos que la arrastran. `
    : `la media (${fv(v, a.media)}) y la mediana (${fv(v, a.mediana)}) son parecidas: los valores se reparten de forma equilibrada. `) +
    (out ? `El máximo (${fv(v, a.maximo)}) está muy lejos del resto (más de 3 veces el rango intercuartílico sobre Q3): es un ${gl('valor atípico', 'atipico')} que se revisa en la fase 06.` : '') +
    (a.n_na ? ` Faltan ${nf(a.n_na)} datos: en la fase 05 se ve si son vacíos esperables o respuestas que faltan.` : '');
}
$('#var0').innerHTML = VORD.map(v => `<button data-v="${v}">${VN[v]}</button>`).join('');
$$('#var0 button').forEach(b => b.onclick = () => showV0(b.dataset.v));
lazy('ch-hist0', () => showV0('ylab'));
onEnf(() => { if ($('#ch-hist0').data) showV0(ENFQ === 'hogar' ? 'yhogpc' : ENFQ === 'oit' ? 'tothrs' : 'ylab'); });
lazy('ch-freq', () => { const FL = { sexo: 'Sexo', zona: 'Área', departamento: 'Departamento', condicion: 'Actividad', nivel_educ: 'Educación', pobre: 'Pobreza' };
  const NL = { 0: 'No pobre', 1: 'Pobre' };
  segBtns('seg-freq', Object.keys(FL).filter(k => freq.some(r => r.variable === k)).map(k => [k, FL[k]]), k => { const R = freq.filter(r => r.variable === k).sort((a, b) => b.pct_ponderado - a.pct_ponderado);
    const lab = R.map(r => ACC(NL[r.categoria] && k === 'pobre' ? NL[r.categoria] : dn(r.categoria)));
    P('ch-freq', [{ type: 'pie', hole: .55, labels: lab, values: R.map(r => r.poblacion), marker: { colors: PAL, line: { color: '#030714', width: 3 } }, textinfo: 'percent', textfont: { color: '#02040c', family: FHEAD }, customdata: R.map(r => r.n),
      hovertemplate: '<b>%{label}</b><br>%{value:,.0f} personas representadas<br>%{customdata:,} en la muestra<extra></extra>', sort: false, pull: R.map((r, i) => i === 0 ? .06 : 0) }],
      { margin: { l: 10, r: 10, t: 10, b: 10 }, legend: { orientation: 'v', x: 1, y: .5, font: { color: C.txt } }, annotations: [{ text: `${FL[k]}<br><span style="font-size:11px">ponderado</span>`, showarrow: false, font: { color: C.txt, size: 15, family: FHEAD } }] }); }); });
lazy('ch-dec', () => { const D = T('F02_deciles_ingreso_laboral'), ks = ['D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9'];
  segBtns('seg-dec', D.map(r => [r.grupo, r.grupo]), g => { const r = by(D, 'grupo', g), ref = by(D, 'grupo', 'Bolivia');
    P('ch-dec', [{ type: 'bar', x: ks.map((k, i) => `${(i + 1) * 10}%`), y: ks.map(k => r[k]), name: g, marker: { color: ks.map(k => r[k] < SMN ? C.pink : C.neon), line: { color: '#030714', width: 1 } }, text: ks.map(k => 'Bs ' + nf(r[k])), textposition: 'outside', textfont: { color: C.txt, size: 10 }, hovertemplate: 'El %{x} gana menos de %{text}<extra></extra>' },
      ...(g !== 'Bolivia' ? [{ type: 'scatter', mode: 'lines+markers', name: 'Bolivia', x: ks.map((k, i) => `${(i + 1) * 10}%`), y: ks.map(k => ref[k]), line: { color: C.mut, dash: 'dot' }, marker: { size: 5 } }] : [])],
      { yaxis: { title: { text: 'Bs al mes' }, tickformat: ',' }, xaxis: { type: 'category', title: { text: 'Porcentaje de ocupados que gana menos que…' } }, margin: { l: 60, r: 20, t: 20, b: 60 }, showlegend: g !== 'Bolivia',
        shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: SMN, y1: SMN, line: { color: C.amber, dash: 'dash' } }], annotations: [{ xref: 'paper', x: 0, y: SMN, text: 'Salario mínimo', showarrow: false, yshift: 10, xanchor: 'left', font: { color: C.amber, size: 11 } }] }); }); });

/* ================= 04 MÉTODO: CRISP-DM + PIPELINE ================= */
(function () {
  const ET = [['Comprensión del negocio', C.amber, [], '¿Cuántos ocupados tienen un empleo vulnerable y dónde están? Se fijan las reglas: salario mínimo 2024 (DS 5154), línea de pobreza oficial del INE y criterio de la OIT.'],
    ['Comprensión de los datos', C.cyan, ['F01', 'F02'], 'Revisar el archivo, confirmar que reproduce las cifras del INE y describirlo con estadística antes de tocar nada.'],
    ['Preparación de los datos', C.neon, ['F03', 'F04', 'F05', 'F06', 'F07', 'F08'], 'Formatos, duplicados, faltantes, atípicos, reglas de validación en ciclo y construcción de las variables objetivo.'],
    ['Modelado', C.lila, ['F10'], 'Seis clasificadores con validación cruzada, ecuación de Mincer, agrupamiento k-means, reglas de asociación y detección de anomalías.'],
    ['Evaluación', C.pink, ['F09', 'F11'], 'Comparar antes y después; evaluar los modelos con datos nunca vistos; desagregar patrones por territorio, sexo y ocupación.'],
    ['Despliegue', C.blue, ['F12'], 'Dataset limpio, bitácora en Excel, simulador exportado y esta página publicada.']];
  const R = 150, r = 92, cx = 200, cy = 200; let s = '';
  ET.forEach((e, i) => { const a0 = (i / 6) * 2 * Math.PI - Math.PI / 2 + 0.03, a1 = ((i + 1) / 6) * 2 * Math.PI - Math.PI / 2 - 0.03;
    const p = (a, rr) => [cx + rr * Math.cos(a), cy + rr * Math.sin(a)]; const [x0, y0] = p(a0, R), [x1, y1] = p(a1, R), [x2, y2] = p(a1, r), [x3, y3] = p(a0, r);
    const am = (a0 + a1) / 2, [tx, ty] = p(am, (R + r) / 2);
    s += `<g class="seg2" data-i="${i}" style="--c:${e[1]}"><path d="M${x0},${y0} A${R},${R} 0 0 1 ${x1},${y1} L${x2},${y2} A${r},${r} 0 0 0 ${x3},${y3} Z" fill="${e[1]}" fill-opacity=".18" stroke="${e[1]}" stroke-width="2"/>
      <text x="${tx}" y="${ty}" text-anchor="middle" style="font-size:11px">${(w => w.length > 1 ? `<tspan x="${tx}" dy="-0.3em">${w[0]}</tspan><tspan x="${tx}" dy="1.2em">${w.slice(1).join(' ').replace('de los ', '')}</tspan>` : `<tspan x="${tx}" dy=".35em">${w[0]}</tspan>`)(e[0].split(' '))}</text></g>`; });
  s += `<circle cx="${cx}" cy="${cy}" r="70" fill="#050b1d" stroke="#24397a"/><text x="${cx}" y="${cy - 8}" text-anchor="middle" style="font-size:16px;font-weight:700">CRISP-DM</text><text x="${cx}" y="${cy + 14}" text-anchor="middle" style="font-size:11px;fill:#93a3d6">${bit.length} decisiones</text>`;
  $('#crisp-svg').innerHTML = `<svg viewBox="0 0 400 400" width="100%">${s}</svg>`;
  const show = i => { const e = ET[i]; $('#crisp-det').innerHTML = `<span class="tag" style="--c:${e[1]}">Etapa ${i + 1} de 6</span><h3 style="margin:12px 0 8px;font-size:26px">${e[0]}</h3><p>${e[3]}</p>
    <div class="chips">${e[2].map(k => chip(k, `${FN[k][1]} · ${bitF(k).length} decisiones${seg(k) != null ? ' · ' + nf(seg(k), 0) + ' s' : ''}`, FN[k][3])).join('') || chip('—', 'Se define antes de programar: el problema y sus umbrales', e[1])}</div>`;
    $$('.seg2 path').forEach((p, j) => p.setAttribute('fill-opacity', j === i ? '.55' : '.18')); };
  $$('.seg2').forEach(g => g.onclick = () => show(+g.dataset.i)); show(1);
  let k = 0; const auto = setInterval(() => { if (!document.querySelector('#metodo.vis')) return; show((k++) % 6); }, 3500);
  $('#crisp-svg').addEventListener('click', () => clearInterval(auto), { once: true });
})();
$('#nodes').innerHTML = FASES.map((f, i) => `<div class="node" data-i="${i}" style="--c:${f[3]}"><div class="stage">${f[2].replace(' de los datos', '')}</div><div class="dot">${f[0].slice(1)}</div><div class="nm">${f[1]}</div><div class="mt">${bitF(f[0]).length} dec · ${seg(f[0]) != null ? nf(seg(f[0]), 0) + ' s' : '—'}</div></div>`).join('');
$$('.node').forEach(n => n.onclick = () => { const f = FASES[+n.dataset.i]; openDock(f[4]); const t = document.getElementById('ph-' + f[0]); if (t) t.scrollIntoView({ behavior: 'smooth' }); });
setTimeout(() => $('#trk').style.width = '100%', 900);
let timer = null;
function resetReplay() { clearInterval(timer); timer = null; $('#ticker').innerHTML = ''; ['f', 'c', 'b', 'e'].forEach(k => $('#mt-' + k).textContent = '0'); $$('.node').forEach(n => n.classList.remove('act')); $('#trk').style.width = '0'; $('#play').textContent = '▶ Reproducir la ejecución'; $('#play-info').textContent = ''; }
$('#reset').onclick = resetReplay;
$('#play').onclick = () => {
  if (timer) { clearInterval(timer); timer = null; $('#play').textContent = '▶ Continuar'; return; }
  if (+$('#mt-b').textContent >= bit.length) resetReplay();
  let i = +$('#mt-b').textContent, f = +$('#mt-f').textContent.replace(/,/g, '') || 0, c = +$('#mt-c').textContent.replace(/,/g, '') || 0;
  $('#play').textContent = '❚❚ Pausar';
  timer = setInterval(() => {
    if (i >= bit.length) { clearInterval(timer); timer = null; $('#play').textContent = '↺ Repetir'; $('#play-info').textContent = 'Ejecución completa: ' + nf(filas0) + ' filas conservadas'; return; }
    const r = bit[i], k = (r.fase || '').slice(0, 3), fi = FASES.findIndex(x => x[0] === k);
    if (r.filas === filas0 && r.columnas >= cols0) { f = r.filas; c = r.columnas; }
    $('#mt-f').textContent = nf(f); $('#mt-c').textContent = nf(c); $('#mt-b').textContent = i + 1; $('#mt-e').textContent = nf(f ? filas0 - f : 0);
    $$('.node').forEach((n, j) => n.classList.toggle('act', j === fi));
    $('#trk').style.width = ((fi + 1) / FASES.length * 100) + '%';
    if (fi >= 0) { const hot = String(r.codigo || '').split(/[;(]/)[0].trim(); openDock(FASES[fi][4], hot.length > 3 ? hot : null); }
    const d = el('div', '', `<b>#${String(r.id).padStart(2, '0')}</b> <i>${k}</i> ${r.atributo ? '<span style="color:var(--cyan)">' + String(r.atributo).slice(0, 48) + '</span> · ' : ''}${ACC(r.accion)}`);
    $('#ticker').prepend(d); i++;
  }, 520);
};
new IntersectionObserver(es => es.forEach(e => e.target.classList.toggle('vis', e.isIntersecting)), { threshold: .2 }).observe($('#metodo'));

/* ================= 05 FASES ================= */
const A0 = T('F01_anomalias_iniciales'), anom = k => (A0.find(r => String(r.anomalia).startsWith(k)) || {}).n;
const F5C = T('F05_clasificacion_faltantes'), CAT67 = T('F05_ylab_faltante_por_categoria'), SENS = T('F05_sensibilidad_tasa_vulnerable'), EXP = T('F05_experimento_sin_pago'), MASK = T('F05_evaluacion_enmascaramiento');
const metodoY = ((bitF('F05').find(r => /Metodo elegido/i.test(r.accion)) || {}).accion || '').replace('Metodo elegido: ', '');
const OUT = T('F06_comparacion_metodos_atipicos'), V7 = T('F07_validacion_inicial'), H7 = T('F07_historial_ciclo'), CAL9 = T('F09_tablero_calidad'), PR9 = T('F09_pruebas_antes_despues'), PR2 = T('F02_pruebas_hipotesis');
const MZ = T('F10_mincer_coeficientes'), MZE = MZ.filter(r => r.modelo === 'Extendida'), MZC = MZ.filter(r => r.modelo === 'Clasica');
const CLP = T('F10_clusters_perfil'), RGL = T('F10_reglas_asociacion');
const VNAME = { factor: 'Factor', z: 'Línea pobreza', zext: 'Línea extrema', ylab: 'Ingreso laboral', yprilab: 'Ingreso principal', ynolab: 'Ingreso no laboral', yper: 'Ingreso personal', yhog: 'Ingreso hogar', yhogpc: 'Ingreso por persona', tothrs: 'Horas' };
const PH = {
  F01: { sub: 'Antes de analizar, confirmar que el archivo es el oficial y está bien leído',
    exp: () => `Se carga el archivo completo <b>como texto</b>, sin cambiar nada, para no perder códigos con ceros a la izquierda. Luego se revisa qué es cada columna, cuáles son la edad, el sexo y el parentesco (se reconocen por sus valores, no por su posición), si los números están bien escritos y si el archivo <b>reproduce las cifras oficiales del INE</b>.`,
    hz: () => { const D = T('F01_deteccion_edad_sexo_parentesco'); return [`Lectura completa: <b>${nf(filas0)}</b> filas × <b>${nf(cols0)}</b> columnas (con huella md5 para saber que siempre es el mismo archivo).`,
      `Columnas clave reconocidas por sus valores: ${D.map(r => `<b>${r.columna}</b> = ${String(r.rol_detectado).toLowerCase()}`).join(', ')}.`,
      `Formato numérico revisado en ${FMT.length} variables: <b>${nf(nFmt)}</b> valores con problema. Tratamiento previo probado con una simulación (ver “Cuidado con el formato regional”).`,
      `Las medias de <b>${vddi.filter(r => Math.abs(r.dif_media_pct) < 0.1).length} de ${vddi.length}</b> variables coinciden con las publicadas por el INE en el catálogo de la encuesta.`,
      `Lista inicial de <b>${A0.length}</b> tipos de anomalía, cada una asignada a la fase que la trata.`]; },
    chips: () => [[nf(filas0), 'personas (filas)', C.neon], [pc(pob0.calculada), `pobreza calculada · INE ${pc(pob0.oficial_ine)}`, C.cyan], ['Bs ' + nf(ylabV.media_calculada), `ingreso laboral medio · INE Bs ${nf(ylabV.media_oficial)}`, C.lila], [nf(nFmt), 'números mal escritos', C.amber]],
    dec: 'Si las cifras no coincidieran con las del INE, ninguna conclusión posterior sería confiable. Coinciden: se puede seguir.',
    chart: id => { const R = vddi.filter(r => r.media_oficial != null);
      P(id, [{ type: 'scatter', mode: 'markers+text', x: R.map(r => r.media_oficial), y: R.map(r => r.media_calculada), text: R.map(r => VNAME[r.variable] || r.variable), textposition: 'top center', textfont: { color: C.txt, size: 10 }, marker: { size: 13, color: C.cyan, line: { color: '#fff', width: 1 } }, hovertemplate: '%{text}<br>INE: %{x:,.1f}<br>Calculado: %{y:,.1f}<extra></extra>' },
        { x: [5, 7000], y: [5, 7000], mode: 'lines', line: { color: C.neon, dash: 'dash' }, hoverinfo: 'skip' }],
        { showlegend: false, xaxis: { type: 'log', title: { text: 'Media publicada por el INE' } }, yaxis: { type: 'log', title: { text: 'Media calculada' } }, margin: { l: 60, r: 10, t: 10, b: 50 },
          annotations: [{ xref: 'paper', yref: 'paper', x: .02, y: .98, xanchor: 'left', text: 'Si el punto cae en la línea, coincide con el INE', showarrow: false, font: { color: C.neon, size: 11 } }] }); },
    cap: 'Cada punto es una variable: su media calculada contra la publicada por el INE.',
    code: [['F01_auditoria', [['raw <- as.data.frame(data.table::fread(RUTA_CSV', 0, 1], ['MAPA <- detectar_mapa(eda)', 0, 0], ['formato <- diagnosticar_formato(raw)', 0, 1], ['sim <- orig; for (v in VARS_REC)', 0, 2], ['val_ddi <- validar_contra_ddi(d_num)', 0, 0]]]] },
  F02: { sub: 'Describir los datos tal como llegaron, para tener un “antes”',
    exp: () => `Antes de tocar nada se describe cada variable con ${gl('medidas de centro', 'media')} (media, mediana, moda), de dispersión (${gl('desviación', 'desv')}, rango) y de forma (${gl('asimetría', 'asim')}). Luego se ponen a prueba preguntas con estadística inferencial: ¿ganan distinto hombres y mujeres? ¿importa el departamento? ¿la educación se asocia con el ingreso?`,
    hz: () => { const a = by(M0, 'variable', 'ylab'); return [`Medidas completas de <b>${M0.length}</b> variables numéricas (ver sección 03).`,
      `Ingreso laboral: media <b>Bs ${nf(a.media)}</b> pero mediana <b>Bs ${nf(a.mediana)}</b>: pocos ingresos muy altos inflan el promedio.`,
      `<b>${PR2.filter(r => r.decision === 'Se rechaza H0').length} de ${PR2.length}</b> pruebas de hipótesis encuentran diferencias reales (p < 0.05).`,
      `<b>${CUR.length}</b> datos curiosos o extremos anotados para las fases siguientes.`]; },
    chips: () => { const a = by(M0, 'variable', 'ylab'), m = T('F02_regresion_mincer_sin_tratar'); return [['Bs ' + nf(a.mediana), 'ingreso laboral mediano', C.neon], [nf(a.asimetria, 1), 'asimetría del ingreso (cola larga)', C.amber], [`${PR2.filter(r => r.decision === 'Se rechaza H0').length}/${PR2.length}`, 'pruebas con diferencias reales', C.pink]]; },
    dec: 'Para hablar del ingreso “típico” se usa la mediana, no la media.',
    chart: id => { const R = PR2.slice().reverse();
      P(id, [{ type: 'bar', orientation: 'h', y: R.map(r => ACC(r.prueba)), x: R.map(r => Math.min(50, -Math.log10(Math.max(r.p_valor, 1e-300)))), marker: { color: R.map(r => r.p_valor < .05 ? C.neon : C.mut) }, customdata: R.map(r => [hq(r.hipotesis_nula), ACC(r.interpretacion), r.p_valor]),
        hovertemplate: '<b>%{customdata[0]}</b><br>%{customdata[1]}<br>p = %{customdata[2]:.2e}<extra></extra>' }],
        { margin: { l: 190, r: 20, t: 10, b: 50 }, xaxis: { title: { text: 'Fuerza de la evidencia (−log10 p)' } }, yaxis: { tickfont: { color: C.txt, size: 10 } },
          shapes: [{ type: 'line', x0: 1.3, x1: 1.3, yref: 'paper', y0: 0, y1: 1, line: { color: C.amber, dash: 'dash' } }], annotations: [{ x: 1.3, yref: 'paper', y: 1.02, text: 'p = 0.05', showarrow: false, font: { color: C.amber, size: 10 } }] }); },
    cap: 'Pasa el cursor por cada barra: qué se preguntó y qué respondieron los datos. A la derecha de la línea amarilla, la diferencia es real.',
    code: [['F02_descriptivo_inferencial', [['med0 <- medidas(eda, vars_q, "Sin tratar")', 0, 0], ['deciles <- bind_rows(', 0, 2], ['curiosidades <- tribble(', 0, 3], ['pruebas0 <- pruebas_hipotesis(eda, "Sin tratar")', 0, 0]]]] },
  F03: { sub: 'Ordenar la forma de los datos sin cambiar su contenido',
    exp: () => `Se limpian espacios sobrantes, tildes y mayúsculas; respuestas como “NS/NR” o “sin dato” pasan a ser vacíos explícitos; los números guardados como texto se convierten a número; se construye la fecha de nacimiento y se ponen <b>etiquetas legibles</b> (1 → Hombre, 2 → Mujer; 1 → Urbana, 2 → Rural).`,
    hz: () => { const cv = T('F03_conversion_tipos'), ct = T('F03_cambios_texto'); return [`<b>${nf(cv.filter(r => r.tipo_despues === 'numeric').length)}</b> columnas convertidas a número; las que no se pueden leer como número quedan como texto.`,
      `<b>${nf(sum(ct.map(r => r.texto_modificado)))}</b> celdas de texto homogeneizadas y <b>${nf(sum(ct.map(r => r.tokens_a_na)))}</b> respuestas tipo “no sabe” pasadas a vacío.`,
      `Nuevas columnas con nombre legible: edad, sexo, zona, departamento, condición de actividad, pobreza, nivel educativo.`]; },
    chips: () => { const cv = T('F03_conversion_tipos'); return [[nf(cv.filter(r => r.tipo_despues === 'numeric').length), 'columnas a número', C.neon], [nf(cv.filter(r => r.tipo_despues !== 'numeric').length), 'columnas de texto o código', C.cyan], ['0', 'valores perdidos en la conversión', C.lila]]; },
    dec: 'Lo que no se puede convertir no se borra: se guarda aparte (columnas txt_).',
    chart: id => { const cv = T('F03_conversion_tipos'), sec = Object.fromEntries(dic0.map(r => [r.variable, ACC(r.seccion).replace(/^\d+ /, '')])), g = {};
      cv.forEach(r => { const s = sec[r.variable] || 'Otras'; g[s] = g[s] || [0, 0]; g[s][r.tipo_despues === 'numeric' ? 0 : 1]++; }); const S = Object.keys(g);
      grow(id, [{ type: 'bar', name: 'Número', x: S, y: S.map(s => g[s][0]), marker: { color: C.neon } }, { type: 'bar', name: 'Texto / código', x: S, y: S.map(s => g[s][1]), marker: { color: C.lila } }],
        { barmode: 'stack', yaxis: { title: { text: 'Columnas' } }, xaxis: { tickfont: { color: C.txt, size: 10 } }, margin: { l: 50, r: 10, t: 10, b: 80 } }, 'y'); },
    cap: 'Columnas por sección del cuestionario según su tipo final.',
    code: [['F03_formatos', [['TOKENS_NA <- c(', 0, 0], ['x0 <- df[[v]]; x <- str_squish(x0)', 0, 3], ['df <- etiquetar(df, MAPA)', 0, 1]]]] },
  F04: { sub: 'Una persona no puede contarse dos veces',
    exp: () => `Se busca la misma fila repetida (duplicado exacto), el mismo hogar y número de persona dos veces (llave repetida) y personas casi idénticas dentro de un hogar. El archivo no trae duplicados, así que se hizo un <b>experimento</b>: se insertaron copias a propósito y se midió cuánto distorsionan los resultados y qué corrección funciona mejor.`,
    hz: () => { const S = T('F04_simulacion_duplicados'), o = S[0] || {}, d = S[1] || {}; return [`Duplicados exactos: <b>${nf(anom('Filas duplicadas exactas'))}</b>. Llaves repetidas: <b>${nf(anom('Llave folio'))}</b>.`,
      `Experimento: con <b>${nf(d.registros - o.registros)}</b> copias, la población aparente sube de ${nf(o.poblacion / 1e6, 2)} M a <b>${nf(d.poblacion / 1e6, 2)} M</b>.`,
      `La corrección “conservar la primera aparición” devuelve exactamente los valores originales.`]; },
    chips: () => { const S = T('F04_simulacion_duplicados'), o = S[0] || {}, d = S[1] || {}; return [[nf(anom('Filas duplicadas exactas')), 'duplicados en el archivo', C.neon], [pc(d.poblacion / o.poblacion - 1), 'población inflada si hubiera copias', C.pink], ['0', 'filas borradas', C.cyan]]; },
    dec: 'Si aparece una copia se conserva la primera; la copia queda marcada con peso 0: no se borra, pero no cuenta.',
    chart: id => { const S = T('F04_simulacion_duplicados');
      grow(id, [{ type: 'bar', x: S.map(r => ACC(r.version)), y: S.map(r => r.poblacion / 1e6), marker: { color: S.map((r, i) => i === 1 ? C.pink : i === 0 ? C.neon : C.cyan) }, text: S.map(r => nf(r.poblacion / 1e6, 2) + ' M'), textposition: 'outside', textfont: { color: C.txt }, customdata: S.map(r => [r.registros, r.tasa_pobreza]), hovertemplate: '%{x}<br>%{customdata[0]:,} registros<br>pobreza %{customdata[1]:.1%}<extra></extra>' }],
        { yaxis: { title: { text: 'Población representada (millones)' }, range: [11.5, 13.3] }, xaxis: { tickfont: { color: C.txt, size: 10 } }, margin: { l: 60, r: 10, t: 10, b: 80 } }, 'y'); },
    cap: 'Experimento: población que representaría la encuesta con copias y con cada corrección.',
    code: [['F04_duplicados', [['df$dup_exacto', 0, 3], ['repetidos <- base_dup[sample(', 0, 1], ['primera     <- datos_sucios', 0, 0]]]] },
  F05: { sub: 'No todo vacío es un error',
    exp: () => `Si a alguien que no trabaja no se le pregunta su salario, ese vacío es correcto (<b>estructural</b>). Solo se completan los vacíos de quien sí debía responder. Hay un caso especial: <b>${nf(sum(CAT67.map(r => r.cero_estructural)))}</b> personas trabajan como familiares o aprendices <b>sin recibir pago</b>; su ingreso no falta: es cero, igual que en el cálculo oficial del INE.`,
    hz: () => { const s = SENS.map(r => r.tasa); return [`<b>${nf(sum(F5C.map(r => naSplit(r)[0])))}</b> vacíos estructurales (la pregunta no correspondía) y <b>${nf(sum(F5C.map(r => naSplit(r)[1])))}</b> de no respuesta, casi todos en columnas que no se usan en el análisis.`,
      `Trabajo sin pago: ingreso = 0 en <b>${nf(sum(CAT67.map(r => r.cero_estructural)))}</b> casos (categorías 6 y 7 del cuestionario).`,
      `Solo <b>${nf(sum(CAT67.map(r => r.no_respuesta)))}</b> ingresos faltan de verdad. Para elegir cómo completarlos se ocultó el 10% de los ingresos conocidos y se probaron ${MASK.length} métodos: ganó <b>${ACC(metodoY)}</b>.`,
      `Con 11 formas distintas de completar, la tasa A solo se mueve entre <b>${pc(Math.min(...s))}</b> y <b>${pc(Math.max(...s))}</b>: la conclusión no depende del método.`]; },
    chips: () => [[nf(sum(CAT67.map(r => r.cero_estructural))), 'trabajan sin pago: ingreso 0', C.amber], [nf(sum(CAT67.map(r => r.no_respuesta))), 'ingresos completados', C.pink], [pc((EXP[0] || {}).pobreza_poblacion), 'pobreza con la decisión correcta', C.neon], [pc((EXP[1] || {}).pobreza_poblacion), 'si se inventara un sueldo', C.pink]],
    dec: () => `Inventarles un sueldo a quienes trabajan sin pago bajaría la pobreza de ${pc((EXP[0] || {}).pobreza_poblacion)} a ${pc((EXP[1] || {}).pobreza_poblacion)}: desaparecería pobreza que sí existe. Por eso su ingreso es 0.`,
    chart: id => { const X = ['Ingreso 0 (decisión del proyecto)', 'Si se inventara un sueldo'], hA = enfs().some(e => e.k === 'persona'), hB = enfs().some(e => e.k !== 'persona');
      P(id, [{ type: 'bar', name: 'Pobreza de toda la población', x: X, y: EXP.map(r => r.pobreza_poblacion), marker: { color: [C.cyan, 'rgba(34,228,255,.45)'], opacity: hB ? 1 : .35 }, text: EXP.map(r => pc(r.pobreza_poblacion)), textposition: 'outside', textfont: { color: C.txt, family: FHEAD, size: 14 }, hovertemplate: '%{x}<br>Pobreza: %{y:.1%}<extra></extra>' },
        { type: 'bar', name: 'Tasa A (ocupados de 19+)', x: X, y: EXP.map(r => r.tasa_A), marker: { color: [C.pink, 'rgba(255,79,163,.45)'], opacity: hA ? 1 : .35 }, text: EXP.map(r => pc(r.tasa_A)), textposition: 'outside', textfont: { color: C.txt, family: FHEAD, size: 14 }, hovertemplate: '%{x}<br>Tasa A: %{y:.1%}<extra></extra>' }],
        { barmode: 'group', yaxis: { tickformat: '.0%', range: [0, .62] }, xaxis: { tickfont: { color: C.txt } }, margin: { l: 50, r: 10, t: 10, b: 60 }, legend: { orientation: 'h', y: -0.2 },
          shapes: [{ type: 'line', x0: -0.45, x1: 1.45, y0: pob0.oficial_ine, y1: pob0.oficial_ine, line: { color: C.cyan, dash: 'dot', width: 1.5 } }], annotations: [{ x: 1.45, y: pob0.oficial_ine, xanchor: 'right', text: `pobreza oficial INE ${pc(pob0.oficial_ine)}`, showarrow: false, yshift: 10, font: { color: C.cyan, size: 11 } }] }); },
    enf: true, cap: 'Experimento con los mismos datos: qué pasaría si se tratara como “faltante” el ingreso de quien trabaja sin pago. Cambia con el enfoque.',
    code: [['F05_faltantes_imputacion', [['df$ylab_cero_estructural <- dom_ocu', 0, 0], ['df$ylab[df$ylab_cero_estructural] <- 0', 0, 0], ['mask <- sample(nrow(obs)', 0, 1], ['METODO_YLAB <- cand$metodo', 0, 0], ['df$ylab[falt_y$.fila] <- completo[[METODO_YLAB]]', 0, 0], ['y_sp <- pmax(imputadores', 0, 0]]]] },
  F06: { sub: 'Valores extremos: ¿reales o errores?',
    exp: () => `Un valor muy alejado puede ser real (alguien que gana mucho) o un error. Se comparan cuatro reglas para detectarlos. En los ingresos se usa la escala logarítmica, porque crecen de forma multiplicativa: con la regla clásica, todo el que gana bien parecería “raro”. Nada se borra: el original se conserva y se crea una versión ${gl('winsorizada', 'winsor')} para los modelos.`,
    hz: () => OUT.filter(r => r.elegido).slice(0, 5).map(r => `<b>${VNAME[r.variable] || r.variable}</b>: ${nf(r.total)} atípicos (${pc(r.pct, 1)}) con la regla “${r.metodo}”.`),
    chips: () => { const y = OUT.find(r => r.variable === 'ylab' && r.elegido) || {}, h = OUT.find(r => r.variable === 'tothrs' && r.elegido) || {}; return [[nf(y.total), `ingresos atípicos (${y.metodo})`, C.amber], [nf(h.total), 'jornadas atípicas', C.pink], ['0', 'valores borrados', C.neon]]; },
    dec: 'Se marca y se recorta, no se borra: el dato original sigue disponible para revisarlo.',
    chart: id => { const vs = [...new Set(OUT.map(r => r.variable))], ms = [...new Set(OUT.map(r => r.metodo))];
      P(id, [{ type: 'heatmap', x: ms, y: vs.map(v => VNAME[v] || v), z: vs.map(v => ms.map(m => (OUT.find(r => r.variable === v && r.metodo === m) || {}).pct)), text: vs.map(v => ms.map(m => { const r = OUT.find(x => x.variable === v && x.metodo === m) || {}; return (r.elegido ? '★ ' : '') + pc(r.pct, 1); })),
        texttemplate: '%{text}', textfont: { color: '#fff', size: 11 }, colorscale: [[0, '#0b1a4a'], [.5, C.lila], [1, C.amber]], showscale: false, xgap: 3, ygap: 3, hovertemplate: '%{y} · %{x}: %{text}<extra></extra>' }],
        { margin: { l: 120, r: 10, t: 10, b: 50 }, yaxis: { tickfont: { color: C.txt } }, xaxis: { tickfont: { color: C.txt } } }); },
    cap: '% de valores marcados por cada regla. ★ = regla elegida para esa variable.',
    code: [['F06_outliers', [['METODOS <- c(', 0, 0], ['for (v in names(VARS_OUT)) {', 0, 4]]]] },
  F07: { sub: 'Reglas de sentido común, repetidas hasta que no quede ningún error',
    exp: () => `Se aplican reglas que cualquiera entiende: nadie trabaja más de 168 horas en una semana, nadie tiene más años de estudio de los que su edad permite, un ingreso no puede ser negativo. Lo imposible <b>vuelve como vacío</b> a la fase de faltantes, se completa con personas parecidas y se valida otra vez: un <b>ciclo</b> que termina cuando no queda ninguna violación.`,
    hz: () => [`<b>${nf(sum(V7.filter(r => /Corregible/.test(r.tipo)).map(r => r.violaciones)))}</b> valores imposibles al inicio.`, ...V7.filter(r => r.violaciones > 0).slice(0, 4).map(r => `${ACC(r.regla.replace(/_/g, ' '))}: <b>${nf(r.violaciones)}</b> (${/Corregible/.test(r.tipo) ? 'se corrige' : 'se marca'})`),
      `El ciclo terminó en <b>${Math.max(...H7.map(r => r.iteracion))}</b> vuelta(s) con 0 violaciones.`],
    chips: () => [[nf(sum(V7.filter(r => /Corregible/.test(r.tipo)).map(r => r.violaciones))), 'valores imposibles', C.pink], [nf(Math.max(...H7.map(r => r.iteracion))), 'vueltas del ciclo', C.amber], ['0', 'violaciones al final', C.neon]],
    dec: 'Un valor imposible no se “arregla a mano”: se trata como faltante y se completa con el mismo método documentado.',
    chart: id => { const it = [...new Set(H7.map(r => r.iteracion))], rg = [...new Set(H7.map(r => r.regla))].filter(g => H7.some(r => r.regla === g && r.violaciones > 0));
      grow(id, rg.map((g, i) => ({ type: 'bar', name: ACC(g.replace(/_/g, ' ')), x: it.map(n => 'Vuelta ' + n), y: it.map(n => (H7.find(r => r.iteracion === n && r.regla === g) || {}).violaciones || 0), marker: { color: PAL[i % PAL.length] } })),
        { barmode: 'stack', yaxis: { title: { text: 'Violaciones' } }, xaxis: { tickfont: { color: C.txt } }, margin: { l: 50, r: 10, t: 10, b: 50 }, legend: { orientation: 'h', y: -0.2 } }, 'y'); },
    cap: 'Violaciones por vuelta del ciclo validación → faltantes → atípicos.',
    code: [['F07_validacion_ciclo', [['reglas_corregibles <- function(d) list(', 0, 6], ['repeat {', 0, 8]]]] },
  F08: { sub: 'Aquí nacen las respuestas',
    exp: () => `Se construyen las tres variables objetivo (A, B y C) sobre las mismas personas, más las variables del trabajo: categoría ocupacional, ${gl('informalidad', 'informal')}, grupo ocupacional, rama, tamaño del hogar, y el ${gl('IVL', 'ivl')} por departamento.`,
    hz: () => [`A · Persona: <b>${pc(te('persona').tasa)}</b> gana menos de Bs ${nf(SMN)}.`, `B · Hogar: <b>${pc(te('hogar').tasa)}</b> vive en un hogar pobre.`, `C · OIT: <b>${pc(te('oit').tasa)}</b> trabaja por cuenta propia o sin pago.`,
      `A y B coinciden poco (${gl('kappa', 'kappa')} <b>${nf(kAB.kappa, 2)}</b>): miden cosas distintas.`],
    chips: () => enfs().map(e => [pc(te(e.k).tasa), e.l, e.c]).concat([[nf(kAB.kappa, 2), 'kappa A-B', C.lila]]),
    dec: 'Se conservan las tres definiciones en lugar de elegir una: cada una ilumina a un grupo distinto de trabajadores.',
    chart: id => { const R = T('F08_resumen_variables_empleo').filter(r => r.variable === 'cat_ocupacional').sort((a, b) => b.pct_ocupados - a.pct_ocupados);
      P(id, enfs().map(e => ({ type: 'bar', orientation: 'h', name: e.l, y: R.map(r => ACC(r.categoria)), x: R.map(r => e.k === 'oit' ? (['Cuenta propia', 'Familiar sin remuneracion'].includes(r.categoria) ? 1 : 0) : r[e.col]), marker: { color: e.c }, customdata: R.map(r => r.pct_ocupados), hovertemplate: '%{y}: %{x:.1%}<br>%{customdata:.1%} de los ocupados<extra>' + e.l + '</extra>' })),
        { barmode: 'group', margin: { l: 180, r: 10, t: 10, b: 50 }, xaxis: { tickformat: '.0%', range: [0, 1.05] }, yaxis: { autorange: 'reversed', tickfont: { color: C.txt, size: 11 } } }); },
    enf: true, cap: 'Tasa del enfoque elegido según la categoría del trabajo. Cambia con el enfoque.',
    code: [['00_config', [['d$crit_ingreso <- d$ylab < CFG$smn', 0, 2], ['d$vul_persona <- lab(d$crit_ingreso)', 0, 2]]], ['F08_transformacion', [['df$informal <- ifelse', 0, 0], ['df$grupo_ocup <- factor', 0, 0]]]] },
  F09: { sub: '¿La preparación mejoró los datos sin deformarlos?',
    exp: () => `Se repiten sobre el dataset preparado las mismas medidas y pruebas de la fase 02. Si las conclusiones cambiaran mucho, la limpieza habría deformado la información; si se mantienen, la refuerza. Además se mide la calidad en cuatro dimensiones.`,
    hz: () => { const a = PR9.filter(r => r.momento === 'Sin tratar'), b = PR9.filter(r => r.momento === 'Limpio'); return [`<b>${a.filter((r, i) => b[i] && r.decision === b[i].decision).length} de ${a.length}</b> conclusiones estadísticas se mantienen.`,
      ...CAL9.map(r => `${ACC(r.dimension)}: <b>${pc(r.Antes)}</b> → <b>${pc(r.Despues)}</b>`)]; },
    chips: () => CAL9.slice(0, 3).map((r, i) => [pc(r.Despues, 0), ACC(r.dimension).split(' (')[0] + ' después', [C.neon, C.cyan, C.lila][i]]),
    dec: 'La limpieza completó y corrigió sin cambiar el mensaje de los datos.',
    chart: id => { const th = CAL9.map(r => ACC(r.dimension).split(' (')[0]);
      P(id, [['Antes', C.pink], ['Despues', C.neon]].map(([k, c]) => ({ type: 'scatterpolar', fill: 'toself', name: k === 'Antes' ? 'Antes' : 'Después', r: CAL9.map(r => r[k]).concat([CAL9[0][k]]), theta: th.concat([th[0]]), line: { color: c }, fillcolor: c === C.neon ? 'rgba(57,255,159,.15)' : 'rgba(255,79,163,.15)' })),
        { polar: { bgcolor: 'rgba(0,0,0,0)', radialaxis: { range: [Math.min(.95, ...CAL9.map(r => r.Antes)) - .01, 1], tickformat: '.0%', gridcolor: C.grid, tickfont: { color: C.dim, size: 9 } }, angularaxis: { gridcolor: C.grid, tickfont: { color: C.txt, size: 11 } } }, margin: { l: 50, r: 50, t: 20, b: 30 } }); },
    cap: 'Calidad del dataset antes y después (más afuera = mejor).',
    code: [['F09_descriptivo_limpio', [['med1 <- medidas(dl, vars_q, "Limpio")', 0, 0], ['pruebas1 <- pruebas_hipotesis(dl, "Limpio")', 0, 0], ['calidad_tab <- data.frame(', 0, 6]]]] },
  F10: { sub: 'Enseñarle a la computadora a reconocer patrones',
    exp: () => `Con los datos listos se entrenan modelos: <b>seis clasificadores</b> intentan adivinar quién es vulnerable (A o B) sin ver su ingreso; la ${gl('ecuación de Mincer', 'mincer')} estima cuánto suma cada año de estudio; ${gl('k-means', 'kmeans')} agrupa trabajadores parecidos y las reglas de asociación encuentran combinaciones frecuentes.`,
    hz: () => ['A', 'B'].map(o => `Objetivo ${o}: mejor modelo <b>${ACC(best(o).modelo)}</b>, ${gl('AUC', 'auc')} <b>${nf(best(o).auc_media, 3)}</b> en ${gl('validación cruzada', 'cv5')}.`)
      .concat([`Cada año de estudio se asocia con <b>${nf((MZC.find(r => r.termino === 'aestudio') || {}).efecto_pct, 1)}%</b> más de ingreso.`, `<b>${CLP.length}</b> tipos de trabajador encontrados por k-means; <b>${nf(RGL.length)}</b> reglas de asociación.`]),
    chips: () => enfs().filter(e => e.k !== 'oit').map(e => { const o = e.s; return [nf(best(o).auc_media, 3), `AUC ${o} · ${ACC(best(o).modelo)}`, e.c]; }).concat([['6', 'modelos comparados', C.lila], [nf(CLP.length), 'grupos k-means', C.cyan]]),
    dec: 'Se elige por AUC con la regla de 1 error estándar; el simulador usa la logística porque se puede leer y calcular en el navegador.',
    chart: id => { const o = enfAB().s, R = CVR.filter(r => r.objetivo === o).sort((a, b) => a.auc_media - b.auc_media);
      grow(id, [{ type: 'bar', orientation: 'h', y: R.map(r => ACC(r.modelo)), x: R.map(r => r.auc_media), marker: { color: R.map((r, i) => i === R.length - 1 ? enfAB().c : C.lila) }, text: R.map(r => nf(r.auc_media, 3)), textposition: 'inside', textfont: { color: '#02040c', family: FHEAD } }],
        { margin: { l: 170, r: 10, t: 10, b: 50 }, xaxis: { range: [.7, .93], title: { text: 'AUC (validación cruzada)' } }, yaxis: { tickfont: { color: C.txt } } }); },
    enf: true, cap: 'AUC de cada modelo para el objetivo del enfoque elegido (C usa A).',
    code: [['F10_modelado', [['idx_tr <- unlist(', 0, 1], ['cv_res[[o]] <- comparar_modelos(tr, OBJ[[o]], X_ML)', 0, 0], ['K_CL <- codo_cl$k', 0, 1]]]] },
  F11: { sub: 'Desagregar: ¿dónde, quiénes y en qué trabajos?',
    exp: () => `Las tasas se desagregan por departamento, área, sexo, ocupación, rama de actividad, edad y educación, siempre <b>ponderadas</b> por el ${gl('factor de expansión', 'factor')} (cada persona encuestada representa a cientos). Cada hallazgo se escribe automáticamente con los números del código.`,
    hz: () => { const e = enf1(), D = dep.slice().sort((a, b) => b[e.col] - a[e.col]); return [`Mayor tasa ${e.s}: <b>${dn(D[0].departamento)}</b> (${pc(D[0][e.col])}); menor: <b>${dn(D[D.length - 1].departamento)}</b> (${pc(D[D.length - 1][e.col])}).`,
      `Mujeres <b>${pc(by(sx, 'sexo', 'Mujer')[e.col])}</b> vs hombres <b>${pc(by(sx, 'sexo', 'Hombre')[e.col])}</b>.`, `Rural <b>${pc(by(zon, 'zona', 'Rural')[e.col])}</b> vs urbano <b>${pc(by(zon, 'zona', 'Urbana')[e.col])}</b>.`,
      `<b>${(E.hallazgos || []).length}</b> hallazgos calculados (sección 10).`]; },
    chips: () => { const e = enf1(), D = dep.slice().sort((a, b) => b[e.col] - a[e.col]); return [[pc(D[0][e.col]), `${dn(D[0].departamento)} (máximo ${e.s})`, e.c], [pp(by(sx, 'sexo', 'Mujer')[e.col] - by(sx, 'sexo', 'Hombre')[e.col]), 'brecha mujeres − hombres', C.pink], [pp(by(zon, 'zona', 'Rural')[e.col] - by(zon, 'zona', 'Urbana')[e.col]), 'brecha rural − urbano', C.amber]]; },
    dec: 'Toda tasa se pondera: sin el factor, la muestra sobrerrepresenta a algunas regiones.',
    chart: id => { const e = enf1(), D = dep.slice().sort((a, b) => a[e.col] - b[e.col]);
      grow(id, enfs().map(x => ({ type: 'bar', orientation: 'h', name: x.l, y: D.map(r => dn(r.departamento)), x: D.map(r => r[x.col]), marker: { color: x.c }, text: D.map(r => pc(r[x.col], 0)), textposition: 'inside', textfont: { color: '#02040c' } })),
        { barmode: 'group', margin: { l: 100, r: 10, t: 10, b: 40 }, xaxis: { tickformat: '.0%' }, yaxis: { tickfont: { color: C.txt } } }); },
    enf: true, cap: 'Tasa por departamento con el enfoque elegido.',
    code: [['F11_patrones', [['tasa2 <- function(d, ...)', 0, 4], ['t_dep  <- tasa2(est, departamento)', 0, 0]]]] },
  F12: { sub: 'Entregar todo y comprobar la regla de oro',
    exp: () => `Se exporta el dataset limpio (con todas las filas y columnas originales más las nuevas), el diccionario final, la bitácora en Excel y los datos de esta página. Antes de terminar, el código <b>verifica</b> que no se haya perdido ninguna fila ni columna; si se perdiera alguna, se detiene con error.`,
    hz: () => [`<b>${nf(filasF)}</b> filas (las mismas del inicio) y <b>${nf(colsF)}</b> columnas: ${nf(cols0)} originales + <b>${nf(colsF - cols0)}</b> nuevas.`, `<b>${bit.length}</b> decisiones en la bitácora, <b>${G.length}</b> gráficos, <b>${Object.keys(E.tablas || {}).length}</b> tablas.`],
    chips: () => [[nf(filasF), 'filas finales', C.neon], [nf(colsF), 'columnas finales', C.cyan], [nf(bit.length), 'decisiones documentadas', C.lila]],
    dec: 'Regla de oro cumplida: ninguna fila ni columna eliminada.',
    chart: id => { grow(id, [{ type: 'bar', x: FASES.map(f => f[0]), y: FASES.map(f => bitF(f[0]).length), marker: { color: FASES.map(f => f[3]) }, text: FASES.map(f => f[1]), textposition: 'none', hovertemplate: '%{x} %{text}: %{y} decisiones<extra></extra>' }],
      { yaxis: { title: { text: 'Decisiones en la bitácora' } }, margin: { l: 50, r: 10, t: 10, b: 40 } }, 'y'); },
    cap: 'Decisiones documentadas por fase.',
    code: [['F12_cierre', [['stopifnot("Se perdieron filas"', 0, 1], ['exportar <- function', 0, 0], ['wb <- openxlsx::createWorkbook()', 0, 0]]]] }
};
$('#phnav').innerHTML = FASES.map(f => `<button style="--c:${f[3]}" data-k="${f[0]}">${f[0]} · ${f[1]}</button>`).join('');
$$('#phnav button').forEach(b => b.onclick = () => document.getElementById('ph-' + b.dataset.k).scrollIntoView({ behavior: 'smooth', block: 'start' }));
FASES.forEach(f => {
  const k = f[0], d = PH[k], recs = bitF(k), gs = G.filter(g => g.fase === k);
  const c = el('div', 'card ph2 reveal'); c.id = 'ph-' + k; c.style.setProperty('--c', f[3]);
  c.innerHTML = `<div class="top2"><div class="badge">${k}</div><div><span class="tag" style="--c:${f[3]}">${f[2]}</span><h3 style="margin-top:6px">${f[1]}</h3><div class="sub">${d.sub}</div></div>
    <div class="meta"><span class="tag" style="--c:var(--mut)">${recs.length} decisiones</span>${seg(k) != null ? `<span class="tag" style="--c:var(--mut)">${nf(seg(k), 1)} s</span>` : ''}${d.enf ? '<span class="tag" style="--c:var(--lila)">cambia con el enfoque</span>' : ''}</div></div>
    <div class="body"><div><h4>Qué se hizo</h4><div class="exp">${d.exp()}</div><h4>Qué se encontró</h4><div class="chips" data-ch="${k}"></div><ul class="hz" data-hz="${k}"></ul>
      <div class="dec"><span class="t">Decisión</span><span data-dc="${k}"></span></div></div>
      <div><h4>En un gráfico</h4><div id="ch-ph-${k}" class="chart sm"></div><div class="src">${d.cap}</div><h4>Código central</h4>${codeBox(d.code)}</div></div>
    ${gs.length ? `<details><summary>Gráficos generados por R en esta fase (${gs.length})</summary><div class="in"><div class="thumbs" data-f="${k}"></div></div></details>` : ''}
    <details><summary>Bitácora de la fase (${recs.length} decisiones)</summary><div class="in"><ul>${recs.map(r => `<li><b>${ACC(r.tipo_problema)}</b>: ${ACC(r.accion)}${r.impacto ? ` <span style="color:var(--dim)">· ${ACC(r.impacto)}</span>` : ''}</li>`).join('')}</ul></div></details>`;
  $('#phases').appendChild(c);
  const th = c.querySelector('.thumbs'); if (th) gs.forEach(g => th.appendChild(thumb(g, gs)));
  const fill = () => { c.querySelector(`[data-ch="${k}"]`).innerHTML = d.chips().map(x => chip(x[0], x[1], x[2])).join(''); c.querySelector(`[data-hz="${k}"]`).innerHTML = d.hz().map(x => `<li>${x}</li>`).join('');
    c.querySelector(`[data-dc="${k}"]`).innerHTML = typeof d.dec === 'function' ? d.dec() : d.dec; };
  fill(); lazy('ch-ph-' + k, () => { d.chart('ch-ph-' + k); if (d.enf) onEnf(() => d.chart('ch-ph-' + k)); });
  if (d.enf || k === 'F11') onEnf(fill);
});

/* ================= 06 ANTES / DESPUÉS ================= */
let V1 = 'ylab';
function showV1(v) { V1 = v; $$('#var1 button').forEach(b => b.classList.toggle('on', b.dataset.v === v));
  const a = by(M0, 'variable', v), b = by(M1, 'variable', v), h0 = histOf(v, 'antes'), h1 = histOf(v, 'despues');
  P('ch-hist1', [{ type: 'bar', x: h0.x, width: h0.w, y: h0.y, customdata: h0.cd, name: 'Antes', marker: { color: 'rgba(255,79,163,.5)', line: { color: C.pink, width: 1 } }, hovertemplate: 'Antes · %{customdata[0]:,.1f}–%{customdata[1]:,.1f}: %{y:,}<extra></extra>' },
    { type: 'bar', x: h1.x, width: h1.w, y: h1.y, customdata: h1.cd, name: 'Después', marker: { color: 'rgba(57,255,159,.42)', line: { color: C.neon, width: 1 } }, hovertemplate: 'Después · %{customdata[0]:,.1f}–%{customdata[1]:,.1f}: %{y:,}<extra></extra>' }],
    { barmode: 'overlay', bargap: 0, xaxis: { title: { text: VN[v] + (h0.lg ? ' (escala logarítmica)' : '') }, ...(h0.lg ? LGX : {}) }, yaxis: { title: { text: 'Personas' } }, margin: { l: 60, r: 20, t: 10, b: 60 },
      shapes: [a.mediana, b.mediana].map((m, i) => ({ type: 'line', x0: xpos(h0, m), x1: xpos(h0, m), yref: 'paper', y0: 0, y1: 1, line: { color: i ? C.neon : C.pink, dash: 'dash', width: 2 } })) });
  const rows = [['Datos válidos', 'n', 0, null], ['Faltantes', 'n_na', 0, 'faltante'], ['Mínimo', 'minimo', 1, 'minimo'], ['Máximo', 'maximo', 1, 'maximo'], ['Media', 'media', 1, 'media'], ['Mediana', 'mediana', 1, 'mediana'], ['Moda', 'moda', 1, 'moda'],
    ['Desviación estándar', 'desv_est', 1, 'desv'], ['Q1', 'q1', 1, 'q1'], ['Q3', 'q3', 1, 'q3'], ['Asimetría', 'asimetria', 2, 'asim'], ['Curtosis', 'curtosis', 2, 'curt']];
  table('tb-ad', [['m', 'Medida'], ['a', 'Antes'], ['b', 'Después'], ['d', 'Cambio']], rows.map(r => { const x = a[r[1]], y = b[r[1]], ch = x ? y / x - 1 : null;
    return { m: r[3] ? gl(r[0], r[3]) : r[0], a: nf(x, r[2]), b: `<b style="color:var(--neon)">${nf(y, r[2])}</b>`, d: ch == null || !isFinite(ch) ? (x === 0 && y === 0 ? '=' : '—') : Math.abs(ch) < 0.0005 ? '<span style="color:var(--dim)">igual</span>' : `<span style="color:${Math.abs(ch) > .05 ? C.amber : C.mut}">${(ch >= 0 ? '+' : '') + nf(100 * ch, 1)}%</span>` }; }));
  const why = { ylab: `Los ${nf(a.n_na - b.n_na)} faltantes menos son los ingresos 0 de quienes trabajan sin pago y los ingresos completados. Por eso el mínimo y la moda pasan a 0 y la media baja: ahora sí se cuenta a quien trabaja sin cobrar.`, tothrs: 'Las jornadas imposibles (más de 168 h) pasaron por el ciclo de validación.', aestudio: 'Los años de estudio imposibles para la edad se corrigieron en el ciclo.' };
  setNeon('ad-neon', [[`Media <b>${fv(v, a.media)} → ${fv(v, b.media)}</b>`, C.cyan], [`Mediana <b>${fv(v, a.mediana)} → ${fv(v, b.mediana)}</b>`, C.lila], [`Faltantes <b>${nf(a.n_na)} → ${nf(b.n_na)}</b>`, C.pink], why[v] && [why[v], C.amber]]);
}
$('#var1').innerHTML = VORD.filter(v => M1.some(r => r.variable === v)).map(v => `<button data-v="${v}">${VN[v]}</button>`).join('');
$$('#var1 button').forEach(b => b.onclick = () => showV1(b.dataset.v));
lazy('ch-hist1', () => showV1('ylab'));
onEnf(() => { if ($('#ch-hist1').data) showV1(ENFQ === 'hogar' ? 'yhogpc' : ENFQ === 'oit' ? 'tothrs' : 'ylab'); });
lazy('ch-cal0', () => { grow('ch-cal0', [['Antes', C.pink], ['Despues', C.neon]].map(([k, c]) => ({ type: 'bar', orientation: 'h', name: k === 'Antes' ? 'Antes' : 'Después', y: CAL9.map(r => ACC(r.dimension).split(' (')[0]), x: CAL9.map(r => r[k]), marker: { color: c }, text: CAL9.map(r => pc(r[k], 2)), textposition: 'inside', textfont: { color: '#02040c', size: 11 } })),
  { barmode: 'group', xaxis: { tickformat: '.0%', range: [Math.min(...CAL9.map(r => r.Antes)) * .97, 1.005] }, yaxis: { tickfont: { color: C.txt } }, margin: { l: 110, r: 10, t: 10, b: 50 } }); });
lazy('ch-falt', () => { const F = T('F09_faltantes_antes_despues');
  grow('ch-falt', [{ type: 'bar', x: F.map(r => VN[r.variable] || r.variable), y: F.map(r => r.Antes), name: 'Antes', marker: { color: C.pink } }, { type: 'bar', x: F.map(r => VN[r.variable] || r.variable), y: F.map(r => r.Despues), name: 'Después', marker: { color: C.neon } }],
    { yaxis: { tickformat: '.1%' }, xaxis: { tickfont: { color: C.txt, size: 10 } }, margin: { l: 56, r: 10, t: 10, b: 80 } }, 'y'); });
(function () { const a = PR9.filter(r => r.momento === 'Sin tratar'), b = PR9.filter(r => r.momento === 'Limpio');
  table('tb-pruebas', [['h', 'Pregunta'], ['d0', 'Antes'], ['d1', 'Después']], a.map((r, i) => ({ h: hq(r.hipotesis_nula), d0: r.decision === 'Se rechaza H0' ? 'Hay diferencia' : 'Sin diferencia', d1: b[i] ? `<b style="color:${b[i].decision === r.decision ? 'var(--neon)' : 'var(--amber)'}">${b[i].decision === 'Se rechaza H0' ? 'Hay diferencia' : 'Sin diferencia'}</b>` : '—' }))); })();

/* ================= 07 DOS MIRADAS ================= */
(function () {
  const Q = [['Solo persona', 'Gana menos del salario mínimo, pero su hogar no es pobre', C.pink], ['Ambos enfoques', 'Gana menos del salario mínimo y vive en un hogar pobre', C.lila], ['Ninguno', 'Ni ingreso bajo ni hogar pobre', C.neon], ['Solo hogar', 'Gana el salario mínimo o más, pero su hogar es pobre', C.cyan]];
  $('#quad').innerHTML = Q.map(q => { const r = by(cuad, 'cuadrante', q[0]); return `<div style="--c:${q[2]}" data-q="${q[0]}"><i class="f" style="height:0"></i><span class="q">${q[0]}</span><span class="p">${pc(r.p)}</span><span class="s">${q[1]} · ${nf(r.n / 1e6, 2)} M</span></div>`; }).join('');
  setTimeout(() => $$('#quad .f').forEach(f => { const q = f.parentElement.dataset.q; f.style.height = (100 * (by(cuad, 'cuadrante', q).p || 0) / 0.55) + '%'; }), 600);
  setNeon('quad-neon', [[`${gl('Kappa', 'kappa')} <b>${nf(kAB.kappa, 2)}</b>`, C.lila], [`Coinciden en <b>${pc(kAB.acuerdo)}</b>`, C.lila], [`No coinciden en <b>${pc((by(cuad, 'cuadrante', 'Solo persona').p || 0) + (by(cuad, 'cuadrante', 'Solo hogar').p || 0))}</b> de los ocupados`, C.amber]]);
  $$('#quad > div').forEach(d => d.onclick = () => { $$('#quad > div').forEach(x => x.style.outline = ''); d.style.outline = '2px solid ' + getComputedStyle(d).getPropertyValue('--c'); drawSankey(d.dataset.q); });
})();
function drawSankey(q) { const X = T('F08_cruce_abc'); if (!X.length) return;
  const nodes = ['A: vulnerable', 'A: no vulnerable', 'B: vulnerable', 'B: no vulnerable', 'C: cuenta propia o sin pago', 'C: asalariado u otro'], col = [C.pink, 'rgba(255,79,163,.35)', C.cyan, 'rgba(34,228,255,.35)', C.amber, 'rgba(255,194,77,.35)'];
  const iA = r => r.A === 'Vulnerable' ? 0 : 1, iB = r => r.B === 'Vulnerable' ? 2 : 3, iC = r => r.C === 'Vulnerable' ? 4 : 5, Ls = {};
  const qsel = r => !q || (q === 'Ambos enfoques' && r.A === 'Vulnerable' && r.B === 'Vulnerable') || (q === 'Solo persona' && r.A === 'Vulnerable' && r.B !== 'Vulnerable') || (q === 'Solo hogar' && r.A !== 'Vulnerable' && r.B === 'Vulnerable') || (q === 'Ninguno' && r.A !== 'Vulnerable' && r.B !== 'Vulnerable');
  X.forEach(r => { [[iA(r), iB(r)], [iB(r), iC(r)]].forEach(([s, t]) => { const k = s + '-' + t; Ls[k] = Ls[k] || [s, t, 0, 0]; Ls[k][2] += r.n; if (qsel(r)) Ls[k][3] += r.n; }); });
  const Lv = Object.values(Ls), tot = sum(X.map(r => r.n));
  P('ch-sankey', [{ type: 'sankey', arrangement: 'snap', node: { label: nodes.map(n => n), color: col, pad: 22, thickness: 22, line: { color: '#030714', width: 1 }, hovertemplate: '%{label}: %{value:,.0f}<extra></extra>' },
    link: { source: Lv.map(l => l[0]), target: Lv.map(l => l[1]), value: Lv.map(l => q ? l[3] || 1e-6 : l[2]), color: Lv.map(l => q ? (l[3] ? 'rgba(180,108,255,.55)' : 'rgba(80,90,130,.08)') : (l[0] === 0 || l[0] === 2 ? 'rgba(255,79,163,.28)' : 'rgba(147,163,214,.16)')),
      customdata: Lv.map(l => (q ? l[3] : l[2]) / tot), hovertemplate: '%{source.label} → %{target.label}<br>%{value:,.0f} personas (%{customdata:.1%})<extra></extra>' } }],
    { margin: { l: 10, r: 10, t: 30, b: 10 }, font: { color: C.txt, size: 12, family: FONT }, title: { text: q ? `Seleccionado: ${q}` : 'Todos los ocupados de 19+ (ponderado)', font: { size: 12, color: C.mut } } }); }
lazy('ch-sankey', () => drawSankey(null));
lazy('ch-3dep', () => { const draw = () => { const R = dep.slice().sort((a, b) => (b[enf1().col] || 0) - (a[enf1().col] || 0));
  P('ch-3dep', Object.values(ENF).map(e => ({ type: 'scatter', mode: 'markers', name: e.l, y: R.map(r => dn(r.departamento)), x: R.map(r => r[e.col]), marker: { size: enfs().includes(e) ? 17 : 10, color: e.c, line: { color: '#030714', width: 1 }, opacity: enfs().includes(e) ? 1 : .45 }, hovertemplate: '%{y} · ' + e.l + ': %{x:.1%}<extra></extra>' }))
    .concat([{ type: 'scatter', mode: 'lines', showlegend: false, hoverinfo: 'skip', x: R.flatMap(r => [Math.min(r.tasa_persona, r.tasa_hogar, r.tasa_oit), Math.max(r.tasa_persona, r.tasa_hogar, r.tasa_oit), null]), y: R.flatMap(r => [dn(r.departamento), dn(r.departamento), null]), line: { color: 'rgba(147,163,214,.35)', width: 6 } }]),
    { margin: { l: 100, r: 20, t: 10, b: 60 }, xaxis: { tickformat: '.0%' }, yaxis: { autorange: 'reversed', tickfont: { color: C.txt } } }); };
  draw(); onEnf(draw); });
let grpK = 'sexo';
function drawGrp() {
  const src = { sexo: [T('F11_tasa_vulnerable_sexo'), r => r.sexo], zona: [T('F11_tasa_vulnerable_zona'), r => r.zona], edad: [T('F11_tasa_vulnerable_edad').filter(r => r.n > 30), r => r.edad_grupo], sz: [T('F11_tasa_vulnerable_sexo_zona'), r => r.zona + ' · ' + r.sexo] }[grpK];
  P('ch-grp', Object.values(ENF).map(e => ({ type: 'bar', name: e.l, x: src[0].map(src[1]), y: src[0].map(r => r[e.col]), marker: { color: e.c, opacity: enfs().includes(e) ? 1 : .3 }, text: src[0].map(r => pc(r[e.col], 0)), textposition: 'outside', textfont: { color: C.txt, size: 10 }, hovertemplate: '%{x} · ' + e.l + ': %{y:.1%}<extra></extra>' })),
    { barmode: 'group', yaxis: { tickformat: '.0%', range: [0, 1.05] }, xaxis: { tickfont: { color: C.txt } }, margin: { l: 50, r: 20, t: 10, b: 60 } });
}
lazy('ch-grp', () => { segBtns('seg-grp', [['sexo', 'Sexo'], ['zona', 'Área'], ['edad', 'Edad'], ['sz', 'Área × sexo']], g => { grpK = g; drawGrp(); }); onEnf(drawGrp); });

/* ================= 08 MAPAS ================= */
const GEO = E.geo, GEO_N = { Potosi: 'Potosí', Beni: 'El Beni' };
const centro = {};
if (GEO) GEO.features.forEach(f => { let xs = 0, ys = 0, n = 0; const walk = c => { if (typeof c[0] === 'number') { xs += c[0]; ys += c[1]; n++; } else c.forEach(walk); }; walk(f.geometry.coordinates); centro[f.properties.name] = [xs / n, ys / n]; });
const ivlOf = (d, k) => ivlE.find(r => r.departamento === d && r.enfoque === k) || {};
const DSZ = T('F11_tasa_vulnerable_ds'), DZ = T('F11_tasa_vulnerable_dz'), DZS = T('F11_tasa_vulnerable_dzs');
const MET = {
  tasa_persona: ['Tasa A · persona', d => by(dep, 'departamento', d).tasa_persona, '.1%', [[0, '#5a2a8a'], [.5, '#c0388f'], [1, C.pink]]],
  tasa_hogar: ['Tasa B · hogar', d => by(dep, 'departamento', d).tasa_hogar, '.1%', [[0, '#1f3f8a'], [.5, '#178fb8'], [1, C.cyan]]],
  tasa_oit: ['Tasa C · OIT', d => by(dep, 'departamento', d).tasa_oit, '.1%', [[0, '#6a4a1a'], [.5, '#c08a20'], [1, C.amber]]],
  ivl_persona: ['IVL A (0-100)', d => ivlOf(d, 'persona').ivl, '.0f', [[0, '#2a3a8a'], [.5, C.lila], [1, C.pink]]],
  ivl_hogar: ['IVL B (0-100)', d => ivlOf(d, 'hogar').ivl, '.0f', [[0, '#2a2a7a'], [.5, C.blue], [1, C.cyan]]],
  informal: ['No aporta a la jubilación', d => by(dep, 'departamento', d).informal, '.1%', [[0, '#1a5a4a'], [.5, '#20a070'], [1, C.neon]]],
  ylab_mediana: ['Ingreso mediano (Bs)', d => by(dep, 'departamento', d).ylab_mediana, ',.0f', [[0, '#6a4a1a'], [.5, '#c08a20'], [1, C.amber]]]
};
let MAPK = 'tasa_persona', DSEL = null;
const lerpC = (stops, t) => { t = Math.max(0, Math.min(1, t)); let i = 0; while (i < stops.length - 2 && t > stops[i + 1][0]) i++;
  const [t0, c0] = stops[i], [t1, c1] = stops[i + 1], u = (t - t0) / ((t1 - t0) || 1), h = c => [1, 3, 5].map(k => parseInt(c.slice(k, k + 2), 16)), A = h(c0), B = h(c1);
  return 'rgb(' + A.map((v, k) => Math.round(v + (B[k] - v) * u)).join(',') + ')'; };
let MAP_SVG = null;
function buildMap() {
  if (!GEO) return; let x0 = 1e9, x1 = -1e9, y0 = 1e9, y1 = -1e9; const pr = ([lon, lat]) => [lon, -Math.log(Math.tan(Math.PI / 4 + lat * Math.PI / 360)) * 180 / Math.PI];
  GEO.features.forEach(f => { const w = c => { if (typeof c[0] === 'number') { const [x, y] = pr(c); x0 = Math.min(x0, x); x1 = Math.max(x1, x); y0 = Math.min(y0, y); y1 = Math.max(y1, y); } else c.forEach(w); }; w(f.geometry.coordinates); });
  const W = 600, H = 640, k = Math.min((W - 20) / (x1 - x0), (H - 20) / (y1 - y0)), ox = (W - k * (x1 - x0)) / 2, oy = (H - k * (y1 - y0)) / 2, P2 = c => { const [x, y] = pr(c); return [ox + (x - x0) * k, oy + (y - y0) * k]; };
  const ring = r => 'M' + r.map(c => P2(c).map(v => v.toFixed(1)).join(',')).join('L') + 'Z';
  const path = g => (g.type === 'Polygon' ? [g.coordinates] : g.coordinates).map(pl => pl.map(ring).join('')).join('');
  const nm = n => Object.keys(GEO_N).find(k => GEO_N[k] === n) || n;
  $('#ch-map').innerHTML = `<svg viewBox="0 0 ${W} ${H}" width="100%" style="max-height:600px;display:block;margin:auto">
    ${GEO.features.map(f => `<path class="dep" data-d="${nm(f.properties.name)}" d="${path(f.geometry)}"/>`).join('')}
    ${GEO.features.map(f => { const c = P2(centro[f.properties.name]); return `<g class="lbl" data-d="${nm(f.properties.name)}" transform="translate(${c[0].toFixed(0)},${c[1].toFixed(0)})"><text class="n" text-anchor="middle" y="-4">${dn(nm(f.properties.name))}</text><text class="v" text-anchor="middle" y="14"></text></g>`; }).join('')}</svg>
    <div class="mapleg" id="mapleg"></div><div class="maptip" id="maptip"></div>`;
  MAP_SVG = $('#ch-map svg');
  $$('#ch-map .dep').forEach(p => { p.onclick = () => showDep(p.dataset.d);
    p.onmousemove = e => { const m = MET[MAPK], t = $('#maptip'), r = $('#ch-map').getBoundingClientRect(); t.style.display = 'block'; t.style.left = (e.clientX - r.left + 14) + 'px'; t.style.top = (e.clientY - r.top + 10) + 'px'; t.innerHTML = `<b>${dn(p.dataset.d)}</b><br>${m[0]}: ${fmtM(m, m[1](p.dataset.d))}`; };
    p.onmouseleave = () => $('#maptip').style.display = 'none'; });
}
const fmtM = (m, v) => m[2] === '.1%' ? pc(v) : m[2] === '.0f' ? nf(v, 0) : 'Bs ' + nf(v);
function drawMap() {
  if (!GEO) { $('#ch-map').innerHTML = '<p class="src">Falta datos_aux/bolivia_deptos.geojson</p>'; return; }
  if (!MAP_SVG) buildMap();
  const m = MET[MAPK], z = dep.map(r => m[1](r.departamento)), lo = Math.min(...z), hi = Math.max(...z), glow = m[3][2][1];
  $$('#ch-map .dep').forEach(p => { const v = m[1](p.dataset.d); p.style.fill = lerpC(m[3], (v - lo) / ((hi - lo) || 1)); p.style.setProperty('--g', glow); p.classList.toggle('sel', p.dataset.d === DSEL); });
  $$('#ch-map .lbl').forEach(g => { g.querySelector('.v').textContent = fmtM(m, m[1](g.dataset.d)); });
  $('#mapleg').innerHTML = `<span>${fmtM(m, lo)}</span><i style="background:linear-gradient(90deg,${m[3].map(c => c[1]).join(',')})"></i><span>${fmtM(m, hi)}</span><b>${m[0]}</b>`;
  drawRank();
}
function drawRank() {
  const m = MET[MAPK], R = dep.map(r => [r.departamento, m[1](r.departamento)]).sort((a, b) => b[1] - a[1]), mx = Math.max(...R.map(r => r[1]));
  const col = MAPK.includes('hogar') ? C.cyan : MAPK.includes('oit') || MAPK.includes('ylab') ? C.amber : MAPK === 'informal' ? C.neon : C.pink;
  if (!DSEL) $('#dpanel').innerHTML = `<h3>${m[0]}</h3><p>Toca un departamento del mapa o de la lista.</p><div class="rank" style="--c:${col}">${R.map((r, i) => `<div class="r" data-d="${r[0]}"><span>${i + 1}. ${dn(r[0])}</span><span class="bar"><i style="width:${100 * r[1] / mx}%"></i></span><span class="pv">${fmtM(m, r[1])}</span></div>`).join('')}</div>`;
  $$('#dpanel .r').forEach(x => x.onclick = () => showDep(x.dataset.d));
}
function showDep(d) {
  DSEL = d; const r = by(dep, 'departamento', d), iA = ivlOf(d, 'persona'), iB = ivlOf(d, 'hogar');
  const h = DSZ.find(x => x.departamento === d && x.sexo === 'Hombre') || {}, mj = DSZ.find(x => x.departamento === d && x.sexo === 'Mujer') || {};
  const u = DZ.find(x => x.departamento === d && x.zona === 'Urbana') || {}, ru = DZ.find(x => x.departamento === d && x.zona === 'Rural') || {};
  const rk = col => 1 + dep.filter(x => x[col] > r[col]).length;
  $('#dpanel').innerHTML = `<span class="tag" style="--c:${C.lila}">Ficha departamental</span><h3 style="font-size:30px;margin:10px 0 4px">${dn(d)}</h3><p class="src" style="margin:0">${nf(r.n)} ocupados de 19+ en la muestra · ${nf(r.poblacion)} representados</p>
    <div class="grid g3" style="margin-top:14px">${chip(pc(r.tasa_persona), `A · puesto ${rk('tasa_persona')} de 9`, C.pink)}${chip(pc(r.tasa_hogar), `B · puesto ${rk('tasa_hogar')} de 9`, C.cyan)}${chip(pc(r.tasa_oit), `C · puesto ${rk('tasa_oit')} de 9`, C.amber)}</div>
    <div class="neons">${neon(`IVL A <b>${nf(iA.ivl, 0)}</b> (#${iA.rango})`, C.pink)}${neon(`IVL B <b>${nf(iB.ivl, 0)}</b> (#${iB.rango})`, C.cyan)}${neon(`No aporta <b>${pc(r.informal)}</b>`, C.neon)}${neon(`Ingreso mediano <b>Bs ${nf(r.ylab_mediana)}</b>`, C.amber)}</div>
    <div class="neons">${neon(`Mujeres A <b>${pc(mj.tasa_persona)}</b> · hombres <b>${pc(h.tasa_persona)}</b>`, C.pink)}${neon(`Rural A <b>${pc(ru.tasa_persona)}</b> · urbana <b>${pc(u.tasa_persona)}</b>`, C.amber)}${neon(`Rural B <b>${pc(ru.tasa_hogar)}</b> · urbana <b>${pc(u.tasa_hogar)}</b>`, C.cyan)}</div>
    <div id="ch-depmini" class="chart sm"></div><button class="btn" id="dep-back">← Ranking</button>`;
  P('ch-depmini', [{ type: 'scatterpolar', fill: 'toself', name: 'IVL A', r: ['desempleo', 'subempleo', 'informalidad', 'baja_educ', 'tasa_enfoque'].map(k => iA[k]), theta: ['Desempleo', 'Menos de 40 h', 'No aporta', 'Poca educación', 'Tasa A'], line: { color: C.pink }, fillcolor: 'rgba(255,79,163,.2)' },
    { type: 'scatterpolar', fill: 'toself', name: 'IVL B', r: ['desempleo', 'subempleo', 'informalidad', 'baja_educ', 'tasa_enfoque'].map(k => iB[k]), theta: ['Desempleo', 'Menos de 40 h', 'No aporta', 'Poca educación', 'Tasa B'], line: { color: C.cyan }, fillcolor: 'rgba(34,228,255,.15)' }],
    { polar: { bgcolor: 'rgba(0,0,0,0)', radialaxis: { tickformat: '.0%', gridcolor: C.grid, tickfont: { color: C.dim, size: 9 } }, angularaxis: { gridcolor: C.grid, tickfont: { color: C.txt, size: 11 } } }, margin: { l: 40, r: 40, t: 20, b: 20 }, legend: { orientation: 'h', y: -0.1 } });
  $('#dep-back').onclick = () => { DSEL = null; drawMap(); };
  drawMap();
}
lazy('ch-map', () => segBtns('seg-map', Object.entries(MET).map(([k, v]) => [k, v[0]]), k => { MAPK = k; drawMap(); }));
onEnf(() => { const k = { persona: 'tasa_persona', hogar: 'tasa_hogar', oit: 'tasa_oit', comparar: 'ivl_persona' }[ENFQ]; if ($('#seg-map button')) segSet('seg-map', k); });
lazy('ch-ivlcomp', () => { const draw = () => { const e = enfAB(), R = ivlE.filter(r => r.enfoque === e.k).sort((a, b) => b.ivl - a.ivl), ks = ['desempleo', 'subempleo', 'informalidad', 'baja_educ', 'tasa_enfoque'];
  const nm = ['Desempleo', 'Menos de 40 h', 'No aporta', 'Poca educación', 'Tasa ' + e.s], zz = ks.map(k => { const v = R.map(r => r[k]), lo = Math.min(...v), hi = Math.max(...v); return v.map(x => (x - lo) / ((hi - lo) || 1)); });
  P('ch-ivlcomp', [{ type: 'heatmap', x: nm.concat(['IVL']), y: R.map(r => dn(r.departamento) + ' (#' + r.rango + ')'), z: R.map((r, i) => zz.map(c => c[i]).concat([r.ivl / 100])),
    text: R.map(r => ks.map(k => pc(r[k], 1)).concat([nf(r.ivl, 0)])), texttemplate: '%{text}', textfont: { color: '#fff', size: 11 }, colorscale: [[0, '#0b1a4a'], [.5, C.lila], [1, e.c]], showscale: false, hovertemplate: '%{y} · %{x}: %{text}<extra></extra>', xgap: 3, ygap: 3 }],
    { margin: { l: 130, r: 10, t: 10, b: 60 }, yaxis: { autorange: 'reversed', tickfont: { color: C.txt } }, xaxis: { tickfont: { color: C.txt } } }); };
  draw(); onEnf(draw); });
lazy('ch-heat', () => { const draw = () => { const e = enf1(), cols = [['Rural', 'Mujer'], ['Rural', 'Hombre'], ['Urbana', 'Mujer'], ['Urbana', 'Hombre']];
  const D = dep.slice().sort((a, b) => b[e.col] - a[e.col]).map(r => r.departamento);
  const Z = D.map(d => cols.map(c => (DZS.find(x => x.departamento === d && x.zona === c[0] && x.sexo === c[1]) || {})[e.col]));
  P('ch-heat', [{ type: 'heatmap', x: cols.map(c => c.join(' · ')), y: D.map(dn), z: Z, text: Z.map(r => r.map(v => pc(v, 0))), texttemplate: '%{text}', textfont: { color: '#fff', family: FHEAD, size: 13 },
    colorscale: [[0, '#0b1a4a'], [.5, C.lila], [1, e.c]], colorbar: { thickness: 10, tickformat: '.0%', tickfont: { color: C.mut } }, xgap: 4, ygap: 4, hovertemplate: '%{y} · %{x}: %{z:.1%}<extra></extra>' }],
    { margin: { l: 100, r: 10, t: 30, b: 50 }, yaxis: { autorange: 'reversed', tickfont: { color: C.txt } }, xaxis: { tickfont: { color: C.txt } }, title: { text: 'Enfoque ' + e.l, font: { size: 12, color: e.c } } }); };
  draw(); onEnf(draw); });

/* ================= 09 OCUPACIONES ================= */
const ICO = {
  'Directivos': '<path d="M4 8h16v11H4z"/><path d="M9 8V5h6v3"/><path d="M4 13h16"/>',
  'Profesionales': '<path d="M2 9l10-5 10 5-10 5z"/><path d="M6 11v5c3 2 9 2 12 0v-5"/>',
  'Tecnicos de nivel medio': '<circle cx="12" cy="12" r="3"/><path d="M12 2v3M12 19v3M2 12h3M19 12h3M5 5l2 2M17 17l2 2M5 19l2-2M17 7l2-2"/>',
  'Empleados de oficina': '<rect x="3" y="4" width="18" height="12" rx="1"/><path d="M8 20h8M12 16v4"/>',
  'Servicios y vendedores': '<path d="M3 9l2-5h14l2 5"/><path d="M4 9v11h16V9"/><path d="M9 20v-6h6v6"/>',
  'Agricultores y pescadores': '<path d="M12 22V10"/><path d="M12 14c-4 0-6-3-6-6 4 0 6 3 6 6zM12 12c4 0 6-3 6-6-4 0-6 3-6 6z"/>',
  'Oficios de construccion e industria': '<path d="M3 21h18"/><path d="M5 21V10l7-5 7 5v11"/><path d="M10 21v-5h4v5"/>',
  'Operadores de maquinaria': '<circle cx="7" cy="17" r="3"/><circle cx="17" cy="17" r="3"/><path d="M10 17h4M3 17V9h11l3 4h4v4"/>',
  'Ocupaciones elementales': '<path d="M7 21l4-9"/><path d="M11 12l6-8 3 2-6 8z"/>',
  'Fuerzas armadas': '<path d="M12 3l8 3v6c0 5-4 8-8 9-4-1-8-4-8-9V6z"/>'
};
const GRP = T('F11_tasa_vulnerable_grupo'), OSC = T('F11_ocupacion_categoria'), OSAL = T('F10_ocupacion_salario');
let OCSEL = null;
function drawOcs() { const e = enf1(), R = GRP.slice().sort((a, b) => b.poblacion - a.poblacion), mx = Math.max(...R.map(r => r.poblacion));
  $('#ocs').innerHTML = R.map((r, i) => `<div class="oc ${OCSEL === r.grupo_ocup ? 'on' : ''}" data-g="${r.grupo_ocup}" style="--c:${PAL[i % PAL.length]}"><svg viewBox="0 0 24 24">${ICO[r.grupo_ocup] || ICO['Ocupaciones elementales']}</svg>
    <div class="n">${ACC(r.grupo_ocup)}</div><div class="s">Bs ${nf(r.ylab_mediana)} <small>mediana/mes</small></div><div class="bar"><i style="width:${100 * r.poblacion / mx}%"></i></div>
    <div class="r">${nf(r.poblacion / 1e3)} mil ocupados</div><div class="r">${enfs().map(x => `${x.s}: <b style="color:${x.c}">${pc(r[x.col], 0)}</b>`).join(' · ')} · no aporta ${pc(r.informal, 0)}</div></div>`).join('');
  $$('.oc').forEach(o => o.onclick = () => { OCSEL = OCSEL === o.dataset.g ? null : o.dataset.g; drawOcs(); drawSun(); }); }
function drawSun() { const e = enf1(), ids = [], lab = [], par = [], val = [], col = [], cd = [];
  GRP.forEach(g => { const k = g.grupo_ocup, ch = OSC.filter(r => r.grupo_ocup === k); if (!ch.length) return;
    ids.push(k); lab.push(ACC(k)); par.push(''); val.push(sum(ch.map(r => r.poblacion))); col.push(g[e.col]); cd.push([g[e.col], g.ylab_mediana, g.informal]);
    ch.forEach(r => { ids.push(k + '/' + r.cat_ocupacional); lab.push(ACC(r.cat_ocupacional)); par.push(k); val.push(r.poblacion); col.push(r[e.col]); cd.push([r[e.col], r.ylab_mediana, r.informal]); }); });
  P('ch-sun', [{ type: 'sunburst', ids, labels: lab, parents: par, values: val, branchvalues: 'total', level: OCSEL || '', maxdepth: 2, customdata: cd, marker: { colors: col, colorscale: [[0, '#0b2a4a'], [.5, C.lila], [1, e.c]], cmin: 0, cmax: 1, line: { color: '#030714', width: 1.5 }, showscale: true, colorbar: { title: { text: 'Tasa ' + e.s, font: { color: C.mut } }, tickformat: '.0%', thickness: 10, tickfont: { color: C.mut } } },
    hovertemplate: '<b>%{label}</b><br>%{value:,.0f} ocupados<br>Tasa ' + e.s + ': %{customdata[0]:.1%}<br>Ingreso mediano Bs %{customdata[1]:,.0f}<br>No aporta: %{customdata[2]:.0%}<extra></extra>', insidetextfont: { color: '#fff', family: FHEAD } }],
    { margin: { l: 0, r: 0, t: 0, b: 0 } }); }
lazy('ocs', () => { drawOcs(); onEnf(drawOcs); });
lazy('ch-sun', () => { drawSun(); onEnf(drawSun); });
lazy('ch-ocup', () => { const gr = [...new Set(OSAL.map(r => r.grupo_ocup))];
  const med = g => sum(OSAL.filter(r => r.grupo_ocup === g).map(r => r.mediana * r.poblacion)) / sum(OSAL.filter(r => r.grupo_ocup === g).map(r => r.poblacion));
  gr.sort((a, b) => med(a) - med(b)); const v = (g, s) => (OSAL.find(r => r.grupo_ocup === g && r.sexo === s) || {});
  P('ch-ocup', [{ type: 'scatter', mode: 'lines', showlegend: false, hoverinfo: 'skip', y: gr.flatMap(g => [ACC(g), ACC(g), null]), x: gr.flatMap(g => [v(g, 'Hombre').mediana, v(g, 'Mujer').mediana, null]), line: { color: 'rgba(147,163,214,.4)', width: 6 } },
    ...['Hombre', 'Mujer'].map(s => ({ type: 'scatter', mode: 'markers', name: s === 'Hombre' ? 'Hombres' : 'Mujeres', y: gr.map(ACC), x: gr.map(g => v(g, s).mediana), marker: { size: 14, color: s === 'Hombre' ? C.cyan : C.pink, line: { color: '#030714', width: 1 } },
      customdata: gr.map(g => [v(g, s).p25, v(g, s).p75, v(g, s).bajo_smn, v(g, s).n, v(g, s).ingreso_hora]), hovertemplate: '%{y} · ' + s + '<br>Mediana Bs %{x:,.0f} (25%: %{customdata[0]:,.0f} · 75%: %{customdata[1]:,.0f})<br>Bajo el salario mínimo: %{customdata[2]:.1%} · Bs %{customdata[4]:.1f}/hora<extra></extra>' }))],
    { margin: { l: 210, r: 20, t: 10, b: 60 }, xaxis: { title: { text: 'Ingreso laboral mediano (Bs/mes)' }, tickformat: ',' }, yaxis: { tickfont: { color: C.txt } },
      shapes: [{ type: 'line', x0: SMN, x1: SMN, yref: 'paper', y0: 0, y1: 1, line: { color: C.amber, dash: 'dash', width: 2 } }], annotations: [{ x: SMN, yref: 'paper', y: 1.02, text: 'Salario mínimo', showarrow: false, font: { color: C.amber, size: 11 } }] }); });

/* ================= 10 PATRONES ================= */
const BS = T('F11_brecha_salarial_genero'), bsH = by(BS, 'sexo', 'Hombre'), bsM = by(BS, 'sexo', 'Mujer');
const sH = by(sx, 'sexo', 'Hombre'), sM = by(sx, 'sexo', 'Mujer'), zU = by(zon, 'zona', 'Urbana'), zR = by(zon, 'zona', 'Rural');
function drawBig() { const e = enf1(), e2 = ENFQ === 'comparar' ? ENF.hogar : null, D = dep.slice().sort((a, b) => b[e.col] - a[e.col]), CT = (x => x.length ? x : T('F11_tasa_vulnerable_rama').filter(r => r.n > 100).map(r => ({ ...r, cat_ocupacional: r.rama })).sort((a, b) => b[e.col] - a[e.col]))(T('F11_tasa_vulnerable_cat').filter(r => r.n > 100 && r[e.col] < .99 && r[e.col] > .01).sort((a, b) => b[e.col] - a[e.col])), ED = T('F11_tasa_vulnerable_educ');
  const sub = (f) => e2 ? `<div class="s">${e2.l}: <b style="color:${e2.c}">${f(e2)}</b></div>` : '';
  const B = [[pc(sM[e.col], 0), `de las mujeres ocupadas ${e.corto}`, `hombres: ${pc(sH[e.col], 0)}`, C.pink, x => pc(sM[x.col], 0)],
    [pc(zR[e.col], 0), `en el área rural ${e.corto}`, `área urbana: ${pc(zU[e.col], 0)}`, C.amber, x => pc(zR[x.col], 0)],
    [pc(D[0][e.col], 0), `en ${dn(D[0].departamento)}, el departamento más alto`, `el más bajo: ${dn(D[D.length - 1].departamento)} ${pc(D[D.length - 1][e.col], 0)}`, C.lila, x => pc(by(dep, 'departamento', D[0].departamento)[x.col], 0)],
    [pc(CT[0][e.col], 0), `en “${ACC(CT[0].cat_ocupacional).toLowerCase()}”`, `${nf(CT[0].poblacion / 1e3)} mil personas`, C.cyan, x => pc(CT[0][x.col], 0)]];
  $('#bign').innerHTML = B.map(b => `<div class="bn" style="--c:${b[3]}"><div class="v">${b[0]}</div><div class="t">${b[1]}</div><div class="s">${b[2]}</div>${sub(b[4])}</div>`).join(''); }
lazy('bign', () => { drawBig(); onEnf(drawBig); });
lazy('ch-gen', () => { const draw = () => P('ch-gen', Object.values(ENF).map(e => ({ type: 'bar', name: e.l, x: ['Hombres', 'Mujeres'], y: [sH[e.col], sM[e.col]], marker: { color: e.c, opacity: enfs().includes(e) ? 1 : .3 }, text: [pc(sH[e.col]), pc(sM[e.col])], textposition: 'outside', textfont: { color: C.txt }, hovertemplate: '%{x} · ' + e.l + ': %{y:.1%}<extra></extra>' })),
    { barmode: 'group', yaxis: { tickformat: '.0%', range: [0, 1] }, xaxis: { tickfont: { color: C.txt, size: 13 } }, margin: { l: 50, r: 20, t: 10, b: 60 } });
  draw(); onEnf(draw);
  setNeon('gen-neon', [[`Ingreso mediano: mujeres <b>Bs ${nf(bsM.mediana_mes)}</b> · hombres <b>Bs ${nf(bsH.mediana_mes)}</b>`, C.amber], [`Por hora: <b>Bs ${nf(bsM.mediana_hora, 1)}</b> vs <b>Bs ${nf(bsH.mediana_hora, 1)}</b>`, C.amber],
    [`Horas promedio: <b>${nf(bsM.horas_media, 1)}</b> vs <b>${nf(bsH.horas_media, 1)}</b>`, C.lila], [`A igual educación, experiencia, horas y región la mujer gana <b>${nf(Math.abs(bsM.brecha_condicionada_mincer_pct), 1)}% menos</b>`, C.neon]]); });
lazy('ch-rur', () => { const SZ = T('F11_tasa_vulnerable_sexo_zona'); const draw = () => { const e = enfs();
  P('ch-rur', e.flatMap(en => ['Urbana', 'Rural'].map(z => ({ type: 'bar', name: `${en.s} · ${z}`, x: ['Hombre', 'Mujer'], y: ['Hombre', 'Mujer'].map(s => (SZ.find(r => r.zona === z && r.sexo === s) || {})[en.col]),
    marker: { color: en.c, opacity: z === 'Rural' ? 1 : .45, line: { color: en.c, width: 1.5 } }, text: ['Hombre', 'Mujer'].map(s => pc((SZ.find(r => r.zona === z && r.sexo === s) || {})[en.col], 0)), textposition: 'outside', textfont: { color: C.txt, size: 11 }, hovertemplate: `${z} · %{x} · ${en.l}: %{y:.1%}<extra></extra>` }))),
    { barmode: 'group', yaxis: { tickformat: '.0%', range: [0, 1.05] }, xaxis: { tickfont: { color: C.txt } }, margin: { l: 50, r: 20, t: 10, b: 60 } }); };
  draw(); onEnf(draw);
  setNeon('rur-neon', [[`Ingreso mediano rural <b>Bs ${nf(zR.ylab_mediana)}</b> · urbano <b>Bs ${nf(zU.ylab_mediana)}</b>`, C.neon], [`No aporta: rural <b>${pc(zR.informal, 0)}</b> · urbano <b>${pc(zU.informal, 0)}</b>`, C.amber]]); });
lazy('ch-edu', () => { const R = T('F11_ingreso_educacion_sexo'), lv = [...new Set(R.map(r => r.nivel_educ))];
  grow('ch-edu', ['Hombre', 'Mujer'].map(s => ({ type: 'bar', name: s === 'Hombre' ? 'Hombres' : 'Mujeres', x: lv.map(ACC), y: lv.map(l => (R.find(r => r.nivel_educ === l && r.sexo === s) || {}).mediana), marker: { color: s === 'Hombre' ? C.cyan : C.pink },
    text: lv.map(l => 'Bs ' + nf((R.find(r => r.nivel_educ === l && r.sexo === s) || {}).mediana)), textposition: 'outside', textfont: { color: C.txt, size: 10 }, hovertemplate: '%{x} · ' + s + ': Bs %{y:,.0f}<extra></extra>' })),
    { barmode: 'group', yaxis: { tickformat: ',', title: { text: 'Bs/mes (mediana)' } }, xaxis: { tickfont: { color: C.txt } }, margin: { l: 60, r: 20, t: 10, b: 70 },
      shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: SMN, y1: SMN, line: { color: C.amber, dash: 'dash' } }] }, 'y'); });
lazy('ch-rama', () => { const R = T('F11_tasa_vulnerable_rama'); const draw = () => { const e = enf1(), S = R.slice().sort((a, b) => a[e.col] - b[e.col]);
  P('ch-rama', [{ type: 'bar', orientation: 'h', y: S.map(r => ACC(r.rama)), x: S.map(r => r[e.col]), marker: { color: S.map(r => r[e.col]), colorscale: [[0, '#0b1a4a'], [.5, C.lila], [1, e.c]] }, text: S.map(r => pc(r[e.col], 0)), textposition: 'outside', textfont: { color: C.txt, size: 10 },
    customdata: S.map(r => [r.ylab_mediana, r.n]), hovertemplate: '%{y}: %{x:.1%}<br>Ingreso mediano Bs %{customdata[0]:,.0f}<extra></extra>' }],
    { margin: { l: 220, r: 40, t: 10, b: 40 }, xaxis: { tickformat: '.0%', range: [0, 1.1] }, yaxis: { tickfont: { color: C.txt, size: 11 } } }); };
  draw(); onEnf(draw); });
lazy('ch-clu3d', () => { const S = T('F10_clusters_muestra_3d'), pf = [...new Set(S.map(r => r.perfil))].sort();
  const h3 = $('#ch-clu3d').parentElement.querySelector('h3'); if (h3) h3.textContent = `${['', '', 'Dos', 'Tres', 'Cuatro', 'Cinco', 'Seis', 'Siete'][CLP.length] || CLP.length} tipos de trabajador (k-means) en 3D`;
  const draw = () => { const e = enfAB(), k = e.k === 'hogar' ? 'vul_B' : 'vul_A';
    P('ch-clu3d', pf.map((p, i) => { const R = S.filter(r => r.perfil === p); return { type: 'scatter3d', mode: 'markers', name: ACC(p), x: R.map(r => r.educacion), y: R.map(r => r.edad), z: R.map(r => Math.min(r.ingreso_bs, 15000)),
      marker: { size: R.map(r => r[k] ? 4.2 : 2.6), color: PAL[i % PAL.length], opacity: .8, symbol: R.map(r => r[k] ? 'diamond' : 'circle'), line: { width: 0 } }, customdata: R.map(r => [r.horas, r[k] ? 'Vulnerable (' + e.s + ')' : 'No vulnerable (' + e.s + ')']),
      hovertemplate: ACC(p) + '<br>%{x} años de estudio · %{y} años<br>Bs %{z:,.0f} · %{customdata[0]} h<br>%{customdata[1]}<extra></extra>' }; }),
      { scene: { ...SC3, xaxis: { ...SC3.xaxis, title: { text: 'Años de estudio' } }, yaxis: { ...SC3.yaxis, title: { text: 'Edad' } }, zaxis: { ...SC3.zaxis, title: { text: 'Ingreso (Bs)' } }, camera: { eye: { x: 1.6, y: 1.4, z: .8 } } }, margin: { l: 0, r: 0, t: 0, b: 0 }, legend: { orientation: 'h', y: 0, font: { color: C.txt, size: 10 } } }); };
  draw(); onEnf(draw);
  setNeon('clu-neon', CLP.map((r, i) => [`${ACC(String(r.perfil).split(':')[0])}: A <b>${pc(r.tasa_vulnerable, 0)}</b> · B <b>${pc(r.tasa_hogar, 0)}</b>`, PAL[i % PAL.length]]).concat([['◆ = vulnerable con el enfoque elegido', C.txt]])); });
(function () { const draw = () => { const e = enfAB(), lab = e.k === 'hogar' ? 'B Hogar vulnerable' : 'A Persona vulnerable';
  const vl = s => String(s).split(' & ').map(it => { const [k, v] = it.split('='); return ({ area: 'área ' + (v || '').toLowerCase(), region: 'región ' + v, sexo: v === 'Mujer' ? 'mujer' : 'hombre', edad: v + ' años', educacion: ACC(v).toLowerCase(), categoria: ACC(v).toLowerCase(), informal: 'no aporta a la Gestora', jefe: v === 'jefe' ? 'jefe de hogar' : 'no es jefe de hogar' })[k] || ACC(it); }).join(' & ');
  table('tb-reglas', [['antecedente', 'Si la persona es…', v => `<span style="font-size:12.5px">${vl(v).replace(/ & /g, ' <b style="color:var(--lila)">+</b> ')}</span>`], ['confianza', '…es vulnerable', v => `<b>${pc(v, 0)}</b>`], ['lift', gl('Lift', 'lift'), v => `<b style="color:${e.c}">×${nf(v, 2)}</b>`], ['n', 'Casos', v => nf(v)]],
    RGL.filter(r => r.consecuente === lab && r.confianza < 0.995).sort((a, b) => b.lift - a.lift).slice(0, 15)); };
  draw(); onEnf(draw); })();
lazy('ch-coh', () => { const R = T('F10_cohortes').filter(r => r.n >= 30), yrs = R.map(r => r.anio_nac), step = 3, fr = [];
  for (let k = yrs[0] + 6; k <= yrs[yrs.length - 1] + step; k += step) { const S = R.filter(r => r.anio_nac <= k);
    fr.push({ name: String(Math.min(k, yrs[yrs.length - 1])), data: [{ x: S.map(r => r.anio_nac), y: S.map(r => r.tasa_vulnerable) }, { x: S.map(r => r.anio_nac), y: S.map(r => r.tasa_hogar) }, { x: S.map(r => r.anio_nac), y: S.map(r => r.educacion) }] }); }
  const S0 = R;
  Plotly.newPlot('ch-coh', [{ x: S0.map(r => r.anio_nac), y: S0.map(r => r.tasa_vulnerable), name: 'Tasa A', mode: 'lines+markers', line: { color: C.pink, width: 3, shape: 'spline' }, marker: { size: 5 } },
    { x: S0.map(r => r.anio_nac), y: S0.map(r => r.tasa_hogar), name: 'Tasa B', mode: 'lines+markers', line: { color: C.cyan, width: 3, shape: 'spline' }, marker: { size: 5 } },
    { x: S0.map(r => r.anio_nac), y: S0.map(r => r.educacion), name: 'Años de estudio', yaxis: 'y2', mode: 'lines', line: { color: C.neon, width: 3, shape: 'spline', dash: 'dot' } }],
    L({ xaxis: { title: { text: 'Año de nacimiento' }, range: [yrs[0] - 1, yrs[yrs.length - 1] + 1] }, yaxis: { tickformat: '.0%', title: { text: 'Tasa' }, range: [0, 1] }, yaxis2: { ...AX, overlaying: 'y', side: 'right', title: { text: 'Años de estudio' }, range: [0, 16], gridcolor: 'rgba(0,0,0,0)' }, margin: { l: 56, r: 56, t: 10, b: 110 },
      updatemenus: [{ type: 'buttons', showactive: false, x: 0, y: -0.28, xanchor: 'left', bgcolor: '#0a1430', bordercolor: C.cyan, font: { color: C.cyan }, buttons: [{ label: '▶ Reproducir', method: 'animate', args: [fr.map(f => f.name), { frame: { duration: 220, redraw: false }, transition: { duration: 200 }, mode: 'immediate' }] }] }],
      sliders: [{ active: fr.length - 1, x: .17, len: .83, y: -0.2, pad: { t: 0 }, bgcolor: '#0a1430', bordercolor: C.grid, font: { color: C.mut }, currentvalue: { prefix: 'Nacidos hasta ', font: { color: C.txt } }, steps: fr.map(f => ({ label: f.name, method: 'animate', args: [[f.name], { frame: { duration: 0, redraw: false }, mode: 'immediate' }] })) }] }), CFG)
    .then(g => Plotly.addFrames(g, fr)); });
$('#hall').innerHTML = (E.hallazgos || []).map(h => `<li>${ACC(h.replace(/^\s*\d+\.\s*/, '')).replace(/\b(cat_ocupacional|tothrs|carga_hogar|grupo_ocup|aestudio|departamento|rama)\b/g, w => ({ cat_ocupacional: 'categoría ocupacional', tothrs: 'horas trabajadas', carga_hogar: 'miembros por ocupado del hogar', grupo_ocup: 'grupo ocupacional', aestudio: 'años de estudio', departamento: 'departamento', rama: 'rama de actividad' })[w])}</li>`).join('');

/* ================= 11 MINCER ================= */
const bz = t => (MZE.find(r => r.termino === t) || {}).b || 0;
const MZS = { aestudio: 12, edad: 35, horas: 48, sexo: 'Hombre', zona: 'Urbana', dep: 'La Paz' };
function mzPred(s) { const ex = Math.max(s.edad - s.aestudio - 6, 0);
  return Math.exp(bz('(Intercept)') + bz('aestudio') * s.aestudio + bz('exper') * ex + bz('exper2') * ex * ex + (s.sexo === 'Mujer' ? bz('sexoMujer') : 0) + (s.zona === 'Rural' ? bz('zonaRural') : 0) + bz('lhoras') * Math.log(s.horas) + bz('departamento' + s.dep)); }
function mz3d() { const ae = Array.from({ length: 23 }, (_, i) => i), ed = Array.from({ length: 27 }, (_, i) => 19 + 2 * i);
  P('ch-mz3d', [{ type: 'surface', x: ae, y: ed, z: ed.map(e => ae.map(a => a + 6 > e ? null : mzPred({ ...MZS, aestudio: a, edad: e }))), colorscale: [[0, '#1f2a6a'], [.4, C.lila], [.75, C.pink], [1, C.amber]], showscale: false, opacity: .95,
    contours: { z: { show: true, usecolormap: true, highlightcolor: '#fff', project: { z: true } } }, hovertemplate: '%{x} años de estudio · %{y} años<br>Bs %{z:,.0f}<extra></extra>' },
    { type: 'scatter3d', mode: 'markers', x: [MZS.aestudio], y: [MZS.edad], z: [mzPred(MZS)], marker: { size: 7, color: C.neon, line: { color: '#fff', width: 2 } }, hovertemplate: 'Perfil elegido: Bs %{z:,.0f}<extra></extra>' }],
    { scene: { ...SC3, xaxis: { ...SC3.xaxis, title: { text: 'Años de estudio' } }, yaxis: { ...SC3.yaxis, title: { text: 'Edad' } }, zaxis: { ...SC3.zaxis, title: { text: 'Bs/mes' } }, camera: { eye: { x: -1.6, y: -1.5, z: .9 } } }, margin: { l: 0, r: 0, t: 0, b: 0 }, showlegend: false }); }
(function () {
  const f = $('#mz-form'); if (!MZE.length) return;
  f.innerHTML = `<div><label>Años de estudio <span class="rv" id="mz-ae-v"></span></label><input type="range" id="mz-ae" min="0" max="22" step="1" value="${MZS.aestudio}"></div>
    <div><label>Edad <span class="rv" id="mz-ed-v"></span></label><input type="range" id="mz-ed" min="19" max="70" step="1" value="${MZS.edad}"></div>
    <div><label>Horas por semana <span class="rv" id="mz-h-v"></span></label><input type="range" id="mz-h" min="8" max="84" step="1" value="${MZS.horas}"></div>
    <div><label>Sexo</label><select id="mz-sx"><option>Hombre</option><option>Mujer</option></select></div>
    <div><label>Área</label><select id="mz-zn"><option>Urbana</option><option>Rural</option></select></div>
    <div><label>Departamento</label><select id="mz-dp">${dep.map(r => r.departamento).map(d => `<option value="${d}" ${d === MZS.dep ? 'selected' : ''}>${dn(d)}</option>`).join('')}</select></div>`;
  const upd = () => { MZS.aestudio = +$('#mz-ae').value; MZS.edad = +$('#mz-ed').value; MZS.horas = +$('#mz-h').value; MZS.sexo = $('#mz-sx').value; MZS.zona = $('#mz-zn').value; MZS.dep = $('#mz-dp').value;
    $('#mz-ae-v').textContent = MZS.aestudio; $('#mz-ed-v').textContent = MZS.edad; $('#mz-h-v').textContent = MZS.horas;
    const y = mzPred(MZS), y1 = mzPred({ ...MZS, aestudio: MZS.aestudio + 1 }), ex = Math.max(MZS.edad - MZS.aestudio - 6, 0);
    $('#mz-y').textContent = 'Bs ' + nf(y); $('#mz-s').textContent = `experiencia estimada ${ex} años · ${y < SMN ? 'por debajo' : 'por encima'} del salario mínimo`;
    $('#mz-d').textContent = (y1 / y - 1 >= 0 ? '+' : '') + nf(100 * (y1 / y - 1), 1) + '%'; $('#mz-d2').textContent = `Bs ${nf(y1 - y)} más al mes (a la misma edad)`;
    const xs = Array.from({ length: 21 }, (_, i) => i);
    P('ch-mz', ['Hombre', 'Mujer'].map(s => ({ x: xs, y: xs.map(a => mzPred({ ...MZS, aestudio: a, sexo: s })), name: s === 'Hombre' ? 'Hombres' : 'Mujeres', mode: 'lines', line: { color: s === 'Hombre' ? C.cyan : C.pink, width: s === MZS.sexo ? 4 : 2, shape: 'spline' }, hovertemplate: '%{x} años de estudio: Bs %{y:,.0f}<extra>' + s + '</extra>' }))
      .concat([{ x: [MZS.aestudio], y: [y], mode: 'markers', showlegend: false, marker: { size: 16, color: C.neon, line: { color: '#fff', width: 2 } }, hovertemplate: 'Perfil elegido: Bs %{y:,.0f}<extra></extra>' }]),
      { xaxis: { title: { text: 'Años de estudio' } }, yaxis: { title: { text: 'Ingreso laboral estimado (Bs/mes)' }, tickformat: ',' }, margin: { l: 70, r: 20, t: 10, b: 60 },
        shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: SMN, y1: SMN, line: { color: C.amber, dash: 'dash' } }], annotations: [{ xref: 'paper', x: 0.01, y: SMN, text: 'Salario mínimo', showarrow: false, yshift: 10, font: { color: C.amber, size: 11 } }] });
    if ($('#ch-mz3d').data) mz3d(); };
  f.querySelectorAll('input,select').forEach(i => i.oninput = upd); lazy('ch-mz', upd); lazy('ch-mz3d', mz3d);
})();
lazy('ch-mzcoef', () => { const R = MZE.filter(r => !['(Intercept)', 'exper2', 'lhoras'].includes(r.termino)).sort((a, b) => a.efecto_pct - b.efecto_pct);
  const lab = t => t.replace('departamento', 'Vivir en ').replace('sexoMujer', 'Ser mujer').replace('zonaRural', 'Área rural').replace('aestudio', '+1 año de estudio').replace('exper', '+1 año de experiencia');
  P('ch-mzcoef', [{ type: 'bar', orientation: 'h', y: R.map(r => lab(r.termino)), x: R.map(r => r.efecto_pct), marker: { color: R.map(r => r.efecto_pct >= 0 ? C.neon : C.pink) },
    customdata: R.map(r => [r.li, r.ls]), text: R.map(r => (r.efecto_pct >= 0 ? '+' : '') + nf(r.efecto_pct, 1) + '%'), textposition: 'outside', cliponaxis: false, textfont: { color: C.txt, size: 10 }, hovertemplate: '%{y}: %{x:+.1f}% (IC 95%: %{customdata[0]:.1f}% a %{customdata[1]:.1f}%)<extra></extra>' }],
    { margin: { l: 170, r: 60, t: 10, b: 50 }, xaxis: { title: { text: 'Efecto sobre el ingreso (%)' }, ticksuffix: '%' }, yaxis: { tickfont: { color: C.txt, size: 11 } },
      annotations: [{ xref: 'paper', yref: 'paper', x: 1, y: 0, xanchor: 'right', text: `Comparado con: hombre, urbano, Chuquisaca · R² aj ${nf((MZE[0] || {}).r2_aj, 3)}`, showarrow: false, font: { color: C.mut, size: 10 } }] }); });
lazy('ch-mzgrp', () => { const R = T('F10_mincer_por_grupo'), c = MZC.find(r => r.termino === 'aestudio') || {};
  grow('ch-mzgrp', [{ type: 'bar', x: ['Todos'].concat(R.map(r => r.grupo)), y: [c.efecto_pct].concat(R.map(r => r.retorno_educ)), marker: { color: [C.neon, C.cyan, C.pink, C.lila, C.amber] }, text: [c.efecto_pct].concat(R.map(r => r.retorno_educ)).map(v => '+' + nf(v, 1) + '%'), textposition: 'outside', textfont: { color: C.txt, family: FHEAD, size: 13 }, hovertemplate: '%{x}: %{y:.2f}% por año<extra></extra>' }],
    { yaxis: { title: { text: '% más de ingreso por cada año de estudio' }, ticksuffix: '%' }, xaxis: { tickfont: { color: C.txt } }, margin: { l: 60, r: 20, t: 20, b: 40 } }, 'y'); });
$('#code-mz').outerHTML = `<div id="code-mz">${codeBox([['F10_modelado', [['rg <- df |> mutate(.fila = row_number())', 0, 2], ['m_clasica <- lm(ly ~ aestudio', 0, 1]]]], 'exper = edad − años de estudio − 6 (experiencia potencial). El efecto en % es 100 × (e<sup>b</sup> − 1).')}</div>`;

/* ================= 12 MODELADO ================= */
const ROC = T('F10_test_roc'), CVP = T('F10_cv_por_pliegue'), PRP = T('F10_cv_pruebas_pareadas'), IMP = T('F10_importancia_permutacion'), CALB = T('F10_test_calibracion'), SIMM = T('F10_simulador_metricas');
const MODC = { 'Naive Bayes': C.mut, 'Regresion logistica': C.cyan, 'Logistica L1 (lasso)': C.blue, 'Arbol de decision': C.amber, 'Random Forest': C.neon, 'Gradient Boosting (GBM)': C.pink };
const MODD = { 'Naive Bayes': 'cuenta frecuencias por separado; muy simple', 'Regresion logistica': 'suma ponderada → probabilidad; se puede leer', 'Logistica L1 (lasso)': 'logística que apaga variables poco útiles', 'Arbol de decision': 'preguntas sí/no en cadena', 'Random Forest': 'cientos de árboles que votan', 'Gradient Boosting (GBM)': 'árboles en fila que corrigen errores' };
const VARL = { cat_ocupacional: 'Categoría ocupacional', tothrs: 'Horas trabajadas', rama: 'Rama de actividad', grupo_ocup: 'Grupo ocupacional', antiguedad: 'Antigüedad', carga_hogar: 'Miembros por ocupado del hogar', departamento: 'Departamento', aestudio: 'Años de estudio', sexo: 'Sexo', zona: 'Área', edad: 'Edad', edad2: 'Edad²', jefe: 'Jefe de hogar', sector: 'Sector', tamano_empresa: 'Tamaño del establecimiento', informal: 'Aporta a la Gestora', seguro_salud: 'Seguro de salud', menores_14: 'Menores de 14 en el hogar', mayores_60: 'Mayores de 60 en el hogar' };
$('#code-cmp').outerHTML = `<div id="code-cmp">${codeBox([['00_modelos', [['comparar_modelos <- function(datos, y, x', 0, 0], ['fold <- pliegues(estrato, k, semilla)', 0, 0], ['for (mo in modelos) {', 0, 7], ['umbrales <- vapply(modelos', 0, 0]]], ['F10_modelado', [['idx_tr <- unlist(', 0, 1], ['cv_res[[o]] <- comparar_modelos(tr, OBJ[[o]], X_ML)', 0, 0]]]],
  'Una sola función entrena los 6 modelos con los mismos 5 pliegues: así la comparación es justa. El 30% de test no se toca hasta el final.')}</div>`;
const nTest = r => r ? r.vp + r.vn + r.fp + r.fn : null;
let OBJ = 'A';
function drawModel() {
  const o = OBJ, ec = o === 'A' ? ENF.persona : ENF.hogar, R = CVR.filter(r => r.objetivo === o).sort((a, b) => b.auc_media - a.auc_media), mods = R.map(r => r.modelo);
  const bst = R[0] || {}, eleg = R.find(r => /legido/.test(r.rol)) || bst, tm = TM.filter(r => r.objetivo === o), lg = R.find(r => r.modelo === 'Regresion logistica') || {}, pr = PRP.filter(r => r.objetivo === o), sm = by(SIMM.filter(r => r.objetivo === o), 'objetivo', o);
  setNeon('cmp-neon', [[`${mods.length} modelos`, C.lila], [`${new Set(CVP.map(r => r.pliegue)).size} rondas de prueba`, C.lila], [`${Object.keys(VARL).length} características por persona`, C.cyan], [`examen final: ${nf(nTest(tm[0]))} personas`, C.neon], [`objetivo ${ec.l}`, ec.c]]);
  const F = CVP.filter(r => r.objetivo === o);
  P('ch-cv', mods.map(m => ({ type: 'scatter', mode: 'markers', name: ACC(m), y: F.filter(r => r.modelo === m).map(() => ACC(m)), x: F.filter(r => r.modelo === m).map(r => r.auc), marker: { size: 9, color: MODC[m], opacity: .7 }, hovertemplate: ACC(m) + ' ronda: %{x:.3f}<extra></extra>', showlegend: false }))
    .concat([{ type: 'scatter', mode: 'markers', y: R.map(r => ACC(r.modelo)), x: R.map(r => r.auc_media), error_x: { type: 'data', array: R.map(r => r.auc_ee), color: C.txt, thickness: 2, width: 8 }, marker: { symbol: 'diamond', size: 15, color: R.map(r => MODC[r.modelo]), line: { color: '#fff', width: 1.5 } }, showlegend: false, hovertemplate: '%{y}: AUC media %{x:.4f}<extra></extra>' }]),
    { margin: { l: 170, r: 20, t: 10, b: 50 }, xaxis: { title: { text: 'AUC (validación cruzada)' }, tickformat: '.3f' }, yaxis: { autorange: 'reversed', tickfont: { color: C.txt } },
      shapes: [{ type: 'rect', x0: bst.auc_media - bst.auc_ee, x1: bst.auc_media + bst.auc_ee, yref: 'paper', y0: 0, y1: 1, fillcolor: 'rgba(57,255,159,.08)', line: { width: 0 } }] });
  const within = R.filter(r => r.auc_media >= bst.auc_media - bst.auc_ee).map(r => ACC(r.modelo));
  $('#why').innerHTML = `<div class="neons">${neon(`Mayor AUC: <b>${ACC(bst.modelo)}</b> ${nf(bst.auc_media, 3)}`, C.neon)}${neon(`Empatados con el mejor (regla de 1 error estándar): ${within.join(', ')}`, C.lila)}${neon(`Elegido: <b>${ACC(eleg.modelo)}</b>`, C.pink)}
    ${neon(`Logística: ${nf(lg.auc_media, 3)} (${nf(bst.auc_media - lg.auc_media, 3)} menos)`, C.cyan)}${sm.auc != null ? neon(`Simulador (logística): AUC examen <b>${nf(sm.auc, 3)}</b>`, C.amber) : ''}</div>
    <div class="grid g3" style="margin-top:6px">
      <div class="qq"><b style="color:var(--neon)">1. Misma cancha</b><p>Los seis modelos se entrenan y evalúan con las mismas 5 rondas. Así, si uno gana, no es por suerte de la partición.</p></div>
      <div class="qq"><b style="color:var(--lila)">2. ¿La diferencia es real?</b><p>Una prueba estadística (t pareada) compara cada modelo con el mejor ronda por ronda. Si no hay diferencia real, se prefiere el más simple.</p></div>
      <div class="qq"><b style="color:var(--pink)">3. Para qué se usa</b><p>${ACC(eleg.modelo)} da las mejores predicciones. Para el simulador se usa la logística: pierde poco y sus coeficientes se pueden leer y calcular en el navegador.</p></div></div>
    <div class="neons">${pr.map(p => neon(`${ACC(p.modelo)}: ${nf(p.dif_auc, 3)} · ${p.p_valor < .05 ? 'peor de verdad' : 'empate estadístico'}`, p.p_valor < .05 ? C.pink : C.neon)).join('')}</div>`;
  table('tb-cv', [['modelo', 'Modelo', (v, r) => `<b style="color:${MODC[v]}">${ACC(v)}</b>${r.rol ? `<span class="win">${r.rol.toUpperCase()}</span>` : ''}<div class="src" style="margin:2px 0 0">${MODD[v] || ''}</div>`], ['auc_media', gl('AUC', 'auc'), v => nf(v, 3)], ['auc_de', '± variación', v => nf(v, 3)],
    ['sensibilidad_youden', 'Detecta vulnerables', v => pc(v, 0)], ['especificidad_youden', 'Acierta no vulnerables', v => pc(v, 0)], ['umbral_youden', gl('Umbral', 'umbral'), v => nf(v, 2)], ['complejidad', 'Complejidad', v => ['', 'baja', 'media', 'alta'][v]], ['segundos', 'Tiempo', v => nf(v, 1) + ' s']], R, r => r.modelo === bst.modelo);
  P('ch-roc', mods.map(m => { const Rr = ROC.filter(r => r.objetivo === o && r.modelo === m), t = by(tm, 'modelo', m);
    return { x: Rr.map(r => r.fpr), y: Rr.map(r => r.tpr), name: `${ACC(m)} (${nf(t.auc, 3)})`, mode: 'lines', line: { color: MODC[m], width: m === bst.modelo ? 4 : 2 }, hovertemplate: 'Falsas alarmas %{x:.0%} · detecta %{y:.0%}<extra>' + ACC(m) + '</extra>' }; })
    .concat([{ x: [0, 1], y: [0, 1], mode: 'lines', line: { color: C.dim, dash: 'dash' }, showlegend: false, hoverinfo: 'skip' }]),
    { xaxis: { title: { text: 'Falsas alarmas (no vulnerables marcados)' }, range: [0, 1], tickformat: '.0%' }, yaxis: { title: { text: 'Vulnerables detectados' }, range: [0, 1], tickformat: '.0%', scaleanchor: 'x' }, margin: { l: 60, r: 10, t: 10, b: 50 }, legend: { orientation: 'v', x: .42, y: .05, bgcolor: 'rgba(3,7,20,.7)' } });
  const sel = $('#sel-mod'); sel.innerHTML = mods.map(m => `<option value="${m}">${ACC(m)}</option>`).join(''); sel.value = bst.modelo; sel.onchange = () => { $('#thr').value = by(tm, 'modelo', sel.value).umbral; cm(); }; $('#thr').value = by(tm, 'modelo', bst.modelo).umbral; $('#thr').oninput = cm; cm();
  const I = IMP.filter(r => r.objetivo === o).sort((a, b) => a.caida_auc - b.caida_auc).slice(-12);
  P('ch-imp', [{ type: 'bar', orientation: 'h', y: I.map(r => VARL[r.variable] || r.variable), x: I.map(r => r.caida_auc), error_x: { type: 'data', array: I.map(r => r.de), color: C.mut }, marker: { color: I.map(r => r.caida_auc), colorscale: [[0, '#0b1a4a'], [.5, C.lila], [1, ec.c]] }, hovertemplate: '%{y}: el AUC cae %{x:.4f}<extra></extra>' }],
    { margin: { l: 190, r: 20, t: 30, b: 40 }, title: { text: ACC((I[0] || {}).modelo || ''), font: { size: 12, color: C.mut } }, xaxis: { title: { text: 'Cuánto empeora sin esa variable' } }, yaxis: { tickfont: { color: C.txt, size: 11 } } });
  P('ch-cal', [bst.modelo, 'Regresion logistica', 'Naive Bayes'].filter((m, i, a) => a.indexOf(m) === i).map(m => { const Cc = CALB.filter(r => r.objetivo === o && r.modelo === m); return { x: Cc.map(r => r.p_media), y: Cc.map(r => r.y_media), name: ACC(m), mode: 'lines+markers', line: { color: MODC[m], width: 2.5 }, marker: { size: 7 } }; })
    .concat([{ x: [0, 1], y: [0, 1], mode: 'lines', line: { color: C.dim, dash: 'dash' }, showlegend: false, hoverinfo: 'skip' }]),
    { xaxis: { title: { text: 'Lo que dice el modelo' }, range: [0, 1], tickformat: '.0%' }, yaxis: { title: { text: 'Lo que pasó en realidad' }, range: [0, 1], tickformat: '.0%' }, margin: { l: 60, r: 10, t: 10, b: 90 }, legend: { orientation: 'h', y: -0.32 } });
  table('tb-test', [['modelo', 'Modelo', v => `<b style="color:${MODC[v]}">${ACC(v)}</b>`], ['auc', 'AUC', v => nf(v, 3)], ['exactitud_balanceada', 'Aciertos (bal.)', v => pc(v, 1)], ['sensibilidad', 'Detecta', v => pc(v, 0)], ['especificidad', 'Acierta no vul.', v => pc(v, 0)], ['precision', 'Precisión', v => pc(v, 0)]],
    tm.slice().sort((a, b) => b.auc - a.auc), r => r.modelo === bst.modelo);
}
function cm() { const m = $('#sel-mod').value, t = +$('#thr').value, tm = by(TM.filter(r => r.objetivo === OBJ), 'modelo', m), Rr = ROC.filter(r => r.objetivo === OBJ && r.modelo === m && isFinite(r.umbral));
  const n1 = tm.vp + tm.fn, n0 = tm.vn + tm.fp; let b = Rr[0] || {}; Rr.forEach(r => { if (Math.abs(r.umbral - t) < Math.abs(b.umbral - t)) b = r; });
  const vp = Math.round(b.tpr * n1), fn = n1 - vp, fp = Math.round(b.fpr * n0), vn = n0 - fp, sens = vp / n1, esp = vn / n0, prec = vp / Math.max(1, vp + fp);
  $('#thr-v').textContent = nf(t, 3);
  $('#cm').innerHTML = `<div class="x"></div><div class="x">El modelo dice: vulnerable</div><div class="x">El modelo dice: no</div><div class="x">Es vulnerable</div><div class="vp"><b>${nf(vp)}</b>acierta</div><div class="fn"><b>${nf(fn)}</b>se le escapan</div>
    <div class="x">No es vulnerable</div><div class="fp"><b>${nf(fp)}</b>falsa alarma</div><div class="vn"><b>${nf(vn)}</b>acierta</div>`;
  setNeon('cm-neon', [[`Detecta <b>${pc(sens, 0)}</b> de los vulnerables`, C.neon], [`Acierta <b>${pc(esp, 0)}</b> de los no vulnerables`, C.cyan], [`Cuando dice “vulnerable”, acierta <b>${pc(prec, 0)}</b>`, C.amber]]); }
lazy('ch-cv', () => { segBtns('seg-obj', [['A', 'Objetivo A · Persona'], ['B', 'Objetivo B · Hogar']], o => { OBJ = o; drawModel(); }, ENFQ === 'hogar' ? 1 : 0); onEnf(() => { if (ENFQ === 'persona' || ENFQ === 'hogar') segSet('seg-obj', ENFQ === 'persona' ? 'A' : 'B'); }); });
lazy('ch-bal', () => { const R = T('F10_balanceo_clases');
  P('ch-bal', [['sensibilidad', 'Detecta vulnerables', C.pink], ['especificidad', 'Acierta no vulnerables', C.cyan], ['auc', 'AUC', C.amber]].map(m => ({ type: 'scatter', mode: 'markers', name: m[1], y: R.map(r => ACC(r.conjunto)), x: R.map(r => r[m[0]]), marker: { size: 14, color: m[2], line: { color: '#030714', width: 1 } }, hovertemplate: '%{y} · ' + m[1] + ': %{x:.3f}<extra></extra>' })),
    { margin: { l: 170, r: 20, t: 10, b: 60 }, xaxis: { range: [0.5, 1], tickformat: '.2f' }, yaxis: { tickfont: { color: C.txt } } });
  const o = by(R, 'conjunto', 'Original'), s = by(R, 'conjunto', 'Submuestreo aleatorio');
  setNeon('bal-neon', [[`Solo <b>${pc(te('hogar').vulnerables_muestra / te('hogar').n_muestra, 0)}</b> son vulnerables en B`, C.cyan], [`Equilibrar sube la detección <b>${pc(o.sensibilidad, 0)} → ${pc(s.sensibilidad, 0)}</b>`, C.pink], [`AUC casi igual <b>${nf(o.auc, 3)} → ${nf(s.auc, 3)}</b>`, C.amber]]); });
lazy('ch-reg', () => { const R = T('F10_regresion_ingreso_cv'), M2 = T('F10_regresion_ingreso_metricas'), ms = M2.map(r => r.modelo), col = [C.neon, C.lila, C.cyan];
  P('ch-reg', ms.map((m, i) => ({ type: 'box', name: ACC(m), y: R.filter(r => r.modelo === m).map(r => r.r2), boxpoints: 'all', jitter: .4, pointpos: 0, marker: { color: col[i % 3], size: 8 }, line: { color: col[i % 3] }, fillcolor: 'rgba(0,0,0,0)', hovertemplate: 'R² ronda %{y:.3f}<extra>' + ACC(m) + '</extra>' })),
    { yaxis: { title: { text: 'R²: % de la variación del ingreso explicada' }, tickformat: '.0%' }, xaxis: { tickfont: { color: C.txt } }, showlegend: false, margin: { l: 60, r: 20, t: 10, b: 60 } });
  setNeon('reg-neon', M2.map((r, i) => [`${ACC(r.modelo)}: explica <b>${pc(r.r2, 0)}</b> · error típico <b>Bs ${nf(r.mae_mediano_bs)}</b>`, col[i % 3]])); });

/* ================= 13 SIMULADOR ================= */
const SIM = E.sim, CMP = T('F11_comparables_perfil'), CMPD = T('F11_comparables_perfil_dep');
(function () {
  if (!SIM || !SIM.modelos) { $('#sim-lead').textContent = 'Falta salidas/05_modelos/simulador_logit.json'; return; }
  const A = SIM.modelos.A, B = SIM.modelos.B, NV = SIM.niveles;
  $('#sim-lead').innerHTML = `Elige un perfil o mueve los controles. La probabilidad la calcula una ${gl('regresión logística', 'logit')} entrenada en R (AUC en el examen final: A ${nf(A.auc_test, 3)} · B ${nf(B.auc_test, 3)}). Debajo del formulario aparece qué pasó con las personas reales parecidas en la encuesta.`;
  const LBL = { sexo: 'Sexo', edad: 'Edad', aestudio: 'Años de estudio', zona: 'Área', departamento: 'Departamento', cat_ocupacional: 'Categoría ocupacional', grupo_ocup: 'Grupo ocupacional', tothrs: 'Horas por semana', informal: 'Aporte a la Gestora', carga_hogar: 'Miembros del hogar por cada ocupado' };
  const vars = SIM.variables.filter(v => v !== 'edad2'), S = {};
  const rng = { edad: [19, 80, 1], aestudio: [0, 22, 1], tothrs: [1, 98, 1], carga_hogar: [1, 10, 0.5] };
  $('#sim-form').innerHTML = vars.map(v => Array.isArray(NV[v]) && typeof NV[v][0] === 'string'
    ? `<div><label>${LBL[v] || v}</label><select data-v="${v}">${NV[v].map(l => `<option value="${l}">${ACC(l)}</option>`).join('')}</select></div>`
    : `<div><label>${LBL[v] || v} <span class="rv" data-r="${v}"></span></label><input type="range" data-v="${v}" min="${rng[v][0]}" max="${rng[v][1]}" step="${rng[v][2]}"></div>`).join('');
  const prob = (M, s) => { let eta = M.intercepto; for (const k in M.numericas) eta += M.numericas[k] * (k === 'edad2' ? s.edad * s.edad : s[k]); for (const k in M.factores) eta += (M.factores[k][s[k]] || 0); return 1 / (1 + Math.exp(-eta)); };
  const PRE = [['Comerciante por cuenta propia, El Alto', { sexo: 'Mujer', edad: 42, aestudio: 9, zona: 'Urbana', departamento: 'La Paz', cat_ocupacional: 'Cuenta propia', grupo_ocup: 'Servicios y vendedores', tothrs: 50, informal: 'No aporta', carga_hogar: 3 }],
    ['Profesional asalariado, Santa Cruz', { sexo: 'Hombre', edad: 35, aestudio: 17, zona: 'Urbana', departamento: 'Santa Cruz', cat_ocupacional: 'Obrero o empleado', grupo_ocup: 'Profesionales', tothrs: 45, informal: 'Aporta', carga_hogar: 2 }],
    ['Agricultora, Potosí rural', { sexo: 'Mujer', edad: 50, aestudio: 3, zona: 'Rural', departamento: 'Potosi', cat_ocupacional: 'Cuenta propia', grupo_ocup: 'Agricultores y pescadores', tothrs: 35, informal: 'No aporta', carga_hogar: 4 }],
    ['Albañil jefe de hogar, Cochabamba', { sexo: 'Hombre', edad: 38, aestudio: 8, zona: 'Urbana', departamento: 'Cochabamba', cat_ocupacional: 'Obrero o empleado', grupo_ocup: 'Oficios de construccion e industria', tothrs: 48, informal: 'No aporta', carga_hogar: 4 }],
    ['Joven que ayuda en el negocio familiar, Beni', { sexo: 'Hombre', edad: 20, aestudio: 11, zona: 'Rural', departamento: 'Beni', cat_ocupacional: 'Familiar sin remuneracion', grupo_ocup: 'Agricultores y pescadores', tothrs: 30, informal: 'No aporta', carga_hogar: 5 }]];
  $('#presets').innerHTML = PRE.map((p, i) => `<button class="btn" data-i="${i}">${p[0]}</button>`).join('');
  const setS = o => { vars.forEach(v => { const n = document.querySelector(`#sim-form [data-v="${v}"]`); if (o[v] != null && n) { if (n.tagName === 'SELECT' && ![...n.options].some(x => x.value === o[v])) return; n.value = o[v]; } }); upd(); };
  $$('#presets button').forEach(b => b.onclick = () => { $$('#presets button').forEach(x => x.classList.remove('pri')); b.classList.add('pri'); setS(PRE[+b.dataset.i][1]); });
  const gauge = (id, p, M, c) => P(id, [{ type: 'indicator', mode: 'gauge+number', value: 100 * p, number: { suffix: '%', valueformat: p < .1 ? '.1f' : '.0f', font: { color: c, family: FHEAD, size: 40 } },
    gauge: { axis: { range: [0, 100], tickcolor: C.mut, tickfont: { color: C.mut } }, bar: { color: c, thickness: .28 }, bgcolor: 'rgba(0,0,0,0)', borderwidth: 0, steps: [{ range: [0, 100 * M.umbral], color: 'rgba(57,255,159,.10)' }, { range: [100 * M.umbral, 100], color: 'rgba(255,79,163,.10)' }],
      threshold: { line: { color: '#fff', width: 3 }, thickness: .8, value: 100 * M.umbral } } }], { margin: { l: 34, r: 34, t: 24, b: 0 }, transition: { duration: 500 } });
  const sim3d = () => { const e = enfAB(), M = e.k === 'hogar' ? B : A, ed = Array.from({ length: 31 }, (_, i) => 19 + 2 * i), ae = Array.from({ length: 23 }, (_, i) => i);
    P('ch-sim3d', [{ type: 'surface', x: ae, y: ed, z: ed.map(x => ae.map(a => prob(M, { ...S, edad: x, aestudio: a }))), colorscale: [[0, C.neon], [M.umbral, C.lila], [1, e.c]], cmin: 0, cmax: 1, showscale: false, opacity: .95,
      contours: { z: { show: true, usecolormap: true, project: { z: true } } }, hovertemplate: '%{x} años de estudio · %{y} años<br>Probabilidad %{z:.1%}<extra></extra>' },
      { type: 'scatter3d', mode: 'markers', x: [S.aestudio], y: [S.edad], z: [prob(M, S)], marker: { size: 7, color: '#fff', line: { color: e.c, width: 3 } }, hovertemplate: 'Perfil: %{z:.1%}<extra></extra>' }],
      { scene: { ...SC3, xaxis: { ...SC3.xaxis, title: { text: 'Años de estudio' } }, yaxis: { ...SC3.yaxis, title: { text: 'Edad' } }, zaxis: { ...SC3.zaxis, title: { text: 'Prob. ' + e.s }, range: [0, 1], tickformat: '.0%' }, camera: { eye: { x: 1.5, y: -1.6, z: .9 } } }, margin: { l: 0, r: 0, t: 0, b: 0 }, showlegend: false }); };
  function comparables(pa, pb) { const ap = S.informal === 'Aporta';
    const m1 = CMP.find(r => r.grupo_ocup === S.grupo_ocup && r.cat_ocupacional === S.cat_ocupacional && r.zona === S.zona && r.sexo === S.sexo && r.aporta_gestora === ap);
    const m2 = CMPD.find(r => r.departamento === S.departamento && r.grupo_ocup === S.grupo_ocup && r.cat_ocupacional === S.cat_ocupacional && r.aporta_gestora === ap);
    const line = (r, d) => r ? `<div>• ${d}: <b>${nf(r.n)}</b> personas en la muestra → <b style="color:var(--pink)">${pc(r.tasa_persona)}</b> gana menos del salario mínimo y <b style="color:var(--cyan)">${pc(r.tasa_hogar)}</b> vive en un hogar pobre (ingreso mediano Bs ${nf(r.ylab_mediana)}).</div>` : '';
    const t1 = `${ACC(S.grupo_ocup).toLowerCase()}, ${ACC(S.cat_ocupacional).toLowerCase()}, ${S.zona === 'Urbana' ? 'urbano' : 'rural'}, ${S.sexo === 'Mujer' ? 'mujeres' : 'hombres'}, ${ap ? 'que aportan' : 'que no aportan'}`;
    const t2 = `${ACC(S.grupo_ocup).toLowerCase()}, ${ACC(S.cat_ocupacional).toLowerCase()} en ${dn(S.departamento)}, ${ap ? 'que aportan' : 'que no aportan'}`;
    $('#sim-comp').innerHTML = `<span class="t">Personas reales parecidas en la encuesta</span>` + (m1 || m2 ? line(m1, t1) + line(m2, t2) : 'No hay al menos 15 personas con esta combinación exacta: el modelo extrapola desde perfiles cercanos.');
    const lowA = pa < .05, lowB = pb < .05;
    $('#sim-why').innerHTML = lowA || lowB ? `<b>¿Por qué una probabilidad tan baja?</b> Este perfil junta casi todas las condiciones que en los datos protegen del ingreso bajo: ${[S.aestudio >= 15 && 'muchos años de estudio', S.cat_ocupacional === 'Obrero o empleado' && 'trabajo asalariado', ap && 'aporte a la Gestora (empleo formal)', ['Profesionales', 'Directivos', 'Tecnicos de nivel medio'].includes(S.grupo_ocup) && 'una ocupación calificada', S.carga_hogar <= 2 && 'pocos dependientes por ocupado', S.tothrs >= 40 && 'jornada completa'].filter(Boolean).join(', ')}. La logística multiplica esas ventajas. Los datos lo confirman: ${m2 ? `entre los ${nf(m2.n)} casos parecidos en ${dn(S.departamento)}, solo ${pc(m2.tasa_persona)} es vulnerable por A y ${pc(m2.tasa_hogar)} por B` : m1 ? `entre ${nf(m1.n)} casos parecidos, solo ${pc(m1.tasa_persona)} es vulnerable por A y ${pc(m1.tasa_hogar)} por B` : 'casi nadie con ese perfil es vulnerable'}.`
      : pa > .7 || pb > .7 ? `<b>¿Por qué tan alta?</b> El perfil reúne las condiciones más asociadas al ingreso bajo: ${[S.cat_ocupacional !== 'Obrero o empleado' && 'trabajo independiente o sin pago', !ap && 'sin aporte a la Gestora', S.aestudio <= 8 && 'pocos años de estudio', S.zona === 'Rural' && 'área rural', S.carga_hogar >= 3 && 'muchos dependientes por ocupado', S.tothrs < 40 && 'jornada corta'].filter(Boolean).join(', ')}.${m1 ? ` En la encuesta, ${pc(m1.tasa_persona)} de las personas parecidas gana menos del salario mínimo.` : ''}`
      : `<b>Cómo leerlo:</b> la aguja blanca es el ${gl('umbral', 'umbral')}: a la derecha, el modelo lo clasifica como vulnerable. La barra de abajo muestra cuánto cambiaría la probabilidad si cambiara una sola característica.`; }
  function upd() {
    vars.forEach(v => { const n = document.querySelector(`#sim-form [data-v="${v}"]`); S[v] = n.tagName === 'SELECT' ? n.value : +n.value; const r = document.querySelector(`#sim-form [data-r="${v}"]`); if (r) r.textContent = S[v]; });
    const pa = prob(A, S), pb = prob(B, S);
    gauge('g-A', pa, A, C.pink); gauge('g-B', pb, B, C.cyan);
    $('#g-A-t').innerHTML = `${pa >= A.umbral ? '<b style="color:var(--pink)">Clasificado vulnerable</b>' : '<b style="color:var(--neon)">No vulnerable</b>'} · promedio nacional ${pc(A.prevalencia)}`;
    $('#g-B-t').innerHTML = `${pb >= B.umbral ? '<b style="color:var(--cyan)">Clasificado vulnerable</b>' : '<b style="color:var(--neon)">No vulnerable</b>'} · promedio nacional ${pc(B.prevalencia)}`;
    const W = [['+4 años de estudio', { aestudio: Math.min(22, S.aestudio + 4) }], ['Aporta a la Gestora', { informal: 'Aporta' }], ['+10 horas', { tothrs: Math.min(98, S.tothrs + 10) }], ['Área contraria', { zona: S.zona === 'Urbana' ? 'Rural' : 'Urbana' }],
      ['Sexo contrario', { sexo: S.sexo === 'Mujer' ? 'Hombre' : 'Mujer' }], ['Pasa a asalariado', { cat_ocupacional: 'Obrero o empleado' }], ['+1 dependiente por ocupado', { carga_hogar: Math.min(10, S.carga_hogar + 1) }]];
    P('sim-contrib', ['A', 'B'].map(o => { const M = o === 'A' ? A : B, p0 = o === 'A' ? pa : pb; return { type: 'bar', orientation: 'h', name: o === 'A' ? 'A · Persona' : 'B · Hogar', y: W.map(w => w[0]), x: W.map(w => prob(M, { ...S, ...w[1] }) - p0), marker: { color: o === 'A' ? C.pink : C.cyan, opacity: enfs().some(e => e.s === o) ? 1 : .35 }, hovertemplate: '%{y}: %{x:+.1%}<extra>' + o + '</extra>' }; }),
      { barmode: 'group', margin: { l: 170, r: 20, t: 30, b: 50 }, xaxis: { tickformat: '+.0%', title: { text: 'Cambio en la probabilidad si…' }, zeroline: true, zerolinecolor: C.mut }, yaxis: { autorange: 'reversed', tickfont: { color: C.txt } }, title: { text: '¿Qué pasaría si cambia una sola cosa?', font: { size: 12, color: C.mut } } });
    comparables(pa, pb); if ($('#ch-sim3d').data) sim3d();
  }
  $$('#sim-form [data-v]').forEach(n => n.oninput = upd);
  lazy('g-A', () => document.querySelector('#presets button').click());
  lazy('ch-sim3d', () => { if (S.edad == null) upd(); sim3d(); });
  onEnf(() => { if ($('#ch-sim3d').data) sim3d(); if ($('#sim-contrib').data) upd(); });
  $('#code-sim').outerHTML = `<div id="code-sim">${codeBox([['F10_modelado', [['m <- glm(reformulate(X_SIM, "y"), data = tr, family = binomial())', 0, 1], ['SIM$modelos[[o]] <- list(', 0, 4], ['stopifnot(max(abs(plogis(eta)', 0, 0]]]],
    'R exporta los coeficientes a un JSON; el navegador repite la misma cuenta: probabilidad = 1 / (1 + e<sup>−(b₀ + b₁x₁ + …)</sup>). El código verifica que la diferencia con R sea menor a 0.00000001.')}</div>`;
})();

/* ================= 14 BITÁCORA + GALERÍA ================= */
const bf = $('#bf'); bf.innerHTML = '<option value="">Todas las fases</option>' + FASES.map(f => `<option value="${f[0]}">${f[0]} · ${f[1]}</option>`).join('');
let bRows = bit;
function renderBit() {
  const q = $('#bq').value.toLowerCase().trim(), fz = bf.value;
  bRows = bit.filter(r => (!fz || (r.fase || '').startsWith(fz)) && (!q || Object.values(r).join(' ').toLowerCase().includes(q)));
  table('tb-bit', [['id', '#'], ['fase', 'Fase', v => { const k = (v || '').slice(0, 3); return `<span class="tag" style="--c:${(FN[k] || [])[3] || C.mut}">${k}</span>`; }], ['atributo', 'Variables', v => `<span class="mono" style="font-size:11.5px">${String(v || '').slice(0, 90)}</span>`], ['tipo_problema', 'Problema', v => ACC(v)], ['metodologia', 'Método', v => ACC(v)], ['accion', 'Decisión', v => ACC(v)], ['criterio', 'Por qué', v => ACC(v)], ['filas_afectadas', 'Filas', v => nf(v)], ['impacto', 'Resultado', v => ACC(v)], ['codigo', 'Código', v => v ? `<span class="mono" style="color:var(--lila);font-size:11.5px">${esc(v)}</span>` : '—']], bRows);
  $('#bcount').textContent = `${bRows.length} de ${bit.length} decisiones`;
}
$('#bq').oninput = renderBit; bf.onchange = renderBit; renderBit();
$('#bcsv').onclick = () => { const ks = Object.keys(bit[0] || {}), q = v => v == null ? '' : '"' + String(v).replace(/"/g, '""') + '"';
  const csv = '﻿' + ks.join(',') + '\n' + bRows.map(r => ks.map(k => q(r[k])).join(',')).join('\n');
  const a = document.createElement('a'); a.href = URL.createObjectURL(new Blob([csv], { type: 'text/csv' })); a.download = 'bitacora_filtrada.csv'; a.click(); };
function renderGal(fz) { const L2 = G.filter(g => !fz || g.fase === fz); const gal = $('#gal'); gal.innerHTML = ''; L2.forEach(g => gal.appendChild(thumb(g, L2))); $('#gcount').textContent = `${L2.length} gráficos generados por R`; }
segBtns('seg-gal', [['', 'Todas'], ...FASES.filter(f => G.some(g => g.fase === f[0])).map(f => [f[0], f[0]])], renderGal);

/* ================= 15 REPRODUCIR ================= */
setNeon('gen', [[`<b>${Object.keys(E.tablas || {}).length}</b> tablas`, C.cyan], [`<b>${bit.length}</b> decisiones en la bitácora`, C.lila], [`<b>${G.length}</b> gráficos`, C.pink], [`<b>${Object.keys(SCR).length}</b> scripts de R`, C.neon], (E.log || {}).tiempo_total ? [`<b>${nf(E.log.tiempo_total, 1)} min</b> de ejecución completa`, C.amber] : null]);
$('#team2').innerHTML = EQ.map(n => `<div class="m"><div class="av">${n.replace(/[\[\]]/g, '').split(' ').filter(Boolean).slice(0, 2).map(w => w[0]).join('').toUpperCase()}</div><div><b>${n}</b><span>Carrera de Informática · UMSA</span></div></div>`).join('');

/* ================= GENERAL ================= */
const ro = new IntersectionObserver(es => es.forEach(e => { if (e.isIntersecting) { e.target.classList.add('in'); ro.unobserve(e.target); } }), { threshold: .06 });
$$('.reveal').forEach(n => ro.observe(n));
const links = [...document.querySelectorAll('.navlinks a')], secs = links.map(a => document.querySelector(a.getAttribute('href')));
addEventListener('scroll', () => { const h = document.documentElement; $('#prog').style.width = (h.scrollTop / (h.scrollHeight - h.clientHeight) * 100) + '%';
  let cur = 0; secs.forEach((s, i) => { if (s && s.getBoundingClientRect().top < 140) cur = i; }); links.forEach((a, i) => a.classList.toggle('on', i === cur));
  let ph = null; FASES.forEach(f => { const n = document.getElementById('ph-' + f[0]); if (n && n.getBoundingClientRect().top < 200) ph = f[0]; }); $$('#phnav button').forEach(b => b.classList.toggle('on', b.dataset.k === ph)); }, { passive: true });
})();
