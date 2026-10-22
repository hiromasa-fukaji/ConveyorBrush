// =============================================================
// SVGのパス(線)を読み込んで、ピースを並べるトラックの点列に変換する
//
//   parseSvgTracks(svgText, curveSteps)
//     → { viewBox, subpaths: [[{x, y}, ...], ...] }  (SVGの座標系のまま)
//
// 対応する要素: path / line / polyline / polygon / rect / circle / ellipse
//   - 曲線(C/S/Q/T/A)は線分に分割して点列にする
//   - 1つの<path>に複数のサブパス(M)があれば、別々のトラックにする
//   - 要素と祖先の transform(matrix/translate/scale/rotate/skewX/skewY)を反映する
//   - <defs>・<clipPath>・<mask>・<symbol> などの中の図形は描画されないので無視する
// 非対応: <use>、<text>、CSSの class / <style> による transform
//
// readSvgViewBox() は sketch.js にある。
// =============================================================

// ------------------------------------------------------------
// SVGパスの d 属性をパースし、曲線を線分に分割(フラット化)した
// 座標点列の配列(サブパスごと)を返す。
// 対応コマンド: M/m L/l H/h V/v C/c S/s Q/q T/t A/a Z/z
// ------------------------------------------------------------
function svgPathToSubpaths(d, curveSteps) {
  const subpaths = [];
  let current = [];
  let cx = 0, cy = 0, sx = 0, sy = 0;
  let prevCmd = null;   // 直前のコマンド(大文字)
  let prevCtrl = null;  // S/T の反射制御点用
  let pos = 0;
  const len = d.length;

  function skipSep() {
    while (pos < len && /[\s,]/.test(d[pos])) pos++;
  }
  function readNumber() {
    skipSep();
    const m = /^[+-]?(\d+\.\d+|\.\d+|\d+)(?:[eE][+-]?\d+)?/.exec(d.slice(pos));
    if (!m) return 0;
    pos += m[0].length;
    return parseFloat(m[0]);
  }
  function readFlag() {
    skipSep();
    const c = d[pos];
    if (c === '0' || c === '1') { pos++; return c === '1' ? 1 : 0; }
    return readNumber() ? 1 : 0;
  }
  function pushPt(x, y) { current.push({ x, y }); }
  function endSubpath() {
    if (current.length >= 2) subpaths.push(current);
    current = [];
  }

  while (pos < len) {
    skipSep();
    if (pos >= len) break;

    const ch = d[pos];
    let cmd;
    if (/[a-zA-Z]/.test(ch)) {
      cmd = ch;
      pos++;
    } else if (prevCmd) {
      // 数値が続く場合は直前コマンドの暗黙の繰り返し(MはLとして扱う)
      cmd = prevCmd === 'M' ? 'L' : prevCmd === 'm' ? 'l' : prevCmd;
    } else {
      break;
    }

    const rel = cmd === cmd.toLowerCase();
    const C = cmd.toUpperCase();

    switch (C) {
      case 'M': {
        const x = readNumber(), y = readNumber();
        if (current.length) endSubpath();
        cx = rel ? cx + x : x;
        cy = rel ? cy + y : y;
        sx = cx; sy = cy;
        pushPt(cx, cy);
        prevCtrl = null;
        break;
      }
      case 'L': {
        const x = readNumber(), y = readNumber();
        cx = rel ? cx + x : x;
        cy = rel ? cy + y : y;
        pushPt(cx, cy);
        prevCtrl = null;
        break;
      }
      case 'H': {
        const x = readNumber();
        cx = rel ? cx + x : x;
        pushPt(cx, cy);
        prevCtrl = null;
        break;
      }
      case 'V': {
        const y = readNumber();
        cy = rel ? cy + y : y;
        pushPt(cx, cy);
        prevCtrl = null;
        break;
      }
      case 'C': {
        const x1 = readNumber(), y1 = readNumber();
        const x2 = readNumber(), y2 = readNumber();
        const x  = readNumber(), y  = readNumber();
        const p0 = { x: cx, y: cy };
        const p1 = { x: rel ? cx + x1 : x1, y: rel ? cy + y1 : y1 };
        const p2 = { x: rel ? cx + x2 : x2, y: rel ? cy + y2 : y2 };
        const p3 = { x: rel ? cx + x  : x,  y: rel ? cy + y  : y  };
        flattenCubicBezier(p0, p1, p2, p3, curveSteps, pushPt);
        cx = p3.x; cy = p3.y;
        prevCtrl = p2;
        break;
      }
      case 'S': {
        const x2 = readNumber(), y2 = readNumber();
        const x  = readNumber(), y  = readNumber();
        const p0 = { x: cx, y: cy };
        const p1 = (prevCtrl && (prevCmd === 'C' || prevCmd === 'c' || prevCmd === 'S' || prevCmd === 's'))
          ? { x: 2 * cx - prevCtrl.x, y: 2 * cy - prevCtrl.y }
          : p0;
        const p2 = { x: rel ? cx + x2 : x2, y: rel ? cy + y2 : y2 };
        const p3 = { x: rel ? cx + x  : x,  y: rel ? cy + y  : y  };
        flattenCubicBezier(p0, p1, p2, p3, curveSteps, pushPt);
        cx = p3.x; cy = p3.y;
        prevCtrl = p2;
        break;
      }
      case 'Q': {
        const x1 = readNumber(), y1 = readNumber();
        const x  = readNumber(), y  = readNumber();
        const p0 = { x: cx, y: cy };
        const p1 = { x: rel ? cx + x1 : x1, y: rel ? cy + y1 : y1 };
        const p2 = { x: rel ? cx + x  : x,  y: rel ? cy + y  : y  };
        flattenQuadBezier(p0, p1, p2, curveSteps, pushPt);
        cx = p2.x; cy = p2.y;
        prevCtrl = p1;
        break;
      }
      case 'T': {
        const x = readNumber(), y = readNumber();
        const p0 = { x: cx, y: cy };
        const p1 = (prevCtrl && (prevCmd === 'Q' || prevCmd === 'q' || prevCmd === 'T' || prevCmd === 't'))
          ? { x: 2 * cx - prevCtrl.x, y: 2 * cy - prevCtrl.y }
          : p0;
        const p2 = { x: rel ? cx + x : x, y: rel ? cy + y : y };
        flattenQuadBezier(p0, p1, p2, curveSteps, pushPt);
        cx = p2.x; cy = p2.y;
        prevCtrl = p1;
        break;
      }
      case 'A': {
        const rx = readNumber(), ry = readNumber();
        const xRot = readNumber();
        const largeArc = readFlag();
        const sweep = readFlag();
        const x = readNumber(), y = readNumber();
        const endX = rel ? cx + x : x;
        const endY = rel ? cy + y : y;
        flattenArc(cx, cy, rx, ry, xRot, largeArc, sweep, endX, endY, curveSteps, pushPt);
        cx = endX; cy = endY;
        prevCtrl = null;
        break;
      }
      case 'Z': {
        cx = sx; cy = sy;
        pushPt(cx, cy);
        endSubpath();
        prevCtrl = null;
        break;
      }
      default: {
        // 未対応コマンドは無視(パース位置がずれるのを防ぐため中断)
        pos = len;
        break;
      }
    }

    prevCmd = cmd;
  }

  endSubpath();
  return subpaths;
}

function flattenCubicBezier(p0, p1, p2, p3, steps, pushPt) {
  for (let s = 1; s <= steps; s++) {
    const t = s / steps;
    const mt = 1 - t;
    const a = mt * mt * mt, b = 3 * mt * mt * t, c = 3 * mt * t * t, e = t * t * t;
    pushPt(
      a * p0.x + b * p1.x + c * p2.x + e * p3.x,
      a * p0.y + b * p1.y + c * p2.y + e * p3.y
    );
  }
}

function flattenQuadBezier(p0, p1, p2, steps, pushPt) {
  for (let s = 1; s <= steps; s++) {
    const t = s / steps;
    const mt = 1 - t;
    const a = mt * mt, b = 2 * mt * t, c = t * t;
    pushPt(a * p0.x + b * p1.x + c * p2.x, a * p0.y + b * p1.y + c * p2.y);
  }
}

// SVG楕円弧(A)を中心パラメータ形式に変換し、角度を等分割して点列化する
// (SVG仕様の endpoint-to-center parameterization に準拠)
function flattenArc(x1, y1, rx, ry, xRotDeg, largeArcFlag, sweepFlag, x2, y2, steps, pushPt) {
  if (rx === 0 || ry === 0 || (x1 === x2 && y1 === y2)) {
    pushPt(x2, y2);
    return;
  }
  rx = Math.abs(rx);
  ry = Math.abs(ry);
  const phi = (xRotDeg * Math.PI) / 180;
  const cosPhi = Math.cos(phi), sinPhi = Math.sin(phi);

  const dx2 = (x1 - x2) / 2, dy2 = (y1 - y2) / 2;
  const x1p = cosPhi * dx2 + sinPhi * dy2;
  const y1p = -sinPhi * dx2 + cosPhi * dy2;

  let rxSq = rx * rx, rySq = ry * ry;
  const x1pSq = x1p * x1p, y1pSq = y1p * y1p;

  const lambda = x1pSq / rxSq + y1pSq / rySq;
  if (lambda > 1) {
    const s = Math.sqrt(lambda);
    rx *= s; ry *= s;
    rxSq = rx * rx; rySq = ry * ry;
  }

  const sign = largeArcFlag !== sweepFlag ? 1 : -1;
  const denom = rxSq * y1pSq + rySq * x1pSq;
  let num = rxSq * rySq - rxSq * y1pSq - rySq * x1pSq;
  if (num < 0) num = 0;
  const co = denom === 0 ? 0 : sign * Math.sqrt(num / denom);
  const cxp = (co * rx * y1p) / ry;
  const cyp = (co * -ry * x1p) / rx;

  const centerX = cosPhi * cxp - sinPhi * cyp + (x1 + x2) / 2;
  const centerY = sinPhi * cxp + cosPhi * cyp + (y1 + y2) / 2;

  function angleBetween(ux, uy, vx, vy) {
    const dot = ux * vx + uy * vy;
    const lenProduct = Math.sqrt((ux * ux + uy * uy) * (vx * vx + vy * vy)) || 1;
    let ang = Math.acos(Math.min(1, Math.max(-1, dot / lenProduct)));
    if (ux * vy - uy * vx < 0) ang = -ang;
    return ang;
  }

  const theta1 = angleBetween(1, 0, (x1p - cxp) / rx, (y1p - cyp) / ry);
  let dTheta = angleBetween(
    (x1p - cxp) / rx, (y1p - cyp) / ry,
    (-x1p - cxp) / rx, (-y1p - cyp) / ry
  );
  if (!sweepFlag && dTheta > 0) dTheta -= 2 * Math.PI;
  if (sweepFlag && dTheta < 0) dTheta += 2 * Math.PI;

  const numSteps = Math.max(2, Math.ceil((steps * Math.abs(dTheta)) / (Math.PI / 2)));
  for (let s = 1; s <= numSteps; s++) {
    const t = theta1 + (dTheta * s) / numSteps;
    pushPt(
      cosPhi * rx * Math.cos(t) - sinPhi * ry * Math.sin(t) + centerX,
      sinPhi * rx * Math.cos(t) + cosPhi * ry * Math.sin(t) + centerY
    );
  }
}

// ------------------------------------------------------------
// 図形要素を、パスの d 文字列に変換する
// ------------------------------------------------------------
function shapeToPathData(el, tag) {
  const num = (name, fallback = 0) => {
    const v = parseFloat(el.getAttribute(name));
    return Number.isFinite(v) ? v : fallback;
  };

  if (tag === 'rect') {
    const x = num('x'), y = num('y'), w = num('width'), h = num('height');
    if (w <= 0 || h <= 0) return '';
    let rx = num('rx'), ry = num('ry');
    if (rx > 0 && ry <= 0) ry = rx;
    if (ry > 0 && rx <= 0) rx = ry;
    rx = Math.min(rx, w / 2);
    ry = Math.min(ry, h / 2);
    if (rx <= 0 && ry <= 0) return `M ${x} ${y} H ${x + w} V ${y + h} H ${x} Z`;
    return [
      `M ${x + rx} ${y}`, `H ${x + w - rx}`, `A ${rx} ${ry} 0 0 1 ${x + w} ${y + ry}`,
      `V ${y + h - ry}`, `A ${rx} ${ry} 0 0 1 ${x + w - rx} ${y + h}`,
      `H ${x + rx}`, `A ${rx} ${ry} 0 0 1 ${x} ${y + h - ry}`,
      `V ${y + ry}`, `A ${rx} ${ry} 0 0 1 ${x + rx} ${y}`, 'Z'
    ].join(' ');
  }

  if (tag === 'circle' || tag === 'ellipse') {
    const cx = num('cx'), cy = num('cy');
    const rx = tag === 'circle' ? num('r') : num('rx');
    const ry = tag === 'circle' ? num('r') : num('ry');
    if (rx <= 0 || ry <= 0) return '';
    return [
      `M ${cx - rx} ${cy}`,
      `A ${rx} ${ry} 0 1 0 ${cx + rx} ${cy}`,
      `A ${rx} ${ry} 0 1 0 ${cx - rx} ${cy}`, 'Z'
    ].join(' ');
  }

  if (tag === 'line') {
    return `M ${num('x1')} ${num('y1')} L ${num('x2')} ${num('y2')}`;
  }

  if (tag === 'polyline' || tag === 'polygon') {
    const nums = (el.getAttribute('points') || '').trim().split(/[\s,]+/).map(parseFloat).filter(Number.isFinite);
    if (nums.length < 4) return '';
    const cmds = [`M ${nums[0]} ${nums[1]}`];
    for (let i = 2; i + 1 < nums.length; i += 2) cmds.push(`L ${nums[i]} ${nums[i + 1]}`);
    if (tag === 'polygon') cmds.push('Z');
    return cmds.join(' ');
  }

  return '';
}

// ------------------------------------------------------------
// transform 属性の文字列を、行列 [a, b, c, d, e, f] に変換する
// ------------------------------------------------------------
function mulMatrix(m, n) { // m × n(nを先に適用)
  return [
    m[0] * n[0] + m[2] * n[1], m[1] * n[0] + m[3] * n[1],
    m[0] * n[2] + m[2] * n[3], m[1] * n[2] + m[3] * n[3],
    m[0] * n[4] + m[2] * n[5] + m[4], m[1] * n[4] + m[3] * n[5] + m[5]
  ];
}

function parseTransform(str) {
  let m = [1, 0, 0, 1, 0, 0];
  if (!str) return m;
  const re = /(matrix|translate|scale|rotate|skewX|skewY)\s*\(([^)]*)\)/g;
  let hit;
  while ((hit = re.exec(str))) {
    const a = hit[2].trim().split(/[\s,]+/).map(parseFloat);
    let t = null;
    switch (hit[1]) {
      case 'matrix':    if (a.length >= 6) t = a.slice(0, 6); break;
      case 'translate': t = [1, 0, 0, 1, a[0] || 0, a[1] || 0]; break;
      case 'scale':     t = [a[0], 0, 0, a.length > 1 ? a[1] : a[0], 0, 0]; break;
      case 'skewX':     t = [1, 0, Math.tan((a[0] || 0) * Math.PI / 180), 1, 0, 0]; break;
      case 'skewY':     t = [1, Math.tan((a[0] || 0) * Math.PI / 180), 0, 1, 0, 0]; break;
      case 'rotate': {
        const r = (a[0] || 0) * Math.PI / 180, c = Math.cos(r), s = Math.sin(r);
        t = [c, s, -s, c, 0, 0];
        if (a.length >= 3) { // rotate(角度 cx cy) = translate(cx,cy) rotate(角度) translate(-cx,-cy)
          t = mulMatrix(mulMatrix([1, 0, 0, 1, a[1], a[2]], t), [1, 0, 0, 1, -a[1], -a[2]]);
        }
        break;
      }
    }
    if (t && t.every(Number.isFinite)) m = mulMatrix(m, t);
  }
  return m;
}

const NON_RENDERED_TAGS = new Set([
  'defs', 'clippath', 'mask', 'symbol', 'pattern', 'marker',
  'lineargradient', 'radialgradient', 'filter', 'metadata', 'title', 'desc'
]);

// ------------------------------------------------------------
// SVGの文字列から、描画される図形をすべて点列(サブパス)にして返す。失敗したら null
// ------------------------------------------------------------
function parseSvgTracks(text, curveSteps) {
  const doc = new DOMParser().parseFromString(text, 'image/svg+xml');
  const root = doc.documentElement;
  if (!root || root.nodeName.toLowerCase() !== 'svg' || doc.querySelector('parsererror')) return null;

  const subpaths = [];
  root.querySelectorAll('path, line, polyline, polygon, rect, circle, ellipse').forEach(el => {
    // 要素から根に向かって、transform を掛け合わせる。描画されない領域の中なら無視する
    let m = [1, 0, 0, 1, 0, 0];
    for (let n = el; n && n !== root; n = n.parentElement) {
      if (n !== el && NON_RENDERED_TAGS.has(n.tagName.toLowerCase())) return;
      m = mulMatrix(parseTransform(n.getAttribute('transform')), m);
    }

    const tag = el.tagName.toLowerCase();
    const d = tag === 'path' ? (el.getAttribute('d') || '') : shapeToPathData(el, tag);
    if (!d) return;

    svgPathToSubpaths(d, curveSteps).forEach(sp => {
      subpaths.push(sp.map(p => ({
        x: m[0] * p.x + m[2] * p.y + m[4],
        y: m[1] * p.x + m[3] * p.y + m[5]
      })));
    });
  });

  return { viewBox: readSvgViewBox(root), subpaths };
}
