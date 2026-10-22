// =============================================================
// マウスのドラッグで引いた線の上を、tile画像が流れるベルトコンベア風スケッチ
//
// 構成:
//   1. ドラッグ中のマウス座標を一定間隔でサンプリングして1本のトラック(折れ線)にする
//   2. トラックごとに専用のマスク・円バッファを持つ(他の線の領域にはみ出さない)
//      - 描いている最中の線: キャンバス全面サイズの共有バッファを使い回す
//      - 描き終わった線: バウンディングボックス分だけの小さいバッファを1回だけ作る
//   3. 円の間隔は CIRCLE_DIAMETER + CIRCLE_GAP の固定値。
//      開いたパスなので無限に続くコンベアとして扱う
//
// 操作: ドラッグで線を描く / z キーで1本戻す / c キーで全消去
// =============================================================

let pieces = [];       // ピース画像(p5.Image)。null は画像を入れていない空の箱
let tintedPieces = []; // circleColorでtint(色付け)済みの生Canvas(piecesと同じ並び。buildTintedImagesで生成)
let tracks       = []; // 各線のデータ { pts, segLens, total, mask: { g, bx, by, bw, bh }, cLayer }
let activeTrack  = null; // ドラッグ中の線(なければnull)
let liveMask, liveCircle; // ドラッグ中の線だけが使う、キャンバス全面サイズの共有バッファ

// --- キャンバスサイズ ---
const INITIAL_W = 630;
const INITIAL_H = 891;

// --- 見た目・動きの調整パラメータ ---
// 値の本体は PARAMS(Tweakpaneのスライダーが書き換える)。
// 毎フレーム syncParams() で下のグローバル変数へ写して使う。
const PARAMS = {
  circleDiameter: 32,    // 円の直径(px)
  circleGapRatio: -0.9, // 円と円の間隔(円の直径に対する比率)。0なら隙間なしでぴったり並ぶ。-1に近いほど重なる
  beltSpeed:      0.2,   // 1フレームあたりの移動量(px)
  trackWidth:     43,    // トラック(ストローク)の太さ
  minPointDist:   4,     // ドラッグ中、前の点からこの距離以上動いたら新しい点を追加する(px)
  jitterAmount:   7,     // レール(進行方向)に垂直な向きへのずれの最大値(px)。0でずれなし
  zoneScale:      0.3,   // 「揺れる区間/揺れない区間」の粗さ。小さいほど長い区間で切り替わる
  zoneThreshold:  0.5,   // このゾーンノイズ値を境に揺れON/OFFが切り替わる(0〜1)
  zoneSoftness:   0.01,  // ON/OFFの境界のなめらかさ。0に近いほどくっきりコントラストが出る
  showTrack:      true,  // トラックの白い縁取りを表示するか
  bkgdColor:      "#ffffff",
  trackColor:     "#ffffff",
  circleColor:    "#ffffff",
};

const DEFAULT_PARAMS = { ...PARAMS }; // Reset用の初期値(localStorage復元より前に控える)

const MAX_PIECES = 8;     // ピースの箱の最大数
const MAX_PIECE_PX = 256; // ピース画像の長辺の上限(px)。これを超える画像は読み込み時に縮小する

const CURVE_STEPS = 100; // SVG読み込み用:曲線1本あたりの分割数(多いほど滑らかで重い)

let CIRCLE_DIAMETER, CIRCLE_GAP, BELT_SPEED, TRACK_WIDTH, MIN_POINT_DIST;
let JITTER_AMOUNT, ZONE_SCALE, ZONE_THRESHOLD, ZONE_SOFTNESS, SHOW_TRACK;
let bkgdColor, trackColor, circleColor; // bkgdColorはUSE_TRANSPARENT_BG=trueの場合は使われない

const USE_TRANSPARENT_BG = false; // 背景を透明にするか(シール表示用)

const STORAGE_KEY = 'conveyor-brush-params';
let pane;

function loadParams() {
  try {
    const saved = JSON.parse(localStorage.getItem(STORAGE_KEY));
    if (!saved) return;
    for (const key of Object.keys(PARAMS)) {
      if (typeof saved[key] === typeof PARAMS[key]) PARAMS[key] = saved[key];
    }
  } catch (e) {}
}

function saveParams() {
  try {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(PARAMS));
  } catch (e) {}
}

// PARAMS -> グローバル変数へ反映。太さ・色が変わったときだけバッファを作り直す
function syncParams() {
  const widthChanged = PARAMS.trackWidth !== TRACK_WIDTH;
  const colorChanged = PARAMS.circleColor !== circleColor;

  CIRCLE_DIAMETER = PARAMS.circleDiameter;
  CIRCLE_GAP      = PARAMS.circleGapRatio * PARAMS.circleDiameter; // 直径に比例した間隔(px)
  BELT_SPEED      = PARAMS.beltSpeed;
  TRACK_WIDTH     = PARAMS.trackWidth;
  MIN_POINT_DIST  = PARAMS.minPointDist;
  JITTER_AMOUNT   = PARAMS.jitterAmount;
  ZONE_SCALE      = PARAMS.zoneScale;
  ZONE_THRESHOLD  = PARAMS.zoneThreshold;
  ZONE_SOFTNESS   = PARAMS.zoneSoftness;
  SHOW_TRACK      = PARAMS.showTrack;
  bkgdColor       = PARAMS.bkgdColor;
  trackColor      = PARAMS.trackColor;
  circleColor     = PARAMS.circleColor;

  if (widthChanged && tracks) {
    tracks.forEach(t => { if (t !== activeTrack) buildLayers(t); });
  }
  if (colorChanged && pieces.some(Boolean)) buildTintedImages();
}

// ------------------------------------------------------------
// 調整パネル(Tweakpane)
// ------------------------------------------------------------
function setupPane() {
  pane = new Tweakpane.Pane({
    title: 'CONVEYOR BRUSH',
    container: document.getElementById('pane-holder')
  });

  const tile = pane.addFolder({ title: 'Piece' });
  tile.addBinding(PARAMS, 'circleDiameter', { label: 'piece size', min: 1, max: 100, step: 1 });
  tile.addBinding(PARAMS, 'circleGapRatio', { label: 'piece gap', min: -0.99, max: 0, step: 0.001 });
  tile.addBinding(PARAMS, 'beltSpeed', { label: 'speed', min: 0, max: 5, step: 0.05 });
  tile.addBinding(PARAMS, 'trackWidth', { label: 'track w', min: 1, max: 200, step: 1 });
  tile.addBinding(PARAMS, 'minPointDist', { label: 'point step', min: 1, max: 100, step: 1 });

  const jitter = pane.addFolder({ title: 'Jitter' });
  jitter.addBinding(PARAMS, 'jitterAmount', { label: 'amount', min: 0, max: 50, step: 0.5 });
  jitter.addBinding(PARAMS, 'zoneScale', { label: 'zone scale', min: 0.01, max: 2, step: 0.01 });
  jitter.addBinding(PARAMS, 'zoneThreshold', { label: 'threshold', min: 0, max: 1, step: 0.01 });
  jitter.addBinding(PARAMS, 'zoneSoftness', { label: 'softness', min: 0.001, max: 0.5, step: 0.001 });

  const palette = pane.addFolder({ title: 'Palette' });
  palette.addBinding(PARAMS, 'showTrack', { label: 'show track' });
  palette.addBinding(PARAMS, 'bkgdColor', { label: 'bg' });
  palette.addBinding(PARAMS, 'trackColor', { label: 'track color' });
  palette.addBinding(PARAMS, 'circleColor', { label: 'piece color' });

  pane.addButton({ title: 'Reset Params' }).on('click', () => {
    Object.assign(PARAMS, DEFAULT_PARAMS);
    pane.refresh();
    saveParams();
  });
  pane.addButton({ title: 'Undo' }).on('click', () => { if (!activeTrack) tracks.pop(); });
  pane.addButton({ title: 'Clear Canvas' }).on('click', () => { if (!activeTrack) tracks = []; });

  pane.addButton({ title: 'Export PNG' }).on('click', () => saveCanvas('conveyor-brush', 'png'));

  pane.on('change', saveParams);
}

let beltOffset = 0;

// ============================================================
// setup / draw
// ============================================================

function preload() {
  pieces = [loadImage("tile.png"), loadImage("tile2.png")];
}

function setup() {
  pixelDensity(displayDensity()); // ★高DPI画面でもマスク/円バッファがジャギらないようにする
  const cnv = createCanvas(INITIAL_W, INITIAL_H);
  cnv.parent('sketch-holder');

  pieces = pieces.map(limitPieceSize);

  loadParams();
  syncParams();
  setupPane();
  setupTilePreview();
  buildTintedImages(); // circleColorで着色したtile画像を1回だけ生成する

  liveMask   = createGraphics(width, height);
  liveCircle = createGraphics(width, height);
}

function draw() {
  syncParams();

  if (USE_TRANSPARENT_BG) {
    clear();
  } else {
    background(bkgdColor);
  }

  beltOffset += BELT_SPEED;

  if (activeTrack) drawLiveMask(activeTrack);

  if (SHOW_TRACK) {
    tracks.forEach(track => drawTrack(track));
  }

  drawCirclesWithMask();
}

// ------------------------------------------------------------
// ピースのプレビュー(右パネル)
// 箱1つ = ピース1個。画像をドラッグ&ドロップ(またはクリックで選択)すると差し代わる。
// 箱にマウスを乗せると右上に✕が出て、押すと箱ごと削除。下の＋で空の箱を追加する。
// コンベア上には、画像の入っている箱が上から順に交互に流れる。
// ------------------------------------------------------------
function setupTilePreview() {
  // ドロップ先以外に落としたとき、ブラウザが画像を開いてしまうのを防ぐ
  window.addEventListener('dragover', e => e.preventDefault());
  window.addEventListener('drop', e => e.preventDefault());
  renderTilePreview();
}

// 削除できるのは、箱が2個以上あり、かつ消しても画像入りの箱が1個以上残る場合
function canRemovePiece(i) {
  return pieces.length >= 2 && (!pieces[i] || pieces.some((p, j) => j !== i && p));
}

// piecesの内容から右パネルを作り直す
function renderTilePreview() {
  const holder = document.getElementById('preview-holder');
  holder.innerHTML = '';

  pieces.forEach((img, i) => {
    const slot = document.createElement('div');
    slot.className = 'tile-slot';
    slot.innerHTML =
      `<div class="tile-slot-label">Piece${i + 1}</div>` +
      `<div class="tile-slot-drop"><img alt=""><span>DROP IMAGE or CLICK</span><button class="tile-slot-close" type="button">✕</button></div>`;
    holder.appendChild(slot);

    const drop = slot.querySelector('.tile-slot-drop');
    if (img) {
      drop.querySelector('img').src = img.canvas.toDataURL();
      drop.classList.add('filled');
    }
    if (canRemovePiece(i)) drop.classList.add('can-remove');

    drop.addEventListener('dragenter', e => { e.preventDefault(); drop.classList.add('over'); });
    drop.addEventListener('dragover',  e => { e.preventDefault(); drop.classList.add('over'); });
    drop.addEventListener('dragleave', () => drop.classList.remove('over'));
    drop.addEventListener('drop', e => {
      e.preventDefault();
      drop.classList.remove('over');
      replaceTile(i, e.dataTransfer.files[0]);
    });
    // ✕ボタン: クリックでファイル選択が開かないようにして、箱ごと削除する
    drop.querySelector('.tile-slot-close').addEventListener('click', e => {
      e.stopPropagation();
      removePiece(i);
    });
    drop.addEventListener('click', () => {
      const input = document.createElement('input');
      input.type = 'file';
      input.accept = 'image/*';
      input.onchange = () => replaceTile(i, input.files[0]);
      input.click();
    });
  });

  if (pieces.length < MAX_PIECES) {
    const add = document.createElement('button');
    add.className = 'tile-add';
    add.type = 'button';
    add.textContent = '＋';
    add.addEventListener('click', addPiece);
    holder.appendChild(add);
  }
}

// 空の箱を1つ追加する(画像を入れるまでは描画に使われない)
function addPiece() {
  if (pieces.length >= MAX_PIECES) return;
  pieces.push(null);
  renderTilePreview();
  const holder = document.getElementById('preview-holder');
  holder.scrollTop = holder.scrollHeight;
}

// 箱を1つ削除する
function removePiece(i) {
  if (!canRemovePiece(i)) return;
  pieces.splice(i, 1);
  buildTintedImages();
  renderTilePreview();
}

// 長辺が MAX_PIECE_PX を超えるときだけ、縦横比を保ったまま縮小する。
// 毎フレーム大きな画像を縮小して描くと重いので、読み込み時に1回だけ小さくしておく。
function limitPieceSize(img) {
  const longSide = Math.max(img.width, img.height);
  if (longSide > MAX_PIECE_PX) {
    const k = MAX_PIECE_PX / longSide;
    img.resize(Math.max(1, Math.round(img.width * k)), Math.max(1, Math.round(img.height * k)));
  }
  return img;
}

function replaceTile(i, file) {
  if (!file || !file.type.startsWith('image/')) return;

  const reader = new FileReader();
  reader.onload = () => {
    loadImage(reader.result, img => {
      if (i >= pieces.length) return; // 読み込み中に箱が削除された場合
      pieces[i] = limitPieceSize(img);
      buildTintedImages();
      renderTilePreview();
    }, () => alert('画像を読み込めませんでした'));
  };
  reader.readAsDataURL(file);
}

// ------------------------------------------------------------
// マウス操作
// ------------------------------------------------------------
function mousePressed(e) {
  if (e && e.target && e.target.tagName !== 'CANVAS') return;
  if (mouseX < 0 || mouseX > width || mouseY < 0 || mouseY > height) return;

  activeTrack = {
    pts: [{ x: mouseX, y: mouseY }],
    segLens: [],
    total: 0,
    mask: { g: liveMask, bx: 0, by: 0, bw: width, bh: height },
    cLayer: liveCircle
  };
  tracks.push(activeTrack);
}

function mouseDragged() {
  if (!activeTrack) return;

  const last = activeTrack.pts[activeTrack.pts.length - 1];
  const d = dist(last.x, last.y, mouseX, mouseY);
  if (d < MIN_POINT_DIST) return;

  activeTrack.pts.push({ x: mouseX, y: mouseY });
  activeTrack.segLens.push(d);
  activeTrack.total += d;
}

function mouseReleased() {
  if (!activeTrack) return;

  const track = activeTrack;
  activeTrack = null;

  if (track.pts.length < 2) {
    // クリックだけで線にならなかった場合は捨てる
    tracks.splice(tracks.indexOf(track), 1);
    return;
  }
  buildLayers(track); // 共有バッファから、この線専用の小さいバッファに切り替える
}

function keyPressed() {
  if (key === 'z' && tracks.length && !activeTrack) tracks.pop();
  if (key === 'c' && !activeTrack) tracks = [];
}

// ============================================================
// SVG読み込み機能(現在は無効。後で復活させる予定のため仕様を残している)
//
// 復活手順:
//   1. index.html の svg-import.js の読み込みはそのまま残してある
//   2. setup() で  mySvg = new SvgImport("text_4_round.svg");  を呼ぶ
//   3. draw() で  mySvg.isReady になったら1回だけ  prepareTracks();  を呼ぶ
//      (tracks に各パスが追加され、ドラッグで描いた線と同じ仕組みで描画される)
// ============================================================
let mySvg;

// ------------------------------------------------------------
// 各パーツ(path / polyline / line)のd属性をSVGパスパーサーで解釈し、
// 曲線(C/S/Q/T/A)も分割して点列化した上で、キャンバス座標に変換し、
// 区間長も事前計算しておく
// ------------------------------------------------------------
function prepareTracks() {
  const vb = mySvg.svgViewBox;
  const sf = min(width / vb.width, height / vb.height);
  const offsetX = (width  - vb.width  * sf) * 0.5;
  const offsetY = (height - vb.height * sf) * 0.5;

  const toCanvas = (px, py) => ({
    x: offsetX + (px - vb.minX) * sf,
    y: offsetY + (py - vb.minY) * sf
  });

  tracks = [];

  mySvg.parts.forEach(part => {
    part.style.hasFill   = false;
    part.style.hasStroke = false;

    // 1つの<path>にMで区切られた複数の独立したサブパスが
    // 含まれる場合があるので、それぞれ別トラックとして扱う
    // (曲線コマンドも正しく分割される)
    const subpaths = svgPathToSubpaths(part.d, CURVE_STEPS);

    subpaths.forEach(rawPts => {
      if (rawPts.length < 2) return;

      const pts = rawPts.map(p => toCanvas(p.x, p.y));

      const segLens = [];
      let total = 0;
      for (let i = 0; i < pts.length - 1; i++) {
        const d = dist(pts[i].x, pts[i].y, pts[i + 1].x, pts[i + 1].y);
        segLens.push(d);
        total += d;
      }

      const track = { pts, segLens, total };
      buildLayers(track); // 描画用のマスク・円バッファを作る
      tracks.push(track);
    });
  });
}

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
// tile/tile2画像に circleColor でtint(色付け)した版を1回だけ作っておく。
// multiplyでtile画像の色に色を掛け合わせ、destination-inで元の透明度(形)を
// 復元することで、p5のtint()と同様に元の柄・濃淡を保ったまま色だけ変える。
// ------------------------------------------------------------
function buildTintedImages() {
  // 空の箱(null)は null のまま。描画側では画像の入っている箱だけを使う
  tintedPieces = pieces.map(img => img ? tintImage(img, circleColor) : null);
}

function tintImage(img, hexColor) {
  const g = createGraphics(img.width, img.height);
  const raw = img.canvas || img.elt;

  g.image(img, 0, 0, img.width, img.height);

  const ctx = g.drawingContext;
  ctx.globalCompositeOperation = "multiply";
  ctx.fillStyle = hexColor;
  ctx.fillRect(0, 0, img.width, img.height);

  ctx.globalCompositeOperation = "destination-in";
  ctx.drawImage(raw, 0, 0, img.width, img.height);

  ctx.globalCompositeOperation = "source-over";
  return g.canvas || g.elt;
}

// ------------------------------------------------------------
// 描き終わった線のために、バウンディングボックス分だけの専用マスクと
// 円バッファを1回だけ生成する(毎フレーム createGraphics しない)
// ------------------------------------------------------------
function buildLayers(track) {
  const margin = TRACK_WIDTH / 2 + 4;
  let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
  track.pts.forEach(p => {
    minX = min(minX, p.x); maxX = max(maxX, p.x);
    minY = min(minY, p.y); maxY = max(maxY, p.y);
  });

  const bx = floor(minX - margin);
  const by = floor(minY - margin);
  const bw = ceil((maxX - minX) + margin * 2);
  const bh = ceil((maxY - minY) + margin * 2);

  const g = createGraphics(bw, bh);
  strokePolyline(g, track.pts, bx, by);

  track.mask   = { g, bx, by, bw, bh };
  track.cLayer = createGraphics(bw, bh);
}

// ドラッグ中の線のマスクを、共有バッファに毎フレーム描き直す
function drawLiveMask(track) {
  liveMask.clear();
  strokePolyline(liveMask, track.pts, 0, 0);
}

function strokePolyline(g, pts, bx, by) {
  g.noFill();
  g.stroke(255);
  g.strokeWeight(TRACK_WIDTH);
  g.strokeJoin(MITER);
  g.strokeCap(PROJECT);
  g.beginShape();
  pts.forEach(p => g.vertex(p.x - bx, p.y - by));
  g.endShape();
}

// ------------------------------------------------------------
// 元の頂点だけを直線でつないでトラックを描く(見た目用)
// ------------------------------------------------------------
function drawTrack(track) {
  if (track.pts.length < 2) return;

  noFill();
  stroke(trackColor);
  strokeWeight(TRACK_WIDTH);
  strokeJoin(MITER);
  strokeCap(PROJECT);

  beginShape();
  track.pts.forEach(p => vertex(p.x, p.y));
  endShape(); // CLOSEしない = 始点と終点はつながない(開いたパス)
}

// (idx, i) の組から常に同じ値を返す疑似乱数(0〜1)。
// フレームごとに再計算しても同じタイルは同じ値になる = ジッターがチラつかない
function hashRandom(idx, i) {
  const x = Math.sin(idx * 12.9898 + i * 78.233) * 43758.5453;
  return x - Math.floor(x);
}

function pointAtDistance(track, distance) {
  const { pts, segLens, total } = track;
  if (pts.length < 2 || total === 0) return null;
  if (distance < 0 || distance > total) return null;

  let remaining = distance;
  for (let i = 0; i < segLens.length; i++) {
    if (remaining <= segLens[i]) {
      const t = segLens[i] === 0 ? 0 : remaining / segLens[i];
      const a = pts[i];
      const b = pts[i + 1];
      return {
        x: lerp(a.x, b.x, t),
        y: lerp(a.y, b.y, t),
        angle: Math.atan2(b.y - a.y, b.x - a.x) // ★進行方向の角度を追加
      };
    }
    remaining -= segLens[i];
  }
  const last = pts[pts.length - 1];
  const prev = pts[pts.length - 2];
  return {
    x: last.x, y: last.y,
    angle: Math.atan2(last.y - prev.y, last.x - prev.x)
  };
}

function pointAtExtendedDistance(track, distance) {
  const { pts, total } = track;
  if (pts.length < 2) return { x: pts[0].x, y: pts[0].y, angle: 0 };

  if (distance < 0) {
    const a = pts[0], b = pts[1];
    const dx = a.x - b.x, dy = a.y - b.y;
    const len = Math.hypot(dx, dy) || 1;
    const t = -distance;
    return {
      x: a.x + (dx / len) * t,
      y: a.y + (dy / len) * t,
      angle: Math.atan2(b.y - a.y, b.x - a.x) // 始点側の向き
    };
  }
  if (distance > total) {
    const a = pts[pts.length - 2], b = pts[pts.length - 1];
    const dx = b.x - a.x, dy = b.y - a.y;
    const len = Math.hypot(dx, dy) || 1;
    const t = distance - total;
    return {
      x: b.x + (dx / len) * t,
      y: b.y + (dy / len) * t,
      angle: Math.atan2(dy, dx) // 終点側の向き
    };
  }
  return pointAtDistance(track, distance);
}

// ------------------------------------------------------------
// トラックごとに使い回しの円バッファへ円を描き、
// 使い回しのマスクでクリップしてから、
// バウンディングボックスの位置にだけ合成する。
//
// createGraphics は毎フレーム呼ばない(buildMasksで1回だけ生成済み)。
// clear() で内容だけリセットするので高速。
// ------------------------------------------------------------
function drawCirclesWithMask() {
  const spacing = CIRCLE_DIAMETER + CIRCLE_GAP;

  // ★画像の縦横比を保ったまま、高さをCIRCLE_DIAMETERに合わせる(ループ外で1回だけ計算)
  // ピースごとにアスペクト比が異なる場合があるので、描画幅は個別に用意する。
  // 画像の入っている箱だけを順に使い、スロット番号で巡回して交互に流す
  const drawH = CIRCLE_DIAMETER;
  const halfH = drawH / 2;
  const items = [];
  pieces.forEach((img, k) => {
    if (img && tintedPieces[k]) items.push({ raw: tintedPieces[k], w: drawH * (img.width / img.height) });
  });
  if (items.length === 0) return;

  tracks.forEach((track, idx) => {
    const total = track.total;
    if (total === 0) return;

    const mask   = track.mask;
    const cLayer = track.cLayer;
    if (!mask || !cLayer) return;

    cLayer.clear();

    const extra = CIRCLE_DIAMETER; // 端の外側にもはみ出させる余白
    const phase = ((beltOffset % spacing) + spacing) % spacing;

    const iMin = Math.floor((-extra - phase) / spacing);
    const iMax = Math.ceil((total + extra - phase) / spacing);

    // ★何周目か(beltOffsetがspacingを何回超えたか)。jitterのスロット番号から
    //   巡回による付け替わりを打ち消すために使う
    const beltStep = Math.floor(beltOffset / spacing);

    // ★p5のpush/translate/rotate/image(状態管理のオーバーヘッドが大きい)を使わず、
    //   生のCanvas2D APIを直接呼ぶことで、画像枚数が多いときの描画コストを下げる
    // ★元画像ではなく、circleColorでtint済みの版(tintedPieces)を描画に使う
    const ctx = cLayer.drawingContext;

    for (let i = iMin; i <= iMax; i++) {
      const distAlong = i * spacing + phase;
      if (distAlong < -extra || distAlong > total + extra) continue;

      const p = pointAtExtendedDistance(track, distAlong);
      if (!p) continue;

      // ★進行方向(p.angle)に垂直な法線方向へ、タイルごとに固定のランダム量だけずらす
      // iはベルト一周ごとに巡回して使い回される「スロット番号」でしかないため、
      // そのまま使うと同じ場所を流れるタイルのジッターが周期的に変わってしまう。
      // beltStep(周回数)を差し引いた slotId は巡回に依存しない固定の識別子になる。
      const slotId = i - beltStep;
      const normalAngle = p.angle + Math.PI / 2;

      // ★低周波のnoise()で「揺れる区間/揺れない区間」のゾーンを作り、
      //   区間の中だけhashRandomの揺れを効かせることでコントラストを出す。
      //   境界だけZONE_SOFTNESSの幅でなめらかにして、タイル単位でのパチパチした
      //   切り替わりを防ぐ(ZONE_SOFTNESSを0に近づけるほどくっきりしたON/OFFになる)。
      const zoneNoise = noise(idx * 500 + slotId * ZONE_SCALE);
      const gate = constrain((zoneNoise - (ZONE_THRESHOLD - ZONE_SOFTNESS)) / (2 * ZONE_SOFTNESS), 0, 1);

      const jitter = gate * (hashRandom(idx, slotId) * 2 - 1) * JITTER_AMOUNT;
      const jx = p.x + Math.cos(normalAngle) * jitter;
      const jy = p.y + Math.sin(normalAngle) * jitter;

      // ★スロット番号でピースを巡回して交互に描画する(2個なら従来のtile/tile2の交互配置)
      const item = items[((slotId % items.length) + items.length) % items.length];
      const rawImg = item.raw;
      const drawW  = item.w;

      ctx.save();
      ctx.translate(jx - mask.bx, jy - mask.by);
      ctx.rotate(p.angle); // ★進行方向に向けて回転
      ctx.drawImage(rawImg, -drawW / 2, -halfH, drawW, drawH);
      ctx.restore();
    }

    // このトラック専用の小さいマスクでクリップ(他パーツと混ざらない)
    // ★JSのピクセルループ(loadPixels/updatePixels)ではなく、
    //   Canvas2Dのcompositing(destination-in)でGPU側にクリップさせる。
    //   マスクのアンチエイリアスされた縁もそのまま活きるので、見た目は変わらず高速。
    ctx.save();
    ctx.globalCompositeOperation = "destination-in";
    ctx.drawImage(mask.g.canvas || mask.g.elt, 0, 0, mask.bw, mask.bh);
    ctx.restore();

    image(cLayer, mask.bx, mask.by);
  });
}
