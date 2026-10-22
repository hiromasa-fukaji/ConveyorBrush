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
//   4. ピース(流すタイル)は右パネルの箱に画像またはSVGをドロップして登録する。
//      SVGピースは、画面用にラスター化しつつ元のSVGも保持し、Export SVGで使う
//
// 操作: ドラッグで線を描く / z キーで1本戻す / c キーで全消去
// =============================================================

let pieces = [];       // ピース画像(p5.Image)。null は画像を入れていない空の箱
let tintedPieces = []; // circleColorでtint(色付け)済みの生Canvas(piecesと同じ並び。buildTintedImagesで生成)
let pieceMeta = [];    // piecesと同じ並び。SVGピースなら { svgText, viewBox }、PNGなどのラスター画像・空の箱は null
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

const SIMPLIFY_EPSILON = 0.5; // SVG書き出し時、線の点を間引く許容誤差(px)
const MAX_PIECES = 8;     // ピースの箱の最大数
const MAX_PIECE_PX = 256; // ピース画像の長辺の上限(px)。これを超える画像は読み込み時に縮小する

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

  pane.addButton({ title: 'Export SVG' }).on('click', exportSVG);
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
  pieceMeta = pieces.map(() => null);

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
  pieceMeta.push(null);
  renderTilePreview();
  const holder = document.getElementById('preview-holder');
  holder.scrollTop = holder.scrollHeight;
}

// 箱を1つ削除する
function removePiece(i) {
  if (!canRemovePiece(i)) return;
  pieces.splice(i, 1);
  pieceMeta.splice(i, 1);
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
  if (!file) return;
  const isSvg = file.type === 'image/svg+xml' || /\.svg$/i.test(file.name);
  if (!isSvg && !file.type.startsWith('image/')) return;

  if (isSvg) {
    // SVGは、画面用にラスター化した画像と、書き出し用の元のSVGの両方を持つ
    file.text().then(text => {
      const info = prepareSvgPiece(text);
      if (!info) { alert('SVGを読み込めませんでした'); return; }
      loadImage(info.rasterURL,
        img => setPiece(i, limitPieceSize(img), { svgText: text, viewBox: info.viewBox }),
        () => alert('SVGを読み込めませんでした'));
    });
    return;
  }

  const reader = new FileReader();
  reader.onload = () => {
    loadImage(reader.result,
      img => setPiece(i, limitPieceSize(img), null),
      () => alert('画像を読み込めませんでした'));
  };
  reader.readAsDataURL(file);
}

function setPiece(i, img, meta) {
  if (i >= pieces.length) return; // 読み込み中に箱が削除された場合
  pieces[i] = img;
  pieceMeta[i] = meta;
  buildTintedImages();
  renderTilePreview();
}

// SVGのviewBox(なければwidth/height)を {minX, minY, w, h} で返す
function readSvgViewBox(root) {
  const nums = (root.getAttribute('viewBox') || '').trim().split(/[\s,]+/).map(parseFloat);
  if (nums.length === 4 && nums.every(Number.isFinite) && nums[2] > 0 && nums[3] > 0) {
    return { minX: nums[0], minY: nums[1], w: nums[2], h: nums[3] };
  }
  const w = parseFloat(root.getAttribute('width'))  || 100;
  const h = parseFloat(root.getAttribute('height')) || 100;
  return { minX: 0, minY: 0, w, h };
}

// SVGの文字列を解析し、画面用にラスター化するためのデータURLと viewBox を返す(失敗したら null)。
// ブラウザ標準の画像読み込みに任せるので、transform・グラデーション・class なども正しく描かれる。
function prepareSvgPiece(text) {
  const doc = new DOMParser().parseFromString(text, 'image/svg+xml');
  const root = doc.documentElement;
  if (!root || root.nodeName.toLowerCase() !== 'svg' || doc.querySelector('parsererror')) return null;

  const viewBox = readSvgViewBox(root);
  const k = MAX_PIECE_PX / Math.max(viewBox.w, viewBox.h); // 長辺が MAX_PIECE_PX になる倍率

  const clone = root.cloneNode(true);
  clone.setAttribute('viewBox', `${viewBox.minX} ${viewBox.minY} ${viewBox.w} ${viewBox.h}`);
  clone.setAttribute('width',  Math.max(1, Math.round(viewBox.w * k)));
  clone.setAttribute('height', Math.max(1, Math.round(viewBox.h * k)));

  const rasterURL = 'data:image/svg+xml;charset=utf-8,' +
    encodeURIComponent(new XMLSerializer().serializeToString(clone));
  return { rasterURL, viewBox };
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
  const items = getPieceItems();
  if (items.length === 0) return;

  const drawH = CIRCLE_DIAMETER;
  const halfH = drawH / 2;

  tracks.forEach((track, idx) => {
    if (track.total === 0) return;

    const mask   = track.mask;
    const cLayer = track.cLayer;
    if (!mask || !cLayer) return;

    cLayer.clear();

    // ★p5のpush/translate/rotate/image(状態管理のオーバーヘッドが大きい)を使わず、
    //   生のCanvas2D APIを直接呼ぶことで、画像枚数が多いときの描画コストを下げる
    // ★元画像ではなく、circleColorでtint済みの版(tintedPieces)を描画に使う
    const ctx = cLayer.drawingContext;

    forEachPlacement(track, idx, items.length, (n, x, y, angle) => {
      const item = items[n];
      ctx.save();
      ctx.translate(x - mask.bx, y - mask.by);
      ctx.rotate(angle); // ★進行方向に向けて回転
      ctx.drawImage(item.raw, -item.w / 2, -halfH, item.w, drawH);
      ctx.restore();
    });

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

// ★画像の縦横比を保ったまま、高さをCIRCLE_DIAMETERに合わせた描画幅つきで、
//   画像の入っている箱だけを返す。画面描画とSVG書き出しで共有する。
//   k = piecesの添字、raw = 着色済みCanvas、w = 描画幅(px)
function getPieceItems() {
  const drawH = CIRCLE_DIAMETER;
  const items = [];
  pieces.forEach((img, k) => {
    if (img && tintedPieces[k]) items.push({ k, raw: tintedPieces[k], w: drawH * (img.width / img.height) });
  });
  return items;
}

// ------------------------------------------------------------
// トラック上の現在のピース配置を1つずつ fn(n, x, y, angle) に渡す。
// n = 何番目のピースを使うか(0〜nItems-1)、x/y = ジッター込みの中心座標、angle = 進行方向。
// 画面描画とSVG書き出しで同じ配置を使うための共通処理。
// ------------------------------------------------------------
function forEachPlacement(track, idx, nItems, fn) {
  const spacing = CIRCLE_DIAMETER + CIRCLE_GAP;
  const total = track.total;
  if (total === 0) return;

  const extra = CIRCLE_DIAMETER; // 端の外側にもはみ出させる余白
  const phase = ((beltOffset % spacing) + spacing) % spacing;

  const iMin = Math.floor((-extra - phase) / spacing);
  const iMax = Math.ceil((total + extra - phase) / spacing);

  // ★何周目か(beltOffsetがspacingを何回超えたか)。jitterのスロット番号から
  //   巡回による付け替わりを打ち消すために使う
  const beltStep = Math.floor(beltOffset / spacing);

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

    // ★スロット番号でピースを巡回して交互に使う(2個なら従来のtile/tile2の交互配置)
    fn(((slotId % nItems) + nItems) % nItems, jx, jy, p.angle);
  }
}

// ============================================================
// SVG書き出し(Illustrator等で編集する用)
//
// 現在のフレームの配置をそのまま書き出す。構成:
//   <defs>  ピースの定義(SVGピースは元のSVGの中身、PNGなどは<image>で埋め込み)
//   背景・トラックの縁取り
//   線ごとに <g clip-path> でクリップし、中に <use> でピースを配置
//
// 線の形のクリップは、ストロークを多角形に変換して <clipPath> にする
// (Illustratorは <mask> より <clipPath> のほうが安定して読めるため)。
// ============================================================
function exportSVG() {
  const items = getPieceItems();
  const liveTracks = tracks.filter(t => t !== activeTrack && t.pts.length >= 2 && t.total > 0);
  // 白い縁取りとクリップは、同じ間引き後の点を使う(別々だと縁がずれて見える)
  const simplified = new Map(liveTracks.map(t => [t, simplifyPolyline(t.pts, SIMPLIFY_EPSILON)]));
  if (items.length === 0 || liveTracks.length === 0) {
    alert('書き出す線がありません');
    return;
  }

  const f = v => Math.round(v * 100) / 100;
  const drawH = CIRCLE_DIAMETER;
  const tint = color(circleColor).levels;
  const needsTint = tint[0] < 255 || tint[1] < 255 || tint[2] < 255;
  const serializer = new XMLSerializer();

  // --- ピースの定義 ---
  let defs = '';
  items.forEach(it => {
    const id = 'piece-' + it.k;
    const meta = pieceMeta[it.k];
    if (meta) {
      const root = new DOMParser().parseFromString(meta.svgText, 'image/svg+xml').documentElement;
      if (needsTint) tintSvgColors(root, tint);
      const inner = Array.from(root.childNodes).map(n => serializer.serializeToString(n)).join('');
      const vb = meta.viewBox;
      const s = drawH / vb.h;
      // ピースの中心を原点に置き、高さがdrawHになるよう拡大縮小する
      defs += `<g id="${id}" transform="scale(${s}) translate(${f(-(vb.minX + vb.w / 2))} ${f(-(vb.minY + vb.h / 2))})">${inner}</g>\n`;
    } else {
      // ラスター画像は、着色済みの画像をそのまま埋め込む(ベクターにはならない)
      defs += `<image id="${id}" x="${f(-it.w / 2)}" y="${f(-drawH / 2)}" width="${f(it.w)}" height="${f(drawH)}" ` +
              `xlink:href="${it.raw.toDataURL('image/png')}"/>\n`;
    }
  });

  // --- 線ごとのクリップとピース配置 ---
  let clips = '';
  let body = '';
  liveTracks.forEach(track => {
    const idx = tracks.indexOf(track);
    let uses = '';
    forEachPlacement(track, idx, items.length, (n, x, y, angle) => {
      uses += `<use xlink:href="#piece-${items[n].k}" transform="translate(${f(x)} ${f(y)}) rotate(${f(angle * 180 / Math.PI)})"/>\n`;
    });
    if (!uses) return;
    clips += `<clipPath id="clip-${idx}"><path clip-rule="evenodd" d="${strokeOutlinePathD(simplified.get(track), TRACK_WIDTH, f)}"/></clipPath>\n`;
    body += `<g id="line-${idx}" clip-path="url(#clip-${idx})">\n${uses}</g>\n`;
  });

  // --- 背景・トラックの縁取り(画面と同じく、ピースより下に敷く) ---
  let base = '';
  if (!USE_TRANSPARENT_BG) {
    base += `<rect id="background" width="${width}" height="${height}" fill="${bkgdColor}"/>\n`;
  }
  if (SHOW_TRACK) {
    liveTracks.forEach((track, n) => {
      const pts = simplified.get(track).map(p => `${f(p.x)},${f(p.y)}`).join(' ');
      base += `<polyline id="track-${n}" points="${pts}" fill="none" stroke="${trackColor}" stroke-width="${TRACK_WIDTH}" ` +
              `stroke-linejoin="miter" stroke-miterlimit="10" stroke-linecap="square"/>\n`;
    });
  }

  const svg =
    `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" ` +
    `width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">\n` +
    `<defs>\n${defs}${clips}</defs>\n${base}${body}</svg>\n`;

  const blob = new Blob([svg], { type: 'image/svg+xml' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = 'conveyor-brush.svg';
  a.click();
  URL.revokeObjectURL(url);
}

// 太さ w のストロークの外形を、1本の path の d に変換する。
// キャンバスの strokeJoin(MITER) / strokeCap(PROJECT) と同じ形になるようにしてある。
// まず Clipper で外形を作る(重なりの合体も、自己交差でできる穴も、Clipperが処理する)。
// 失敗したときだけ、線分ごとの多角形をすべて重ねた形で出す(巻き方向はそろえてある)。
function strokeOutlinePathD(pts, w, f) {
  if (typeof ClipperLib !== 'undefined') {
    try {
      return clipperStrokeOutlineD(pts, w, f);
    } catch (e) {
      console.warn('線の外形の合体に失敗したため、合体せずに書き出します', e);
    }
  } else {
    console.warn('Clipperが読み込めないため、線の外形を合体せずに書き出します');
  }

  const h = w / 2;
  const polys = [];

  const dirs = [];
  for (let i = 0; i < pts.length - 1; i++) {
    const dx = pts[i + 1].x - pts[i].x, dy = pts[i + 1].y - pts[i].y;
    const len = Math.hypot(dx, dy) || 1;
    dirs.push({ x: dx / len, y: dy / len });
  }

  // 各線分を長方形に(両端は PROJECT キャップとして h だけ延長)
  for (let i = 0; i < dirs.length; i++) {
    const d = dirs[i];
    const nx = -d.y * h, ny = d.x * h;
    const a = { x: pts[i].x - (i === 0 ? d.x * h : 0), y: pts[i].y - (i === 0 ? d.y * h : 0) };
    const last = i === dirs.length - 1;
    const b = { x: pts[i + 1].x + (last ? d.x * h : 0), y: pts[i + 1].y + (last ? d.y * h : 0) };
    polys.push([
      { x: a.x + nx, y: a.y + ny }, { x: b.x + nx, y: b.y + ny },
      { x: b.x - nx, y: b.y - ny }, { x: a.x - nx, y: a.y - ny }
    ]);
  }

  // 折れ角の外側にマイター(とがった継ぎ目)を足す
  for (let i = 1; i < dirs.length; i++) {
    const d1 = dirs[i - 1], d2 = dirs[i];
    const cross = d1.x * d2.y - d1.y * d2.x;
    if (Math.abs(cross) < 1e-6) continue; // ほぼ直線
    const dot = d1.x * d2.x + d1.y * d2.y;
    const sgn = cross > 0 ? -1 : 1;      // 折れ曲がりの外側
    const P = pts[i];
    const n1 = { x: -d1.y * sgn, y: d1.x * sgn };
    const n2 = { x: -d2.y * sgn, y: d2.x * sgn };
    const A = { x: P.x + n1.x * h, y: P.y + n1.y * h };
    const B = { x: P.x + n2.x * h, y: P.y + n2.y * h };
    if (1 + dot > 0.02) { // マイター比が10以内ならとがらせる(超えたら面取り)
      const k = h / (1 + dot);
      polys.push([P, A, { x: P.x + (n1.x + n2.x) * k, y: P.y + (n1.y + n2.y) * k }, B]);
    } else {
      polys.push([P, A, B]);
    }
  }

  // Clipperが使えない(読み込めない・計算に失敗した)場合は、合体せずに重ねたまま出す
  return polys.map(poly => {
    let area = 0;
    for (let i = 0; i < poly.length; i++) {
      const p = poly[i], q = poly[(i + 1) % poly.length];
      area += p.x * q.y - q.x * p.y;
    }
    const ordered = area < 0 ? poly.slice().reverse() : poly;
    return 'M' + ordered.map(p => `${f(p.x)} ${f(p.y)}`).join('L') + 'Z';
  }).join('');
}

// Clipperは整数で計算するので、座標をCLIPPER_SCALE倍してから渡し、結果を戻す
const CLIPPER_SCALE = 100;

function clipperStrokeOutlineD(pts, w, f) {
  const path = pts.map(p => ({ X: Math.round(p.x * CLIPPER_SCALE), Y: Math.round(p.y * CLIPPER_SCALE) }));
  const co = new ClipperLib.ClipperOffset(10, 0.25 * CLIPPER_SCALE); // 第1引数 = マイター比の上限(キャンバスと同じ10)
  co.AddPath(path, ClipperLib.JoinType.jtMiter, ClipperLib.EndType.etOpenSquare); // 開いた線・端は四角く延長(PROJECT)
  const solution = new ClipperLib.Paths();
  co.Execute(solution, (w / 2) * CLIPPER_SCALE);

  return solution.map(ring =>
    'M' + ring.map(p => `${f(p.X / CLIPPER_SCALE)} ${f(p.Y / CLIPPER_SCALE)}`).join('L') + 'Z'
  ).join('');
}

// Ramer–Douglas–Peucker法で、折れ線を許容誤差 epsilon(px)以内で単純化する。
// ドラッグで4pxごとに打った点を間引いて、線の形のクリップを軽くする。
function simplifyPolyline(pts, epsilon) {
  if (pts.length < 3) return pts;
  const keep = new Uint8Array(pts.length);
  keep[0] = keep[pts.length - 1] = 1;
  const stack = [[0, pts.length - 1]];
  while (stack.length) {
    const [lo, hi] = stack.pop();
    const a = pts[lo], b = pts[hi];
    const dx = b.x - a.x, dy = b.y - a.y;
    const len = Math.hypot(dx, dy);
    let maxD = 0, maxI = -1;
    for (let i = lo + 1; i < hi; i++) {
      const d = len === 0
        ? Math.hypot(pts[i].x - a.x, pts[i].y - a.y)
        : Math.abs(dy * (pts[i].x - a.x) - dx * (pts[i].y - a.y)) / len;
      if (d > maxD) { maxD = d; maxI = i; }
    }
    if (maxD > epsilon) {
      keep[maxI] = 1;
      stack.push([lo, maxI], [maxI, hi]);
    }
  }
  return pts.filter((_, i) => keep[i]);
}

// SVGの色指定(fill / stroke / stop-color / flood-color と style 属性内)に、
// 画面と同じ乗算の着色を掛けて書き換える。none / url(...) / currentColor はそのまま。
function tintSvgColors(root, tint) {
  const COLOR_PROPS = ['fill', 'stroke', 'stop-color', 'flood-color'];
  [root, ...root.querySelectorAll('*')].forEach(el => {
    COLOR_PROPS.forEach(prop => {
      const v = el.getAttribute(prop);
      if (v) el.setAttribute(prop, multiplyColor(v, tint));
    });
    const style = el.getAttribute('style');
    if (style) {
      el.setAttribute('style', style.split(';').map(decl => {
        const idx = decl.indexOf(':');
        if (idx === -1) return decl;
        const prop = decl.slice(0, idx).trim();
        return COLOR_PROPS.includes(prop) ? `${prop}:${multiplyColor(decl.slice(idx + 1).trim(), tint)}` : decl;
      }).join(';'));
    }
  });
}

function multiplyColor(value, tint) {
  const v = value.trim();
  if (/^(none|transparent|currentcolor|inherit)$/i.test(v) || v.startsWith('url(')) return value;
  try {
    const c = color(v).levels;
    const hex = n => Math.round(n).toString(16).padStart(2, '0');
    return '#' + hex(c[0] * tint[0] / 255) + hex(c[1] * tint[1] / 255) + hex(c[2] * tint[2] / 255);
  } catch (e) {
    return value;
  }
}
