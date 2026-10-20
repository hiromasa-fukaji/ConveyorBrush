// ==========================================
// svg-import.js
// p5.js SVG Import & Animation Library
// ==========================================

class SvgImport {
  constructor(url) {
    this.parts = [];
    this.svgViewBox = { minX: 0, minY: 0, width: 1, height: 1 };
    this.isReady = false;
    this.loadError = null;

    // URLが渡された場合は即座に読み込みを開始
    if (url) {
      this.load(url);
    }
  }

  async load(url) {
    try {
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`Failed to fetch SVG: ${response.status}`);
      }

      const svgText = await response.text();
      const parser = new DOMParser();
      const doc = parser.parseFromString(svgText, "image/svg+xml");
      const svgEl = doc.querySelector("svg");

      if (!svgEl) {
        throw new Error("No <svg> element found.");
      }

      this.svgViewBox = this.resolveViewBox(svgEl);
      this.parts = [];

      const initialStyle = {
        fill: "black",
        fillOpacity: 1,
        stroke: "none",
        strokeOpacity: 1,
        strokeWidth: 1,
        opacity: 1
      };

      this.collectPartsFromNode(svgEl, initialStyle, this.parts);
      this.isReady = true;
    } catch (error) {
      this.loadError = error;
      console.error("SvgImport Load Error:", error);
    }
  }

  // 描画とアニメーションを実行するメインメソッド
  draw(animationCallback) {
    if (this.loadError) {
      push();
      fill(180, 40, 40);
      noStroke();
      textAlign(CENTER, CENTER);
      text("SVG Load Error", width / 2, height / 2);
      pop();
      return;
    }

    if (!this.isReady) {
      return; // 読み込み中は何も描画しない
    }

    // キャンバスに合わせたスケールと中央配置の計算
    const scaleFactor = min(width / this.svgViewBox.width, height / this.svgViewBox.height);
    const offsetX = (width - this.svgViewBox.width * scaleFactor) * 0.5;
    const offsetY = (height - this.svgViewBox.height * scaleFactor) * 0.5;

    push();
    translate(offsetX, offsetY);
    scale(scaleFactor);
    translate(-this.svgViewBox.minX, -this.svgViewBox.minY);

    for (let i = 0; i < this.parts.length; i++) {
      const part = this.parts[i];
      push();

      // ★ ユーザーが渡したアニメーション関数（コールバック）をここで実行
      if (typeof animationCallback === "function") {
        animationCallback(part, i);
      }

      this.applyPartStyle(part.style);

      if (part.style.hasFill) {
        drawingContext.fill(part.path);
      }
      if (part.style.hasStroke) {
        drawingContext.stroke(part.path);
      }
      
      pop();
    }
    pop();
  }

  // --- 以下、ライブラリ内部の解析用メソッド群（ブラックボックス） ---

  collectPartsFromNode(node, inheritedStyle, outParts) {
    for (const child of node.children) {
      const tag = child.tagName.toLowerCase();
      const mergedStyle = this.mergeStyle(inheritedStyle, this.parseStyleFromElement(child));

      const d = this.elementToPathData(child, tag);
      if (d) {
        outParts.push({
          path: new Path2D(d),
          d: d,                              // ← これを追加
          style: this.normalizeStyle(mergedStyle)
        });
      }

      if (child.children.length > 0) {
        this.collectPartsFromNode(child, mergedStyle, outParts);
      }
    }
  }

  elementToPathData(el, tag) {
    if (tag === "path") return el.getAttribute("d") || "";

    if (tag === "rect") {
      const x = this.numAttr(el, "x", 0);
      const y = this.numAttr(el, "y", 0);
      const w = this.numAttr(el, "width", 0);
      const h = this.numAttr(el, "height", 0);
      let rx = this.numAttr(el, "rx", 0);
      let ry = this.numAttr(el, "ry", 0);

      if (w <= 0 || h <= 0) return "";

      if (rx > 0 && ry <= 0) ry = rx;
      if (ry > 0 && rx <= 0) rx = ry;
      rx = min(rx, w / 2);
      ry = min(ry, h / 2);

      if (rx <= 0 && ry <= 0) {
        return `M ${x} ${y} H ${x + w} V ${y + h} H ${x} Z`;
      }

      return [
        `M ${x + rx} ${y}`,
        `H ${x + w - rx}`,
        `A ${rx} ${ry} 0 0 1 ${x + w} ${y + ry}`,
        `V ${y + h - ry}`,
        `A ${rx} ${ry} 0 0 1 ${x + w - rx} ${y + h}`,
        `H ${x + rx}`,
        `A ${rx} ${ry} 0 0 1 ${x} ${y + h - ry}`,
        `V ${y + ry}`,
        `A ${rx} ${ry} 0 0 1 ${x + rx} ${y}`,
        "Z"
      ].join(" ");
    }

    if (tag === "circle") {
      const cx = this.numAttr(el, "cx", 0);
      const cy = this.numAttr(el, "cy", 0);
      const r = this.numAttr(el, "r", 0);
      if (r <= 0) return "";
      return [
        `M ${cx - r} ${cy}`,
        `A ${r} ${r} 0 1 0 ${cx + r} ${cy}`,
        `A ${r} ${r} 0 1 0 ${cx - r} ${cy}`,
        "Z"
      ].join(" ");
    }

    if (tag === "ellipse") {
      const cx = this.numAttr(el, "cx", 0);
      const cy = this.numAttr(el, "cy", 0);
      const rx = this.numAttr(el, "rx", 0);
      const ry = this.numAttr(el, "ry", 0);
      if (rx <= 0 || ry <= 0) return "";
      return [
        `M ${cx - rx} ${cy}`,
        `A ${rx} ${ry} 0 1 0 ${cx + rx} ${cy}`,
        `A ${rx} ${ry} 0 1 0 ${cx - rx} ${cy}`,
        "Z"
      ].join(" ");
    }

    if (tag === "line") {
      const x1 = this.numAttr(el, "x1", 0);
      const y1 = this.numAttr(el, "y1", 0);
      const x2 = this.numAttr(el, "x2", 0);
      const y2 = this.numAttr(el, "y2", 0);
      return `M ${x1} ${y1} L ${x2} ${y2}`;
    }

    if (tag === "polyline" || tag === "polygon") {
      const pointsRaw = (el.getAttribute("points") || "").trim();
      const nums = pointsRaw
        .replace(/,/g, " ")
        .split(/\s+/)
        .map(v => parseFloat(v))
        .filter(v => Number.isFinite(v));

      if (nums.length < 4) return "";

      const cmds = [`M ${nums[0]} ${nums[1]}`];
      for (let i = 2; i + 1 < nums.length; i += 2) {
        cmds.push(`L ${nums[i]} ${nums[i + 1]}`);
      }
      if (tag === "polygon") cmds.push("Z");
      return cmds.join(" ");
    }

    return "";
  }

  numAttr(el, name, fallback) {
    const raw = el.getAttribute(name);
    if (raw == null || raw === "") return fallback;
    const v = parseFloat(raw);
    return Number.isFinite(v) ? v : fallback;
  }

  parseStyleFromElement(el) {
    const style = {};
    const inline = el.getAttribute("style");
    if (inline) {
      const pairs = inline.split(";");
      for (const pair of pairs) {
        const idx = pair.indexOf(":");
        if (idx === -1) continue;
        const k = pair.slice(0, idx).trim();
        const v = pair.slice(idx + 1).trim();
        if (k) style[k] = v;
      }
    }

    const attrNames = [
      "fill", "fill-opacity", "stroke", "stroke-opacity", "stroke-width", "opacity"
    ];

    for (const key of attrNames) {
      const value = el.getAttribute(key);
      if (value != null && value !== "") {
        style[key] = value;
      }
    }
    return style;
  }

  mergeStyle(base, overrides) {
    return {
      fill: overrides.fill ?? base.fill,
      fillOpacity: this.parseMaybeNumber(overrides["fill-opacity"], base.fillOpacity),
      stroke: overrides.stroke ?? base.stroke,
      strokeOpacity: this.parseMaybeNumber(overrides["stroke-opacity"], base.strokeOpacity),
      strokeWidth: this.parseMaybeNumber(overrides["stroke-width"], base.strokeWidth),
      opacity: this.parseMaybeNumber(overrides.opacity, base.opacity)
    };
  }

  parseMaybeNumber(raw, fallback) {
    if (raw == null) return fallback;
    const v = parseFloat(raw);
    return Number.isFinite(v) ? v : fallback;
  }

  normalizeStyle(style) {
    const fillOn = style.fill !== "none" && style.fillOpacity > 0 && style.opacity > 0;
    const strokeOn = style.stroke !== "none" && style.strokeOpacity > 0 && style.opacity > 0 && style.strokeWidth > 0;

    return {
      hasFill: fillOn,
      hasStroke: strokeOn,
      fill: style.fill,
      stroke: style.stroke,
      fillAlpha: constrain(style.fillOpacity * style.opacity, 0, 1),
      strokeAlpha: constrain(style.strokeOpacity * style.opacity, 0, 1),
      strokeWidth: max(0, style.strokeWidth)
    };
  }

  applyPartStyle(style) {
    if (style.hasFill) {
      const c = color(style.fill);
      c.setAlpha(style.fillAlpha * 255);
      fill(c);
    } else {
      noFill();
    }

    if (style.hasStroke) {
      const c = color(style.stroke);
      c.setAlpha(style.strokeAlpha * 255);
      stroke(c);
      strokeWeight(style.strokeWidth);
    } else {
      noStroke();
    }
  }

  resolveViewBox(svgEl) {
    const vb = svgEl.getAttribute("viewBox");
    if (vb) {
      const vals = vb
        .trim()
        .split(/\s+|,/) 
        .map(v => parseFloat(v))
        .filter(v => Number.isFinite(v));
      if (vals.length === 4 && vals[2] > 0 && vals[3] > 0) {
        return { minX: vals[0], minY: vals[1], width: vals[2], height: vals[3] };
      }
    }
    const w = parseFloat(svgEl.getAttribute("width")) || 1;
    const h = parseFloat(svgEl.getAttribute("height")) || 1;
    return { minX: 0, minY: 0, width: max(1, w), height: max(1, h) };
  }
}