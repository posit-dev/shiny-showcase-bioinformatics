import json, math, os, random

OUT = os.path.dirname(os.path.abspath(__file__))
W, H = 1200, 800
MONO = "'Source Code Pro', 'Courier New', monospace"
SANS = "'Open Sans', 'Segoe UI', Arial, sans-serif"

FONTS = "https://fonts.googleapis.com/css2?family=Source+Code+Pro:wght@500;600&family=Open+Sans:wght@400;600;700&family=Space+Grotesk:wght@700&family=Fraunces:opsz,wght@9..144,600&family=IBM+Plex+Mono:wght@500;600&family=Inter:wght@500;600&display=swap"


def shell(bg, ink, muted, name, kicker, svg_body, extra_html=""):
    return f"""<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <script src="./support.js"></script>
</head>
<body>
<x-dc>
<helmet>
  <link rel="stylesheet" href="{FONTS}">
  <style>
    body {{ margin: 0; }}
    a {{ color: {ink}; }} a:hover {{ color: {muted}; }}
  </style>
</helmet>
<div style="position: relative; width: {W}px; height: {H}px; overflow: hidden; background: {bg}; font-family: {SANS}; color: {ink};">
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" style="position: absolute; left: 0; top: 0;">
{svg_body}
  </svg>
{extra_html}
  <div style="position: absolute; left: 64px; bottom: 56px; display: flex; flex-direction: column; gap: 10px;">
    <div style="font-family: {MONO}; font-size: 27px; font-weight: 600; letter-spacing: 0.08em; text-transform: uppercase; color: {ink};">{name}</div>
    <div style="font-family: {SANS}; font-size: 18px; color: {muted};">{kicker}</div>
  </div>
</div>
</x-dc>
</body>
</html>
"""


def t(x, y, s, size=16, fill="#000", fam=MONO, weight=500, anchor="start", extra=""):
    return (f'<text x="{x}" y="{y}" font-family="{fam}" font-size="{size}" font-weight="{weight}" '
            f'fill="{fill}" text-anchor="{anchor}" {extra}>{s}</text>')


# ---------------------------------------------------------------- 1 tahoe
def tahoe():
    teal, blue, green, sand, slate, ink, snow = "#0B7285", "#1C7ED6", "#2F9E44", "#E8A317", "#5F6B7A", "#212529", "#F8FAFB"
    p = []
    # depth contours (the lake)
    for i in range(6):
        y0 = 505 + i * 26
        d = f"M -20 {y0}"
        for x in range(0, 1240, 40):
            d += f" Q {x + 20} {y0 - 7 + (i % 2) * 14} {x + 40} {y0}"
        col = teal if i % 2 == 0 else blue
        p.append(f'<path d="{d}" fill="none" stroke="{col}" stroke-width="3" opacity="{0.85 - i * 0.13:.2f}"/>')
    # cell field with a lassoed subset
    for r in range(16):
        for c in range(22):
            x, y = 760 + c * 18, 200 + r * 18
            inside = 880 <= x <= 1060 and 290 <= y <= 420
            col, op = (green, 0.95) if inside else (slate, 0.32)
            p.append(f'<circle cx="{x}" cy="{y}" r="3.2" fill="{col}" opacity="{op}"/>')
    p.append(f'<rect x="868" y="278" width="204" height="154" rx="12" fill="none" stroke="{sand}" stroke-width="3" stroke-dasharray="10 7"/>')
    p.append(t(868, 462, "subset &#8594; recipe.R / recipe.py", 16, slate))
    p.append(t(64, 150, "ARC VIRTUAL CELL ATLAS &#183; SINGLE-CELL PERTURBATIONS", 20, slate, extra='letter-spacing="0.08em"'))
    html = (f'<div style="position: absolute; left: 58px; top: 168px; display: flex; flex-direction: column; gap: 0px; '
            f'font-family: \'Space Grotesk\', {SANS}; font-weight: 700; font-size: 176px; line-height: 0.92; letter-spacing: -0.03em; color: {teal};">'
            f'<div>Tahoe</div><div>100M</div></div>')
    return shell(snow, ink, slate, "tahoe-explorer",
                 "100.6M cells &#183; 50 cell lines &#183; 379 drugs &#183; never loaded, always queried",
                 "\n".join(p), html)


# ---------------------------------------------------------------- 2 genescout
MASCOT = """<svg x="70" y="150" width="360" height="360" viewBox="0 0 128 128">
  <ellipse cx="60" cy="120" rx="23" ry="4" fill="#5f1f2b" opacity="0.12"/>
  <ellipse cx="50" cy="116" rx="7" ry="4.2" fill="#5f1f2b"/>
  <ellipse cx="70" cy="116" rx="7" ry="4.2" fill="#5f1f2b"/>
  <g fill="none" stroke-linecap="round">
    <path d="M52 113 C70 105 44 93 60 84" stroke="#7c2b39" stroke-width="8"/>
    <path d="M68 113 C50 105 76 93 60 84" stroke="#9a6b2e" stroke-width="8"/>
  </g>
  <g stroke="#5f1f2b" stroke-width="3" stroke-linecap="round" opacity="0.5">
    <line x1="54" y1="107.5" x2="66" y2="107.5"/>
    <line x1="54" y1="92" x2="66" y2="92"/>
  </g>
  <circle cx="60" cy="52" r="26" fill="#faf5f0"/>
  <circle cx="60" cy="52" r="26" fill="none" stroke="#9a6b2e" stroke-width="7"/>
  <circle cx="51" cy="49" r="4" fill="#2c211d"/>
  <circle cx="67" cy="49" r="4" fill="#2c211d"/>
  <path d="M50 60 Q60 68 70 60" fill="none" stroke="#2c211d" stroke-width="3.4" stroke-linecap="round"/>
  <path d="M44 42 Q48 34 58 32" fill="none" stroke="#ffffff" stroke-width="3" stroke-linecap="round" opacity="0.6"/>
  <ellipse cx="60" cy="30" rx="29" ry="6.5" fill="#7c2b39"/>
  <path d="M48 30 v-7 a12 11 0 0 1 24 0 v7 z" fill="#8f3444"/>
  <rect x="48" y="25.5" width="24" height="5" rx="2.5" fill="#5f1f2b"/>
</svg>"""


def genescout():
    maroon, oxblood, ochre, sage, paper, ink = "#7c2b39", "#5f1f2b", "#9a6b2e", "#4d6b60", "#f7f1ec", "#2c211d"
    plexmono = "'IBM Plex Mono', 'Source Code Pro', monospace"
    p = [MASCOT]
    p.append(f'<rect x="520" y="160" width="616" height="440" rx="18" fill="#fffaf6" stroke="#e5d9cf" stroke-width="2"/>')
    p.append(t(556, 214, "Shortlist", 28, oxblood, fam="'Fraunces', Georgia, serif", weight=600))
    p.append(t(1100, 212, "ranked &#183; cited", 15, ochre, anchor="end"))
    rows = [("NF1", 360, "[12]"), ("SUZ12", 286, "[7]"), ("CDKN2A", 222, "[5]"), ("EED", 160, "[3]")]
    for i, (g, w, c) in enumerate(rows):
        y = 272 + i * 68
        p.append(t(556, y, str(i + 1), 18, ochre, fam=plexmono))
        p.append(t(590, y, g, 26, ink, fam=plexmono, weight=600))
        p.append(f'<rect x="700" y="{y - 16}" width="{w}" height="16" rx="8" fill="{maroon}" opacity="{0.95 - i * 0.16:.2f}"/>')
        p.append(t(700 + w + 14, y - 2, c, 16, ochre, fam=plexmono))
    y = 272 + 4 * 68
    p.append(t(556, y, "5", 18, sage, fam=plexmono, extra='opacity="0.7"'))
    p.append(t(590, y, "TTN", 26, sage, fam=plexmono, weight=600, extra='opacity="0.7"'))
    p.append(f'<line x1="590" y1="{y - 9}" x2="656" y2="{y - 9}" stroke="{sage}" stroke-width="3"/>')
    p.append(t(700, y - 2, "vetoed &#183; artifact gene &#183; no LLM in the veto", 15, sage, fam=plexmono))
    return shell(paper, ink, ochre, "genescout",
                 "candidate genes + a disease context &#8594; a plausibility-ranked, cited shortlist",
                 "\n".join(p))


# ---------------------------------------------------------------- 3 plotomics live
def plotomics_live():
    cream, ink, red, orange, teal, grey, sand = "#FAF6EE", "#2b211a", "#C02942", "#ED773C", "#6FA79A", "#9AA39E", "#D8CFBE"
    rnd = random.Random(7)
    p = []
    for _ in range(1100):
        x, y = rnd.uniform(430, 1160), rnd.uniform(80, 668)
        col = rnd.choice([teal, teal, grey, sand])
        p.append(f'<circle cx="{x:.0f}" cy="{y:.0f}" r="2.1" fill="{col}" opacity="0.65"/>')
    blobs = [(620, 220, 52), (905, 175, 40), (770, 420, 78), (1015, 475, 58), (560, 560, 44), (890, 640, 34)]
    for (cx, cy, s) in blobs:
        for _ in range(230):
            a, r = rnd.uniform(0, 6.283), abs(rnd.gauss(0, s))
            x, y = cx + math.cos(a) * r, cy + math.sin(a) * r * 0.85
            if 430 < x < 1160 and 80 < y < 668:
                col = red if rnd.random() < 0.55 else orange
                p.append(f'<circle cx="{x:.0f}" cy="{y:.0f}" r="2.5" fill="{col}" opacity="0.85"/>')
    p.append(f'<ellipse cx="770" cy="420" rx="128" ry="108" fill="none" stroke="{ink}" stroke-width="2.5" stroke-dasharray="9 7"/>')
    p.append(t(770, 296, "lasso &#183; 41,208 cells", 15, ink, anchor="middle"))
    p.append(t(64, 150, "XENIUM &#183; SPATIAL TRANSCRIPTOMICS", 20, "#8a4e1f", extra='letter-spacing="0.08em"'))
    html = (f'<div style="position: absolute; left: 62px; top: 175px; font-family: {SANS}; font-weight: 700; '
            f'font-size: 84px; line-height: 1; letter-spacing: -0.02em; color: {ink};">1,000,000</div>'
            f'<div style="position: absolute; left: 64px; top: 272px; font-family: {SANS}; font-size: 22px; color: #8a4e1f;">points on screen, pan and zoom</div>')
    return shell(cream, ink, "#8a4e1f", "Plotomics Live",
                 "26 pages of GPU-rendered figures &#183; one computation, WebGL or ggplot2",
                 "\n".join(p), html)


# ---------------------------------------------------------------- 4 variant reviewer
def variant_reviewer():
    green, clay, stone, charcoal, paper = "#4c7a5b", "#c07a52", "#7a7468", "#2c2a25", "#faf8f3"
    inter = f"'Inter', {SANS}"
    p = []
    p.append(t(64, 200, "BRAF", 110, green, fam=inter, weight=600, extra='letter-spacing="-0.02em"'))
    p.append(t(350, 200, "p.Val600Glu", 30, stone, weight=600))
    p.append(t(64, 246, "c.1799T&gt;A &#183; rs113488022 &#183; ClinVar: Pathogenic &#183; GRCh38", 19, stone))
    seq = list("GCTACAGTGAAATCT")
    x0, y0, w, h, gap = 64, 330, 64, 82, 9
    for i, b in enumerate(seq):
        x = x0 + i * (w + gap)
        mutant = (i == 7)
        fill = clay if mutant else "#efeae2"
        col = "#ffffff" if mutant else charcoal
        p.append(f'<rect x="{x}" y="{y0}" width="{w}" height="{h}" rx="10" fill="{fill}" stroke="{clay if mutant else "#e0d9cc"}" stroke-width="2"/>')
        p.append(t(x + w / 2, y0 + 55, "A" if mutant else b, 36, col, anchor="middle", weight=600))
        if mutant:
            p.append(t(x + w / 2, y0 - 14, "T", 20, stone, anchor="middle"))
            p.append(f'<line x1="{x + w / 2 - 9}" y1="{y0 - 20}" x2="{x + w / 2 + 9}" y2="{y0 - 20}" stroke="{stone}" stroke-width="2"/>')
    cx = x0 + 6 * (w + gap)
    cw = 3 * w + 2 * gap
    p.append(f'<path d="M {cx} {y0 + h + 14} v 10 h {cw} v -10" fill="none" stroke="{green}" stroke-width="2.5"/>')
    p.append(t(cx + cw / 2, y0 + h + 46, "codon 600 &#183; GTG &#8594; GAG", 16, green, anchor="middle"))
    p.append(t(64, 560, "18 evidence cards, fetched in parallel", 15, stone))
    for i in range(18):
        x = 64 + i * 62
        fill = green if i < 3 else ("#efeae2" if i < 14 else "#f6f2ea")
        op = 1 if i < 14 else 0.6
        p.append(f'<rect x="{x}" y="{574}" width="52" height="40" rx="7" fill="{fill}" stroke="#d8d1c3" stroke-width="1.5" opacity="{op}"/>')
    return shell(paper, charcoal, stone, "variant-reviewer",
                 "one gene, one variant, one page &#183; a 3D structure viewer and a ClinVar typeahead",
                 "\n".join(p))


# ---------------------------------------------------------------- 5 gene list builder
def gene_list_builder():
    canvas, card, border, tan, crimson, ink = "#EAE3DE", "#F7F2EE", "#D9CEC6", "#C29979", "#C33149", "#33241E"
    inter = f"'Inter', {SANS}"
    p = []
    p.append(t(64, 132, "breast cancer &#8594; MONDO:0007254", 20, ink))
    sources = ["ClinVar", "DGIdb", "DISEASES", "gnomAD constraint", "Open Targets", "PanelApp", "Pharos"]
    for i, s in enumerate(sources):
        y = 200 + i * 56
        p.append(t(64, y + 6, s, 19, ink))
        p.append(f'<circle cx="300" cy="{y}" r="6" fill="{tan}"/>')
        p.append(f'<path d="M 306 {y} C 470 {y}, 470 400, 640 400" fill="none" stroke="{tan}" stroke-width="2.5" opacity="0.85"/>')
    p.append(f'<circle cx="640" cy="400" r="8" fill="{crimson}"/>')
    p.append(f'<rect x="680" y="200" width="456" height="400" rx="16" fill="{card}" stroke="{border}" stroke-width="2"/>')
    p.append(t(712, 240, "ranked genes &#183; one row per gene", 15, "#8a7a70"))
    genes = [("BRCA1", 300, 5), ("BRCA2", 250, 5), ("PALB2", 200, 5), ("PTEN", 150, 4)]
    for i, (g, w, n) in enumerate(genes):
        y = 296 + i * 76
        p.append(t(712, y, str(i + 1), 18, tan))
        p.append(t(740, y, g, 26, ink, fam=inter, weight=600))
        p.append(f'<rect x="740" y="{y + 12}" width="{w}" height="12" rx="6" fill="{crimson}" opacity="{1 - i * 0.18:.2f}"/>')
        for k in range(n):
            p.append(f'<circle cx="{1055 + k * 16}" cy="{y - 8}" r="5" fill="{tan}"/>')
    return shell(canvas, ink, "#8a7a70", "gene-list-builder",
                 "one disease name &#183; 7 sources in parallel &#183; one deduplicated, re-rankable panel",
                 "\n".join(p))


# ---------------------------------------------------------------- 6 de explorer
def de_explorer():
    blue, teal, green, orange, grey, ink = "#447099", "#419599", "#72994E", "#EE6331", "#404041", "#404041"
    axis, dot = "#d9d9de", "#c9cbd0"
    rnd = random.Random(11)
    L, R, T, B = 170, 1120, 90, 640
    def px(v): return L + (v + 6) / 12 * (R - L)
    def py(v): return B - v / 8 * (B - T)
    p = []
    p.append(f'<line x1="{L}" y1="{B}" x2="{R}" y2="{B}" stroke="{axis}" stroke-width="2"/>')
    p.append(f'<line x1="{L}" y1="{T}" x2="{L}" y2="{B}" stroke="{axis}" stroke-width="2"/>')
    for v in (-1, 1):
        p.append(f'<line x1="{px(v):.0f}" y1="{T}" x2="{px(v):.0f}" y2="{B}" stroke="{axis}" stroke-width="1.5" stroke-dasharray="6 6"/>')
    p.append(f'<line x1="{L}" y1="{py(1.3):.0f}" x2="{R}" y2="{py(1.3):.0f}" stroke="{axis}" stroke-width="1.5" stroke-dasharray="6 6"/>')
    pts = []
    for _ in range(1500):
        x = rnd.gauss(0, 1.35)
        x = max(-5.8, min(5.8, x))
        y = abs(x) * rnd.uniform(0.15, 1.15) + abs(rnd.gauss(0, 0.5))
        y = min(7.6, y)
        pts.append((x, y))
    for x, y in pts:
        sig = abs(x) > 1 and y > 1.3
        col = (orange if x > 0 else blue) if sig else dot
        p.append(f'<circle cx="{px(x):.0f}" cy="{py(y):.0f}" r="{3.2 if sig else 2.4}" fill="{col}" opacity="{0.9 if sig else 0.75}"/>')
    labels = [("NAPSA", 4.6, 7.1, orange), ("SFTPC", 5.2, 6.2, orange), ("KRT5", -4.9, 7.3, blue), ("TP63", -4.2, 6.3, blue)]
    for g, x, y, col in labels:
        p.append(f'<circle cx="{px(x):.0f}" cy="{py(y):.0f}" r="5" fill="{col}"/>')
        p.append(t(px(x) + (12 if x > 0 else -12), py(y) + 5, g, 16, ink, anchor=("start" if x > 0 else "end"), weight=600))
    p.append(t((L + R) / 2, B + 34, "log2 fold change", 15, grey, anchor="middle"))
    p.append(t(L - 24, (T + B) / 2, "&#8722;log10 adj. p", 15, grey, anchor="middle", extra=f'transform="rotate(-90 {L - 24} {(T + B) / 2})"'))
    p.append(f'<circle cx="{R - 250}" cy="{T - 44}" r="6" fill="{orange}"/>' + t(R - 238, T - 39, "up in LUAD", 15, grey))
    p.append(f'<circle cx="{R - 120}" cy="{T - 44}" r="6" fill="{blue}"/>' + t(R - 108, T - 39, "up in LUSC", 15, grey))
    return shell("#FFFFFF", ink, grey, "DE Explorer",
                 "differential expression over TCGA lung RNA-seq &#183; volcano, PCA, per-gene detail",
                 "\n".join(p))


# ---------------------------------------------------------------- 7 signature scoring
def signature_scoring():
    bg, ink, rust, green, dgreen, muted = "#F2EFE6", "#111111", "#C4562F", "#72994E", "#3F5A2A", "#6b6b66"
    rnd = random.Random(3)
    n = 50
    vals = sorted(math.tanh((i - 24.5) / 9) + rnd.uniform(-0.08, 0.08) for i in range(n))
    L, R, y0, scale = 90, 1130, 410, 175
    bw = (R - L) / n
    p = []
    p.append(f'<line x1="{L}" y1="{y0}" x2="{R}" y2="{y0}" stroke="#bdb8ad" stroke-width="1.5"/>')
    for i, v in enumerate(vals):
        x = L + i * bw
        h = abs(v) * scale
        if v >= 0:
            col = rust
            y = y0 - h
        else:
            col = green if abs(v) < 0.6 else dgreen
            y = y0
        p.append(f'<rect x="{x + 1.5:.1f}" y="{y:.1f}" width="{bw - 3:.1f}" height="{h:.1f}" rx="2" fill="{col}"/>')
    p.append(t(L, 130, "HR+", 30, dgreen, fam=SANS, weight=700))
    p.append(t(L, 158, "estrogen response &#183; early and late", 15, muted))
    p.append(t(R, 130, "HER2+", 30, rust, fam=SANS, weight=700, anchor="end"))
    p.append(t(R, 158, "E2F targets &#183; MYC targets &#183; G2M checkpoint", 15, muted, anchor="end"))
    p.append(t(R, 640, "50 Hallmark gene sets, one score per sample, sorted by the contrast", 15, muted, anchor="end"))
    return shell(bg, ink, muted, "Signature Scoring",
                 "TCGA-BRCA &#183; ssGSEA over 50 Hallmark pathways &#183; contrast clinical subtypes",
                 "\n".join(p))


# ---------------------------------------------------------------- 8 drug perturbation
def drug_perturbation():
    bg, ink, teal, orange, blue, muted = "#EFF4F4", "#2b3435", "#419599", "#EE6331", "#447099", "#5f6b6b"
    rnd = random.Random(5)
    q = [rnd.uniform(-1, 1) for _ in range(16)]
    q = [v if abs(v) > 0.25 else v * 3 for v in q]
    def jitter(v, k): return max(-1, min(1, v * k + rnd.uniform(-0.12, 0.12)))
    sigs = [("query signature", q, ink, None),
            ("mimic", [jitter(v, 0.9) for v in q], teal, "+0.88"),
            ("reverser", [jitter(-v, 0.9) for v in q], orange, "&#8722;0.85")]
    p = []
    for j, (label, vals, col, score) in enumerate(sigs):
        cx = 250 + j * 380
        top = 170
        p.append(f'<line x1="{cx}" y1="{top - 10}" x2="{cx}" y2="{top + 16 * 26}" stroke="{ink}" stroke-width="2" opacity="0.5"/>')
        for i, v in enumerate(vals):
            y = top + i * 26
            w = abs(v) * 125
            fill = teal if v > 0 else orange
            x = cx if v > 0 else cx - w
            p.append(f'<rect x="{x:.1f}" y="{y}" width="{w:.1f}" height="16" rx="3" fill="{fill}" opacity="0.92"/>')
        p.append(t(cx, 130, label, 20, col if score else ink, anchor="middle", weight=600))
        if score:
            p.append(t(cx, 640, f"connectivity {score}", 18, col, anchor="middle", weight=600))
    p.append(f'<circle cx="612" cy="70" r="5" fill="{teal}"/>' + t(624, 75, "up", 15, muted))
    p.append(f'<circle cx="672" cy="70" r="5" fill="{orange}"/>' + t(684, 75, "down", 15, muted))
    return shell(bg, ink, muted, "Drug Perturbation",
                 "connectivity scoring against a reference panel &#183; rank compounds into mimics and reversers",
                 "\n".join(p))


# ---------------------------------------------------------------- 9 genome explorer
def genome_explorer():
    bg, ink, indigo, indigo2, orange, muted, band1, band2 = "#EEEEF5", "#2c2c3a", "#383D94", "#5A5FC7", "#EE6331", "#5f6178", "#c9cbdf", "#8e90b0"
    rnd = random.Random(9)
    p = []
    L, R, y, h = 64, 1136, 150, 34
    p.append(f'<rect x="{L}" y="{y}" width="{R - L}" height="{h}" rx="17" fill="#f7f7fb" stroke="#b8bad0" stroke-width="2"/>')
    x = L + 12
    k = 0
    while x < R - 40:
        w = rnd.choice([18, 26, 34, 46, 60])
        if abs(x - 600) < 30:
            x += w
            continue
        col = band1 if k % 2 == 0 else band2
        p.append(f'<rect x="{x}" y="{y + 4}" width="{min(w, R - 14 - x)}" height="{h - 8}" rx="3" fill="{col}" opacity="0.8"/>')
        x += w + 4
        k += 1
    p.append(f'<path d="M 588 {y + 2} L 612 {y + h / 2} L 588 {y + h - 2} Z M 616 {y + 2} L 592 {y + h / 2} L 616 {y + h - 2} Z" fill="#a9abc4"/>')
    mx = 1010
    p.append(f'<rect x="{mx - 5}" y="{y - 6}" width="10" height="{h + 12}" rx="3" fill="{orange}"/>')
    p.append(t(mx, y - 16, "3q26.32", 15, orange, anchor="middle", weight=600))
    p.append(t(L, y - 16, "chr3", 15, muted))
    p.append(f'<line x1="{mx - 5}" y1="{y + h}" x2="{L}" y2="300" stroke="#b8bad0" stroke-width="1.5" stroke-dasharray="5 5"/>')
    p.append(f'<line x1="{mx + 5}" y1="{y + h}" x2="{R}" y2="300" stroke="#b8bad0" stroke-width="1.5" stroke-dasharray="5 5"/>')
    ty = 470
    p.append(f'<line x1="{L}" y1="{ty}" x2="{R}" y2="{ty}" stroke="{indigo2}" stroke-width="2"/>')
    for i in range(0, R - L - 20, 44):
        x = L + i
        p.append(f'<path d="M {x + 30} {ty - 4} l 6 4 l -6 4" fill="none" stroke="{indigo2}" stroke-width="1.5"/>')
    exons = [80, 150, 260, 330, 360, 400, 440, 520, 560, 600, 640, 700, 740, 800, 840, 870, 900, 940, 1000, 1060, 1100]
    for ex in exons:
        p.append(f'<rect x="{ex - 8}" y="{ty - 11}" width="16" height="22" rx="2" fill="{indigo}"/>')
    p.append(t(L, ty - 30, "PIK3CA", 18, indigo, weight=600))
    p.append(t(R, ty + 44, "chr3:178,864,901-178,958,886 &#183; hg19", 15, muted, anchor="end"))
    lolli = [("H1047R", 1100, 150, orange, 13, "129", "end"), ("E545K", 882, 104, indigo2, 10, "43", "start"), ("E542K", 866, 68, indigo2, 9, "31", "end"), ("N345K", 400, 46, indigo2, 7, "12", "start")]
    for lab, x, hgt, col, r, n, side in lolli:
        cy = ty - 11 - hgt
        p.append(f'<line x1="{x}" y1="{ty - 11}" x2="{x}" y2="{cy}" stroke="{col}" stroke-width="2.5"/>')
        p.append(f'<circle cx="{x}" cy="{cy}" r="{r}" fill="{col}"/>')
        if side == "end":
            p.append(t(x - r - 10, cy + 5, f"{lab} &#183; {n}", 15, ink, anchor="end"))
        else:
            p.append(t(x + r + 8, cy + 5, f"{lab} &#183; {n}", 15, ink))
    return shell(bg, ink, muted, "Genome Explorer",
                 "recurrent TCGA-BRCA driver mutations on hg19 &#183; igv.js &#183; click a variant, the browser zooms",
                 "\n".join(p))


# ---------------------------------------------------------------- 10 recount explorer
def recount_explorer():
    cream, espresso, coffee, caramel, latte, crema, foam, sage, amber, brick = "#FBF7F2", "#2B1D14", "#6F4A32", "#8A5A3B", "#C4A484", "#D9C4A9", "#E7DACA", "#5C7B4E", "#B5822E", "#9E3B32"
    rnd = random.Random(21)
    p = []
    cols, rows = 14, 8
    for r in range(rows):
        for c in range(cols):
            x, y = 64 + c * 46, 110 + r * 46
            fill = rnd.choice([foam, foam, crema, latte])
            if (c, r) == (6, 3):
                fill = brick
            p.append(f'<rect x="{x}" y="{y}" width="36" height="36" rx="7" fill="{fill}"/>')
    sx, sy = 64 + 6 * 46 + 36, 110 + 3 * 46 + 18
    p.append(f'<path d="M {sx} {sy} C {sx + 120} {sy}, 700 300, 760 300" fill="none" stroke="{caramel}" stroke-width="2" stroke-dasharray="6 5"/>')
    cx0, cy0, cw, ch = 760, 110, 376, 330
    p.append(f'<rect x="{cx0}" y="{cy0}" width="{cw}" height="{ch}" rx="14" fill="#ffffff" stroke="{coffee}" stroke-width="2"/>')
    p.append(t(cx0 + 24, cy0 + 36, "SRP009840 &#183; human &#183; 20 samples", 15, espresso, weight=600))
    p.append(t(cx0 + 24, cy0 + 58, "NT5C2-mutant clones in relapsed ALL", 13, caramel))
    px0, py0, pw, ph = cx0 + 40, cy0 + 90, cw - 70, ch - 130
    p.append(f'<line x1="{px0}" y1="{py0 + ph}" x2="{px0 + pw}" y2="{py0 + ph}" stroke="#e0d3c4" stroke-width="1.5"/>')
    p.append(f'<line x1="{px0}" y1="{py0}" x2="{px0}" y2="{py0 + ph}" stroke="#e0d3c4" stroke-width="1.5"/>')
    groups = [(0.3, 0.35, sage), (0.7, 0.3, amber), (0.55, 0.75, brick)]
    for gx, gy, col in groups:
        for _ in range(22):
            x = px0 + (gx + rnd.gauss(0, 0.08)) * pw
            y = py0 + (gy + rnd.gauss(0, 0.09)) * ph
            p.append(f'<circle cx="{x:.0f}" cy="{y:.0f}" r="4.5" fill="{col}" opacity="0.9"/>')
    p.append(t(px0 + pw, py0 + ph + 18, "PC1", 12, caramel, anchor="end"))
    p.append(t(px0 - 8, py0 + 10, "PC2", 12, caramel, anchor="end"))
    html = (f'<div style="position: absolute; left: 62px; top: 500px; font-family: {SANS}; font-weight: 700; '
            f'font-size: 88px; line-height: 1; letter-spacing: -0.02em; color: {espresso};">18,998</div>'
            f'<div style="position: absolute; left: 64px; top: 598px; font-family: {SANS}; font-size: 22px; color: {caramel};">uniformly processed RNA-seq studies, one catalog</div>')
    return shell(cream, espresso, caramel, "recount-explorer",
                 "recount3 &#183; human and mouse &#183; browse, QC, PCA, export, no code",
                 "\n".join(p), html)


ARTBOARDS = [
    ("Main.dc.html", "tahoe-explorer", tahoe),
    ("GeneScout.dc.html", "genescout", genescout),
    ("PlotomicsLive.dc.html", "Plotomics Live", plotomics_live),
    ("VariantReviewer.dc.html", "variant-reviewer", variant_reviewer),
    ("GeneListBuilder.dc.html", "gene-list-builder", gene_list_builder),
    ("DEExplorer.dc.html", "DE Explorer", de_explorer),
    ("SignatureScoring.dc.html", "Signature Scoring", signature_scoring),
    ("DrugPerturbation.dc.html", "Drug Perturbation", drug_perturbation),
    ("GenomeExplorer.dc.html", "Genome Explorer", genome_explorer),
    ("RecountExplorer.dc.html", "recount-explorer", recount_explorer),
]

canvas = {"artboards": [], "launch": {"view": "canvas"}}
for i, (fname, title, fn) in enumerate(ARTBOARDS):
    with open(os.path.join(OUT, fname), "w", encoding="utf-8") as f:
        f.write(fn())
    row, col = divmod(i, 5)
    canvas["artboards"].append({"file": fname, "title": title, "x": col * (W + 100), "y": row * (H + 160), "w": W, "h": H})
with open(os.path.join(OUT, "canvas.json"), "w", encoding="utf-8") as f:
    json.dump(canvas, f, indent=2)
print("wrote", len(ARTBOARDS), "artboards")
